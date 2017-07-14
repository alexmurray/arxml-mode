;;; arxml-mode --- Major mode for editing arxml files -*- lexical-binding: t -*-

;;; Commentary:

;;
;; TODO: Improve imenu integration to separate out into element types
;;
;; TODO: Add more yasnippets
;;

;;; Code:
(require 'sgml-mode)
(require 'nxml-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'thingatpt)
(require 'speedbar)
(require 'eldoc)

(require 'yasnippet nil t)
(require 'flycheck nil t)
(require 'smartparens nil t)
(require 'company nil t)
(require 'evil-matchit nil t)
(require 'all-the-icons nil t)

(defconst arxml-mode-base-path
  (file-name-directory
   (file-truename
    (expand-file-name
     (or load-file-name buffer-file-name)))))

;; path to the AUTOSAR_00042.xsd shipped with arxml-mode.el
(defvar arxml-mode-xsd-path
  (expand-file-name "AUTOSAR_00042.xsd" arxml-mode-base-path))

;; yasnippet integration
(when (boundp 'yas-snippet-dirs)
  (push (expand-file-name "snippets" arxml-mode-base-path)
        yas-snippet-dirs))

;; flycheck integration
(when (fboundp 'flycheck-add-mode)
  (flycheck-add-mode 'xml-xmllint 'arxml-mode)
  (flycheck-add-mode 'xml-xmlstarlet 'arxml-mode))

;; smartparens integration
(when (boundp 'sp-navigate-consider-sgml-tags)
  (add-to-list 'sp-navigate-consider-sgml-tags 'arxml-mode))

;; evil-matchit integration
(when (boundp 'evilmi-plugins)
  (plist-put evilmi-plugins 'arxml-mode '((evilmi-template-get-tag evilmi-template-jump)
                                          (evilmi-simple-get-tag evilmi-simple-jump)
                                          (evilmi-html-get-tag evilmi-html-jump))))
;; all the icons integration
(when (boundp 'all-the-icons-icon-alist)
  (push '("\.arxml$" all-the-icons-faicon "file-code-o" :height 0.95 :face all-the-icons-lorange)
        all-the-icons-icon-alist))
(when (boundp 'all-the-icons-mode-icon-alist)
  (push '(arxml-mode all-the-icons-faicon "file-code-o" :height 0.95 :face all-the-icons-lorange)
        all-the-icons-mode-icon-alist))

;; xref integration
;; table to store tags data
(defvar arxml-mode-tags-table (make-hash-table :test 'equal))

;; list to store tag names for xref-apropos and completion-at-point
(defvar arxml-mode-tags-list nil)

;; directory where all arxml files reside
(defvar arxml-mode-directory nil)

;; a tag - has type, name, definitions and references
(cl-defstruct arxml-mode-tag type name def ref)

;; a tag location (ie. a definition or reference) - has file, line number and
;; column number
(cl-defstruct arxml-mode-tag-location file line col)

(defun arxml-mode-lookup-tag (identifier &optional no-error)
  "Lookup tag for IDENTIFIER and signal an error unless NO-ERROR."
  (let ((tag (gethash identifier arxml-mode-tags-table)))
    (when (and (null tag) (null no-error))
      (error "%s not found in tags table" identifier))
    tag))

(defun arxml-mode-ensure-tags ()
  "Ensure the tags have been parsed and consistent."
  (unless (string-equal arxml-mode-directory default-directory)
    (arxml-mode-reset-tags))
  (if arxml-mode-tags-list
      ;; raise error if not found in tags
      (dolist (identifier arxml-mode-tags-list)
        (assert (arxml-mode-lookup-tag identifier t) t))
    ;; ensure to create tags hash table
    (dolist (f (directory-files default-directory nil ".*\\.arxml\\'"))
      (with-current-buffer (find-file-noselect f)
        (arxml-mode-parse-buffer)))))

(defun arxml-mode-reset-tags ()
  "Reset tag information."
  (interactive)
  (setq arxml-mode-tags-list nil)
  (setq arxml-mode-directory nil)
  (clrhash arxml-mode-tags-table))

(defun arxml-mode-find-tag-locations (type name)
  "Return a list of arxml-mode-tag-location of the TYPE of NAME from the tags table."
  (arxml-mode-ensure-tags)
  (let ((tag (arxml-mode-lookup-tag name)))
    (pcase type
      ('def (arxml-mode-tag-def tag))
      ('ref (arxml-mode-tag-ref tag))
      (_ (error "Unknown type %S" type)))))

;; only active when in arxml-mode
(defun arxml-mode-xref-backend ()
  "Xref backend for arxml."
  (when (eq major-mode 'arxml-mode)
    'arxml))

(defun arxml-mode-get-xrefs (type name)
  "Return a list of xref locations of TYPE for NAME."
  (remove nil
          (mapcar #'(lambda (tag)
                      (if tag
                          (xref-make name
                                     (xref-make-file-location
                                      (arxml-mode-tag-location-file tag)
                                      (arxml-mode-tag-location-line tag)
                                      (arxml-mode-tag-location-col tag)))))
                  (arxml-mode-find-tag-locations type name))))

(defun arxml-mode-identifier-at-point ()
  "Get a alist containing the arxml identifier at point."
  ;; ensure in a text section
  (when (eq 'text (car (sgml-lexical-context)))
    (let* ((current (thing-at-point 'symbol t))
           (start (car (bounds-of-thing-at-point 'symbol)))
           (matched (when current
                      (string-match "\\(.*?\\)>\\([[:alnum:]/_]+\\)" current))))
      ;; go up and back to get current tag name
      (when matched
        (let ((identifier (match-string 2 current))
              (begin (+ start (match-beginning 2)))
              (end (+ start (match-end 2))))
          (save-excursion
            (nxml-backward-up-element)
            (let ((tag-name (xmltok-start-tag-local-name))
                  (attr (nxml-find-following-attribute))
                  (dest nil))
              ;; if tag-name is a short-name we need to build the full name
              (when (string-equal tag-name "SHORT-NAME")
                (let ((file (expand-file-name (buffer-file-name)))
                      (line (line-number-at-pos)))
                  (dolist (name arxml-mode-tags-list)
                    (let ((tag (arxml-mode-lookup-tag name)))
                      ;; check ends with identifier
                      (when (and (>= (length name)
                                     (length identifier))
                                 (string-equal identifier
                                               (substring name (- (length identifier)) nil)))
                        (dolist (def (arxml-mode-tag-def tag))
                          (when (and  (string-equal file (arxml-mode-tag-location-file def))
                                      (= line (arxml-mode-tag-location-line def)))
                            (setq identifier name))))))))
              (when attr
                (when (string-equal (xmltok-attribute-local-name attr) "DEST")
                  (setq dest (xmltok-attribute-value attr))))
              `((tag-name . ,tag-name)
                (dest .,dest)
                (identifier . ,identifier)
                (begin . ,begin)
                (end . ,end)))))))))


(defvar arxml-mode-parse-stack nil)

(defun arxml-mode-parse-tag (text start-tag)
  "Parse TEXT and START-TAG."
  (if start-tag
      (push `((tag . ,(cdar start-tag))
              (attrs . ,(cadr start-tag))
              (ref . ,nil)
              (short-name . ,nil)
              (pos . ,(point)))
            arxml-mode-parse-stack)
    ;; else treat as data and end of tag
    (let ((current (car arxml-mode-parse-stack))
          (parent (cadr arxml-mode-parse-stack))
          (type nil)
          (identifier nil)
          (element nil))
      (when (string-equal (alist-get 'tag current) "SHORT-NAME")
        ;; assign this text to the short-name property of parent (cadr)
        (setf (alist-get 'short-name parent) text)
        ;; short-name is at current location
        (setf (alist-get 'pos parent)
              (alist-get 'pos current)))
      ;; if ends with REF is a reference then add as ref
      (when (string-match "REF\\'" (alist-get 'tag current))
        (setf (alist-get 'ref current) text))
      ;; now process end of tag - if the element on top of stack has a short-name
      ;; then this is a definition
      (when (alist-get 'short-name current)
        (setq type 'def)
        (setq identifier
              (concat "/" (mapconcat #'(lambda (tag) (alist-get 'short-name tag))
                                     (reverse
                                      (cl-remove-if-not
                                       #'(lambda (tag) (alist-get 'short-name tag))
                                       arxml-mode-parse-stack))
                                     "/")))
        (setq element (alist-get 'tag current)))
      ;; if is a reference and has a DEST this is a reference
      (when (and (alist-get 'ref current)
                 (cdr (assoc "DEST" (alist-get 'attrs current))))
        (setq type 'ref)
        (setq identifier (alist-get 'ref current))
        (setq element (cdr (assoc "DEST" (alist-get 'attrs current)))))
      (when (and type identifier element)
        ;; see if already in table and update
        (let ((tag (arxml-mode-lookup-tag identifier t))
              (line nil)
              (col nil))
          (save-excursion
            (goto-char (alist-get 'pos current))
            (setq line (line-number-at-pos))
            ;; ensure tabs only count as 1 character otherwise xref gets
            ;; confused
            (setq col (let ((tab-width 1))
                        (current-column))))
          (unless tag
            (setq tag (make-arxml-mode-tag :type element :name identifier :def nil :ref nil))
            (cl-pushnew identifier arxml-mode-tags-list :test #'equal)
            (puthash identifier tag arxml-mode-tags-table))
          (let ((location (make-arxml-mode-tag-location :file (buffer-file-name) :line line :col col)))
            (pcase type
              ('def (push location (arxml-mode-tag-def tag)))
              ('ref (push location (arxml-mode-tag-ref tag)))
              (_ (error "Unknown type %S" type))))))
      (pop arxml-mode-parse-stack)))
  ;; ensure to return nil since this is technically a validation function so
  ;; return nil to signal all is valid :)
  nil)

(defun arxml-mode-tag-location-is-current-buffer-p (location)
  "Return whether LOCATION refers to the current buffer."
  (string-equal (arxml-mode-tag-location-file location)
                (buffer-file-name)))

(defun arxml-mode-parse-buffer ()
  "Parse current buffer."
  (setq arxml-mode-parse-stack nil)
  ;; clear any existing entries which refer to this buffer
  (let ((orphans nil))
    (dolist (identifier arxml-mode-tags-list)
      (let ((tag (arxml-mode-lookup-tag identifier t)))
        ;; is an orphan already if not in tags table
        (if (null tag)
            (push identifier orphans)
          (setf (arxml-mode-tag-def tag)
                (cl-delete-if #'arxml-mode-tag-location-is-current-buffer-p
                              (arxml-mode-tag-def tag)))
          (setf (arxml-mode-tag-ref tag)
                (cl-delete-if #'arxml-mode-tag-location-is-current-buffer-p
                              (arxml-mode-tag-ref tag)))
          ;; if there are no definitions or references left then remove from tags
          ;; table
          (when (= 0 (+ (length (arxml-mode-tag-def tag))
                        (length (arxml-mode-tag-ref tag))))
            (remhash identifier arxml-mode-tags-table)
            (push identifier orphans)))))
    ;; remove any orphans from the list
    (dolist (orphan orphans)
      (setq arxml-mode-tags-list
            (cl-delete orphan arxml-mode-tags-list :test #'equal))))
  ;; use nxml to parse and hook in via the tag validation function
  (let ((nxml-parse-file-name (buffer-file-name))
        (nxml-validate-function #'arxml-mode-parse-tag))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (condition-case err
            (nxml-parse-instance)
          (nxml-file-parse-error nil))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql arxml)))
  (alist-get 'identifier (arxml-mode-identifier-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql arxml)) identifier)
  (arxml-mode-get-xrefs 'def identifier))

(cl-defmethod xref-backend-references ((_backend (eql arxml)) identifier)
  (arxml-mode-get-xrefs 'ref identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql arxml)) identifier)
  (arxml-mode-ensure-tags)
  (let ((tags))
    (dolist (name arxml-mode-tags-list)
      (let ((tag (arxml-mode-lookup-tag name)))
        (when (string-match identifier name)
          (dolist (location (arxml-mode-tag-def tag))
            (push (xref-make name
                             (xref-make-file-location
                              (arxml-mode-tag-location-file location)
                              (arxml-mode-tag-location-line location)
                              (arxml-mode-tag-location-col location)))
                  tags)))))
    tags))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql arxml)))
  (arxml-mode-ensure-tags)
  arxml-mode-tags-list)

(defun arxml-mode-tag-location-to-string (location)
  "Get string representing LOCATION."
  (format "%s: %d: %d"
          (file-relative-name (arxml-mode-tag-location-file location))
          (arxml-mode-tag-location-line location)
          (arxml-mode-tag-location-col location)))

;; completion at point - only for ref tags
(defun arxml-mode-completion-at-point ()
  "`completion-at-point' function for arxml-mode."
  (arxml-mode-ensure-tags)
  (let ((identifier (arxml-mode-identifier-at-point)))
    (when (and identifier
               ;; when tag has REF suffix - this is a reference so complete
               ;; based on tags
               (string-match "REF\\'" (alist-get 'tag-name identifier)))
      (list (alist-get 'begin identifier)
            (alist-get 'end identifier)
            (cl-remove-if-not #'(lambda (name)
                                  (let ((tag (arxml-mode-lookup-tag name)))
                                    (string-equal (alist-get 'dest identifier)
                                                  (arxml-mode-tag-type tag))))
                              arxml-mode-tags-list)
            :exclusive 'no
            :company-docsig #'identity
            :company-doc-buffer #'(lambda (identifier)
                                    (let ((defs (arxml-mode-find-tag-locations 'def identifier))
                                          (refs (arxml-mode-find-tag-locations 'ref identifier)))
                                      (company-doc-buffer
                                       (format "%s\n\nDefined at:\n%s\n\nReferenced at:\n%s"
                                               identifier
                                               (mapconcat
                                                #'arxml-mode-tag-location-to-string
                                                defs "\n")
                                               (mapconcat
                                                #'arxml-mode-tag-location-to-string
                                                refs "\n")))))
            :company-location #'(lambda (identifier)
                                  (let ((def (car (arxml-mode-find-tag-locations 'def identifier))))
                                    (cons (arxml-mode-tag-location-file def)
                                          (arxml-mode-tag-location-line def))))))))

;; imenu
(defun arxml-mode-imenu-create-index ()
  "Create imenu index for current buffer."
  (arxml-mode-ensure-tags)
  (let ((index nil))
    (dolist (identifier arxml-mode-tags-list)
      (let ((tag (arxml-mode-lookup-tag identifier)))
        (dolist (def (arxml-mode-tag-def tag))
          (when (string-equal (expand-file-name (buffer-file-name))
                              (arxml-mode-tag-location-file def))
            (push (cons (arxml-mode-tag-name tag)
                        (save-restriction
                          (widen)
                          (goto-char (point-min))
                          (forward-line (1- (arxml-mode-tag-location-line def)))
                          (move-to-column (arxml-mode-tag-location-col def))
                          (point))) index)))))
    (list (push "Name" index))))

(defun arxml-mode-eldoc-function ()
  "Support for eldoc mode."
  (arxml-mode-ensure-tags)
  (let ((tag (arxml-mode-lookup-tag (alist-get 'identifier (arxml-mode-identifier-at-point)) t)))
    (when tag
      (format "%s -> %s"
              (arxml-mode-tag-name tag)
              (propertize (arxml-mode-tag-type tag)
                          'face 'bold)))))

;; define our major-mode
(define-derived-mode arxml-mode nxml-mode "arxml"
  "Major mode for editing arxml files."
  (add-to-list 'after-change-functions #'(lambda (beg end len) (arxml-mode-parse-buffer)))
  (add-to-list 'xref-backend-functions #'arxml-mode-xref-backend)
  (add-to-list 'completion-at-point-functions #'arxml-mode-completion-at-point)
  (setq imenu-create-index-function #'arxml-mode-imenu-create-index)
  (imenu-add-to-menubar "ARXML")
  (speedbar-add-supported-extension '("\\.arxml"))
  (add-function :before-until (local 'eldoc-documentation-function)
                #'arxml-mode-eldoc-function)
  ;; integrate with flycheck for validation
  (when (boundp 'flycheck-xml-xmlstarlet-xsd-path)
    (setq flycheck-xml-xmlstarlet-xsd-path arxml-mode-xsd-path))
  (when (boundp 'flycheck-xml-xmllint-xsd-path)
    (setq flycheck-xml-xmllint-xsd-path flycheck-xml-xmlstarlet-xsd-path))
  ;; limit company to only the useful backends
  (when (boundp 'company-backends)
    (setq company-backends '((company-nxml company-capf)))))

;; use local schemas.xml to find our local AUTOSAR_00042.rnc
(add-to-list 'rng-schema-locating-files (expand-file-name "schemas.xml" arxml-mode-base-path))
(add-to-list 'auto-mode-alist '("\\.arxml\\'" . arxml-mode))

(provide 'arxml-mode)

;;; arxml-mode.el ends here
