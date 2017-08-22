;;; arxml-mode.el --- Major mode for editing arxml files -*- lexical-binding: t -*-

;; Copyright (c) 2017 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/arxml-mode
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages provides an enhanced editing environemtn for AUTOSAR arxml
;; files.

;;;; Setup

;; (require 'arxml-mode)

;;; Code:
(require 'sgml-mode)
(require 'nxml-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'thingatpt)
(require 'speedbar)
(require 'eldoc)
(require 'flyspell)

;;; Forward declaration of optional dependencies
(declare-function company-doc-buffer "ext:company.el")
(declare-function flycheck-error-new-at "ext:flycheck.el")
(declare-function flycheck-define-generic-checker "ext:flycheck.el")
(declare-function flycheck-add-next-checker "ext:flycheck.el")
(declare-function flycheck-add-mode "ext:flycheck.el")

(defvar flycheck-xml-xmlstarlet-xsd-path)
(defvar flycheck-xml-xmllint-xsd-path)
(defvar flycheck-checkers)
(defvar company-backends)
(defvar yas-snippet-dirs)
(defvar sp-navigate-consider-sgml-tags)
(defvar evilmi-plugins)
(defvar all-the-icons-icon-alist)
(defvar all-the-icons-mode-icon-alist)

(defgroup arxml-mode nil
  "Major mode for editing ARXML files and projects."
  :group 'languages)

(defcustom arxml-mode-reparse-on-buffer-change nil
  "When t reparse the buffer after every change, otherwise only reparse when buffer is saved."
  :group 'arxml-mode
  :tag "Reparse on buffer change"
  :type 'boolean)

(defconst arxml-mode-base-path
  (file-name-directory
   (file-truename
    (expand-file-name
     (or load-file-name buffer-file-name)))))

;; path to the AUTOSAR_00042.xsd shipped with arxml-mode.el
(defconst arxml-mode-xsd-path
  (expand-file-name "AUTOSAR_00042.xsd" arxml-mode-base-path))

;; xref integration
;; table to store tags data
(defvar arxml-mode-tags-table (make-hash-table :test 'equal))

;; list to store tag names for xref-apropos and completion-at-point
(defvar arxml-mode-tags-list nil)

;; directory where all arxml files reside
(defvar arxml-mode-directory nil)

;; a tag - has type, name, description definitions and references
(cl-defstruct arxml-mode-tag type name desc defs refs)

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
    (arxml-mode-reset-tags)
    (setq arxml-mode-directory default-directory))
  (if arxml-mode-tags-list
      ;; raise error if not found in tags
      (dolist (identifier arxml-mode-tags-list)
        (arxml-mode-lookup-tag identifier))
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
      ('defs (arxml-mode-tag-defs tag))
      ('refs (arxml-mode-tag-refs tag))
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
            (let ((element (xmltok-start-tag-local-name))
                  (attr (nxml-find-following-attribute))
                  (dest nil))
              ;; if element is a short-name we need to build the full name
              (when (string-equal element "SHORT-NAME")
                (let ((file (expand-file-name (buffer-file-name)))
                      (line (line-number-at-pos)))
                  (dolist (name arxml-mode-tags-list)
                    (let ((tag (arxml-mode-lookup-tag name)))
                      ;; check ends with identifier
                      (when (and (>= (length name)
                                     (length identifier))
                                 (string-equal identifier
                                               (substring name (- (length identifier)) nil)))
                        (dolist (def (arxml-mode-tag-defs tag))
                          (when (and  (string-equal file (arxml-mode-tag-location-file def))
                                      (= line (arxml-mode-tag-location-line def)))
                            (setq identifier name))))))))
              (when attr
                (when (string-equal (xmltok-attribute-local-name attr) "DEST")
                  (setq dest (xmltok-attribute-value attr))))
              `((element . ,element)
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
              (desc . ,nil)
              (pos . ,(point)))
            arxml-mode-parse-stack)
    ;; else treat as data and end of tag
    (let ((current (car arxml-mode-parse-stack))
          (parent (cadr arxml-mode-parse-stack))
          (grandparent (cl-caddr arxml-mode-parse-stack))
          (type nil)
          (desc nil)
          (identifier nil)
          (element nil))
      (when (string-equal (alist-get 'tag current) "L-2")
        ;; assign this text to the desc property of parent of parent
        (setf (alist-get 'desc grandparent) text))
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
        (setq desc (alist-get 'desc current))
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
            (setq tag (make-arxml-mode-tag :type element :name identifier :desc desc :defs nil :refs nil))
            (cl-pushnew identifier arxml-mode-tags-list :test #'equal)
            (puthash identifier tag arxml-mode-tags-table))
          (let ((location (make-arxml-mode-tag-location :file (buffer-file-name) :line line :col col)))
            (pcase type
              ('def (push location (arxml-mode-tag-defs tag)))
              ('ref (push location (arxml-mode-tag-refs tag)))
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
          (setf (arxml-mode-tag-defs tag)
                (cl-delete-if #'arxml-mode-tag-location-is-current-buffer-p
                              (arxml-mode-tag-defs tag)))
          (setf (arxml-mode-tag-refs tag)
                (cl-delete-if #'arxml-mode-tag-location-is-current-buffer-p
                              (arxml-mode-tag-refs tag)))
          ;; if there are no definitions or references left then remove from tags
          ;; table
          (when (= 0 (+ (length (arxml-mode-tag-defs tag))
                        (length (arxml-mode-tag-refs tag))))
            (remhash identifier arxml-mode-tags-table)
            (push identifier orphans)))))
    ;; remove any orphans from the list
    (dolist (orphan orphans)
      (setq arxml-mode-tags-list
            (cl-delete orphan arxml-mode-tags-list :test #'equal))))
  ;; ensure we don't clobber match data since we run at buffer modification
  (save-match-data
    ;; use nxml to parse and hook in via the tag validation function
    (let ((nxml-parse-file-name (buffer-file-name))
          (nxml-validate-function #'arxml-mode-parse-tag))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (condition-case nil
              (nxml-parse-instance)
            (nxml-file-parse-error nil)))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql arxml)))
  (alist-get 'identifier (arxml-mode-identifier-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql arxml)) identifier)
  (arxml-mode-get-xrefs 'defs identifier))

(cl-defmethod xref-backend-references ((_backend (eql arxml)) identifier)
  (arxml-mode-get-xrefs 'refs identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql arxml)) identifier)
  (arxml-mode-ensure-tags)
  (let ((tags))
    (dolist (name arxml-mode-tags-list)
      (let ((tag (arxml-mode-lookup-tag name)))
        (when (string-match identifier name)
          (dolist (location (arxml-mode-tag-defs tag))
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
               (string-match "REF\\'" (alist-get 'element identifier)))
      (list (alist-get 'begin identifier)
            (alist-get 'end identifier)
            (cl-remove-if-not #'(lambda (name)
                                  ;; only include tags which have type matching
                                  ;; dest
                                  (let ((tag (arxml-mode-lookup-tag name)))
                                    (string-equal (alist-get 'dest identifier)
                                                  (arxml-mode-tag-type tag))))
                              ;; don't include current identifier
                              (cl-remove (alist-get 'identifier identifier)
                                         arxml-mode-tags-list
                                         :test #'equal))
            :exclusive 'no
            :company-docsig #'identity
            :company-doc-buffer #'(lambda (identifier)
                                    (let* ((tag (arxml-mode-lookup-tag identifier))
                                           (desc (arxml-mode-tag-desc tag)))
                                      (company-doc-buffer
                                       (format "%s\n\n%sDefined at:\n%s\n\nReferenced at:\n%s"
                                               identifier
                                               (if desc (concat desc "\n\n") "")
                                               (mapconcat
                                                #'arxml-mode-tag-location-to-string
                                                (arxml-mode-tag-defs tag) "\n")
                                               (mapconcat
                                                #'arxml-mode-tag-location-to-string
                                                (arxml-mode-tag-defs tag) "\n")))))
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
        (dolist (def (arxml-mode-tag-defs tag))
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
      (format "%s -> %s%s"
              (arxml-mode-tag-name tag)
              (propertize (arxml-mode-tag-type tag)
                          'face 'bold)
              (let ((desc (arxml-mode-tag-desc tag)))
                (if desc
                    (concat ": " desc)
                  ""))))))

(defun arxml-mode-reparse-buffer ()
  "Reparse buffer when `arxml-mode-reparse-on-buffer-change' is set."
  (when arxml-mode-reparse-on-buffer-change
    (arxml-mode-parse-buffer)))

(define-derived-mode arxml-mode nxml-mode "arxml"
  "Major mode for editing arxml files."
  ;; reparse on either buffer change on only on save
  (add-to-list 'after-change-functions #'arxml-mode-reparse-buffer)
  (add-hook 'after-save-hook #'arxml-mode-parse-buffer nil t)
  (add-to-list 'xref-backend-functions #'arxml-mode-xref-backend)
  (add-to-list 'completion-at-point-functions #'arxml-mode-completion-at-point)
  (setq imenu-create-index-function #'arxml-mode-imenu-create-index)
  (imenu-add-to-menubar "ARXML")
  (speedbar-add-supported-extension '("\\.arxml"))
  (add-function :before-until (local 'eldoc-documentation-function)
                #'arxml-mode-eldoc-function)
  ;; integrate with flycheck for validation
  (with-eval-after-load 'flycheck
    (setq flycheck-xml-xmlstarlet-xsd-path arxml-mode-xsd-path)
    (setq flycheck-xml-xmllint-xsd-path flycheck-xml-xmlstarlet-xsd-path))
  ;; limit company to only the useful backends
  (with-eval-after-load 'company
    (setq company-backends '((company-nxml company-capf)))))

;; use local schemas.xml to find our local AUTOSAR_00042.rnc
(add-to-list 'rng-schema-locating-files (expand-file-name "schemas.xml" arxml-mode-base-path))

(add-to-list 'auto-mode-alist '("\\.arxml\\'" . arxml-mode))

;; flyspell integration
(put 'arxml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)

;; yasnippet integration
(with-eval-after-load 'yasnippet
  (push (expand-file-name "snippets" arxml-mode-base-path)
        yas-snippet-dirs))

;; flycheck integration
(with-eval-after-load 'flycheck
  (eval-and-compile
    (defun arxml-mode-flycheck-start (checker callback)
      "Flycheck start routine for unresolved references with CHECKER and CALLBACK."
      (arxml-mode-ensure-tags)
      (let ((errs))
        (dolist (identifier arxml-mode-tags-list)
          (let ((tag (arxml-mode-lookup-tag identifier)))
            (when (and (= 0 (length (arxml-mode-tag-defs tag))))
              (dolist (ref (arxml-mode-tag-refs tag))
                (when (string-equal (buffer-file-name)
                                    (arxml-mode-tag-location-file ref))
                  (push
                   (flycheck-error-new-at
                    (arxml-mode-tag-location-line ref)
                    (arxml-mode-tag-location-col ref)
                    'error (format "Unresolved reference %s [%s]"
                                   (arxml-mode-tag-name tag)
                                   (arxml-mode-tag-type tag))
                    :checker checker
                    :filename (arxml-mode-tag-location-file ref)
                    :buffer (get-file-buffer (arxml-mode-tag-location-file ref)))
                   errs))))))
        (funcall callback 'finished errs))))

  (flycheck-define-generic-checker 'arxml
    "Check for invalid references etc in arxml files."
    :start #'arxml-mode-flycheck-start
    :modes '(arxml-mode))
  (add-to-list 'flycheck-checkers 'arxml t)

  (flycheck-add-mode 'xml-xmlstarlet 'arxml-mode)
  (flycheck-add-mode 'xml-xmllint 'arxml-mode)
  (flycheck-add-next-checker 'xml-xmlstarlet '(warning . arxml))
  (flycheck-add-next-checker 'xml-xmllint '(warning . arxml)))


;; smartparens integration
(with-eval-after-load 'smartparens
  (add-to-list 'sp-navigate-consider-sgml-tags 'arxml-mode))

;; evil-matchit integration
(with-eval-after-load 'evil-matchit
  (plist-put evilmi-plugins 'arxml-mode '((evilmi-template-get-tag evilmi-template-jump)
                                          (evilmi-simple-get-tag evilmi-simple-jump)
                                          (evilmi-html-get-tag evilmi-html-jump))))
;; all the icons integration
(with-eval-after-load 'all-the-icons
  (push '("\.arxml$" all-the-icons-faicon "file-code-o" :height 0.95 :face all-the-icons-lorange)
        all-the-icons-icon-alist)
  (push '(arxml-mode all-the-icons-faicon "file-code-o" :height 0.95 :face all-the-icons-lorange)
        all-the-icons-mode-icon-alist))

(provide 'arxml-mode)

;;; arxml-mode.el ends here
