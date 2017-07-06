;;; arxml-mode --- Major mode for editing arxml files
;; -*- lexical-binding: t -*-

;;; Commentary:

;;
;; TODO: Add imenu integration like ant-mode.el https://github.com/tkg/ant-mode
;;
;; TODO: Add more yasnippets
;;

;;; Code:
(require 'nxml-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'thingatpt)
(require 'yasnippet)
(require 'flycheck)

(defconst arxml-mode-base-path
  (file-name-directory
   (file-truename
    (expand-file-name
     (or load-file-name buffer-file-name)))))

;; path to the AUTOSAR_00042.xsd shipped with arxml-mode.el
(defvar arxml-mode-xsd-path
  (expand-file-name "AUTOSAR_00042.xsd" arxml-mode-base-path))

;; yasnippet integration
(push (expand-file-name "snippets" arxml-mode-base-path)
      yas-snippet-dirs)

;; flycheck integration
(flycheck-add-mode 'xml-xmllint 'arxml-mode)
(flycheck-add-mode 'xml-xmlstarlet 'arxml-mode)

;; xref integration
;; table to store index data
(defvar arxml-mode-tags-table nil)

;; list to store tag names for xref-apropos and completion-at-point
(defvar arxml-mode-tags-list nil)

;; a tag - has definitions and references
(cl-defstruct arxml-mode-tag name def ref)

;; a tag location (ie. a definition or reference) - has file, line number and
;; column number
(cl-defstruct arxml-mode-tag-location file line col)

(defun arxml-mode-parse-index (&optional index)
  "Parse the index specified by INDEX."
  (interactive "fIndex file:")
  (with-current-buffer (find-file-noselect index)
    (setq arxml-mode-tags-table (make-hash-table :test 'equal))
    (setq arxml-mode-tags-list nil)
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([dr]\\) \\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\)$"
            nil t)
      (let ((type (substring-no-properties (match-string 1)))
            (name (substring-no-properties (match-string 2)))
            (file (substring-no-properties (match-string 3)))
            (line (string-to-number (substring-no-properties (match-string 4))))
            (col  (string-to-number (substring-no-properties (match-string 5)))))
        ;; see if already in table and update
        (let ((tag (gethash name arxml-mode-tags-table)))
          (unless tag
            (setq tag (make-arxml-mode-tag :name name :def nil :ref nil))
            (push name arxml-mode-tags-list)
            (puthash name tag arxml-mode-tags-table))
          (let ((location (make-arxml-mode-tag-location :file file :line line :col col)))
            (pcase type
              ("d" (push location (arxml-mode-tag-def tag)))
              ("r" (push location (arxml-mode-tag-ref tag)))
              (_ (error "Unknown type %S" type)))))))))

(defun arxml-mode-ensure-index ()
  (unless arxml-mode-tags-list
    (unless (file-exists-p "index")
      (unless (zerop (process-file
                      (expand-file-name "arxml.py" arxml-mode-base-path)
                      nil nil nil "-f" "index"))
        (error "Failed: '%s -f index'" (expand-file-name "arxml.py" arxml-mode-base-path))))
    (arxml-mode-parse-index "index")))

(defun arxml-mode-find-tag-location (type name)
  "Return an arxml-mode-tag-location of the TYPE of NAME from the index file."
  (arxml-mode-ensure-index)
  (let ((tag (gethash name arxml-mode-tags-table)))
    (when tag
      (pcase type
        ('def (arxml-mode-tag-def tag))
        ('ref (arxml-mode-tag-ref tag))
        (_ (error "Unknown type %S" type))))))

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
                  (arxml-mode-find-tag-location type name))))

(defun arxml-mode-identifier-at-point ()
  "Get a plist containing the arxml identifier at point."
  (let* ((current (thing-at-point 'symbol t))
         (start (car (bounds-of-thing-at-point 'symbol)))
         (matched (string-match ".*>\\(.*\\)\\(<.*?\\)" current)))
    (when matched
      `((identifier . ,(match-string 1 current))
        (begin . ,(+ start (match-beginning 1)))
        (end . ,(+ start (match-end 1)))))))


(defun arxml-mode-create-index (&optional dir)
  "Generate index file for DIR."
  (interactive
   (read-directory-name "Root Directory: "))
  (unless dir
    (setq dir default-directory))
  (let ((default-directory dir)
        (proc-buf (get-buffer-create " *arxml-mode-create-index*")))
    (let ((proc (start-file-process "arxml-mode-create-index" proc-buf
                                    (expand-file-name "arxml.py" arxml-mode-base-path)
                                    "-f" "index")))
      (set-process-sentinel proc
                            #'(lambda (process _event)
                                (when (eq (process-status process) 'exit)
                                  (if (zerop (process-exit-status process))
                                      (message "Success: created index")
                                    (message "Failed to create index (%d)"
                                             (process-exit-status process)))))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql arxml)))
  (alist-get 'identifier (arxml-mode-identifier-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql arxml)) identifier)
  (arxml-mode-get-xrefs 'def identifier))

(cl-defmethod xref-backend-references ((_backend (eql arxml)) identifier)
  (arxml-mode-get-xrefs 'ref identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql arxml)) identifier)
  (arxml-mode-ensure-index)
  (let ((tags))
    (dolist (name arxml-mode-tags-list)
      (let ((tag (gethash name arxml-mode-tags-table)))
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
  (arxml-mode-ensure-index)
  arxml-mode-tags-list)

;; completion at point
(defun arxml-mode-completion-at-point ()
  "`completion-at-point' function for arxml-mode."
  (let ((identifier (arxml-mode-identifier-at-point)))
    (when identifier
      (arxml-mode-ensure-index)
      (list (alist-get 'begin identifier)
            (alist-get 'end identifier)
            arxml-mode-tags-list
            :exclusive 'no
            :company-docsig #'identity))))

;; define our major-mode
(define-derived-mode arxml-mode nxml-mode "arxml"
  "Major mode for editing arxml files."
  (add-to-list 'xref-backend-functions 'arxml-mode-xref-backend)
  (add-to-list 'completion-at-point-functions #'arxml-mode-completion-at-point)
  ;; integrate with flycheck
  (setq flycheck-xml-xmlstarlet-xsd-path arxml-mode-xsd-path)
  (setq flycheck-xml-xmllint-xsd-path flycheck-xml-xmlstarlet-xsd-path))

;; use local schemas.xml to find our local AUTOSAR_00042.rnc
(add-to-list 'rng-schema-locating-files (concat (file-name-directory
                                                 (or load-file-name buffer-file-name))
                                                "schemas.xml"))
(add-to-list 'auto-mode-alist '("\\.arxml\\'" . arxml-mode))

(provide 'arxml-mode)

;;; arxml-mode.el ends here
