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
(require 'yasnippet)
(require 'flycheck)

;; yasnippet integration
(push (expand-file-name "snippets" (file-name-directory
                                    (file-truename
                                     (expand-file-name
                                      (or load-file-name buffer-file-name)))))
      yas/snippet-dirs)

;; flycheck integration
(flycheck-def-option-var flycheck-arxml-schema-path
    (concat (file-name-directory
             (file-truename
              (expand-file-name
               (or load-file-name buffer-file-name))))
            "AUTOSAR_00042.xsd")
    arxml-xmllint
  "Path to the schema to validate ARXML files against.")

(flycheck-define-checker arxml-xmllint
  "A ARXML syntax checker and validator using the xmllint utility.

The xmllint is part of libxml2, see URL
`http://www.xmlsoft.org/'."
  :command ("xmllint" "--noout"
            (option "--schema" flycheck-arxml-schema-path nil)
            "-")
  :standard-input t
  :error-patterns
  ((error line-start "-:" line ": " (message) line-end))
  :modes (arxml-mode))

;; table to store index data
(defvar arxml-tags-table nil)

(defun arxml-parse-index (&optional index)
  "Parse the index specified by INDEX."
  (interactive "fIndex file:")
  (with-current-buffer (find-file-noselect index)
    (setq arxml-tags-table nil)
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([dr]\\) \\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\)$"
            nil t)
      (let ((type (substring-no-properties (match-string 1)))
            (symbol (intern (substring-no-properties (match-string 2))))
            (file (substring-no-properties (match-string 3)))
            (line (string-to-number (substring-no-properties (match-string 4))))
            (col  (string-to-number (substring-no-properties (match-string 5)))))
        ;; see if already in table and update
        (let ((tag (alist-get symbol arxml-tags-table)))
          (unless tag
            (setq tag `((symbol . ,symbol)
                        (def . ,nil)
                        (ref . ,nil))))
          (setf (alist-get symbol arxml-tags-table) tag)
          (pcase type
            ("d" (push `((file . ,file)
                         (line . ,line)
                         (col . ,col))
                       (alist-get 'def tag)))
            ("r" (push `((file . ,file)
                         (line . ,line)
                         (col . ,col))
                       (alist-get 'ref tag)))
            (_ (error "Unknown type %S" type))))))))

(defun arxml-find-tag (type tag)
  "Return an alist of the TYPE of TAG from the index file."
  (unless arxml-tags-table
    (arxml-parse-index (if (file-exists-p "index")
                           "index"
                         (read-file-name "Index file:" nil "index"))))
  (let ((tag (alist-get (intern tag) arxml-tags-table)))
    (alist-get type tag)))

;; only active when in arxml-mode
(defun arxml-xref-backend ()
  "Xref backend for arxml."
  (when (eq major-mode 'arxml-mode)
    'arxml))

(defun arxml-get-xrefs (type name)
  "Return a list of xref locations of TYPE for NAME."
  (remove nil
          (mapcar #'(lambda (tag)
                      (if tag
                          (xref-make name
                                     (xref-make-file-location (alist-get 'file tag)
                                                              (alist-get 'line tag)
                                                              (alist-get 'col tag)))))
                  (arxml-find-tag type name))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql arxml)))
  (let* ((current (substring-no-properties (symbol-name (symbol-at-point))))
         (matched (string-match ".*>\\(.*\\)<.*" current))
         (identifier (if matched (match-string 1 current))))
    identifier))

(cl-defmethod xref-backend-definitions ((_backend (eql arxml)) identifier)
  (arxml-get-xrefs 'def identifier))

(cl-defmethod xref-backend-references ((_backend (eql arxml)) identifier)
  (arxml-get-xrefs 'ref identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql arxml)) identifier)
  (let ((tags))
    (dolist (tag arxml-tags-table)
      (let ((name (symbol-name (car tag))))
        (when (string-match identifier name)
          (dolist (tag (alist-get 'def (cdr tag)))
            (push (xref-make name
                             (xref-make-file-location (alist-get 'file tag)
                                                      (alist-get 'line tag)
                                                      (alist-get 'col tag)))
                  tags)))))
    tags))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql arxml)))
  (let ((identifiers))
    (dolist (tag arxml-tags-table)
      (push (symbol-name (car tag)) identifiers))
    identifiers))

;; define our major-mode
(define-derived-mode arxml-mode nxml-mode "arxml"
  "Major mode for editing arxml files."
  (when (boundp 'flycheck-checkers)
    (add-to-list 'flycheck-checkers 'arxml-xmllint))
  (when (boundp 'xref-backend-functions)
    (add-to-list 'xref-backend-functions 'arxml-xref-backend)))

;; use local schemas.xml to find our local AUTOSAR_00042.rnc
(add-to-list 'rng-schema-locating-files (concat (file-name-directory
                                                 (or load-file-name buffer-file-name))
                                                "schemas.xml"))
(add-to-list 'auto-mode-alist '("\\.arxml\\'" . arxml-mode))

(provide 'arxml-mode)

;;; arxml-mode.el ends here
