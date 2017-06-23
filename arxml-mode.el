;;; arxml-mode --- Major mode for editing arxml files

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

;; try and dynamically setup yasnippet snippets if available
(when (and (require 'yasnippet nil t)
           (boundp 'yas/snippet-dirs))
  (push (expand-file-name "snippets" (file-name-directory
                                      (or load-file-name buffer-file-name)))
        yas/snippet-dirs))

;; try and dynamically integrate with flychck
(when (and (require 'flycheck nil t)
           (fboundp 'flycheck-define-checker)
           (fboundp 'flycheck-def-option-var))
  (flycheck-def-option-var flycheck-arxml-schema-path
      (concat (file-name-directory
               (or load-file-name buffer-file-name))
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
    :modes (arxml-mode)))

;; hash table to store index data
(define-hash-table-test 'string-equal-test 'string= 'sxhash)

(defvar arxml-tags-table nil)

(defun arxml-parse-index (&optional index)
  "Parse the index specified by INDEX."
  (interactive "fIndex file:")
  (with-current-buffer (find-file-noselect index)
    (if arxml-tags-table
        (clrhash arxml-tags-table)
      (setq arxml-tags-table (make-hash-table :test #'string-equal-test)))
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([dr]\\) \\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\)$"
            (point-max) t)
      (let ((type (substring-no-properties (match-string 1)))
            (name (substring-no-properties (match-string 2)))
            (file (substring-no-properties (match-string 3)))
            (line (string-to-number (substring-no-properties (match-string 4))))
            (col  (string-to-number (substring-no-properties (match-string 5)))))
        ;; see if already in table and update
        (let ((entry (gethash name arxml-tags-table)))
          (unless entry
            ;; create a new entry
            (setq entry '((def . nil)
                          (ref . nil)))
            (puthash name entry arxml-tags-table))
          (pcase type
            ("d" (setf (cdr (assoc 'def entry)) `((file . ,file)
                                                  (line . ,line)
                                                  (col . ,col))))
            ("r" (setf (cdr (assoc 'ref entry))
                       (push `((file . ,file)
                               (line . ,line)
                               (col . ,col))
                             (cdr (assoc 'ref entry)))))
            (_ (error "Unknown type %S" type))))))))

(defun arxml-find-entry (type symbol)
  "Return an alist of the TYPE of SYMBOL from the index file."
  (unless arxml-tags-table
    (arxml-parse-index (read-file-name "Index file:" nil "index")))
  (let ((entry (gethash symbol arxml-tags-table)))
    (cdr (assoc type entry))))

(defun arxml-find-definition (symbol)
  "Return an alist of the definition of SYMBOL from the index file."
  (arxml-find-entry 'def symbol))

(defun arxml-find-references (symbol)
  "Return a list of alists of the references of SYMBOL from the index file."
  (arxml-find-entry 'ref symbol))

;; only active when in arxml-mode
(defun arxml-xref-backend ()
  "arxml backend for Xref."
  (when (eq major-mode 'arxml-mode)
    'arxml))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql arxml)))
  (let* ((current (substring-no-properties (symbol-name (symbol-at-point))))
         (matched (string-match ".*>\\(.*\\)<.*" current))
         (identifier (if matched (match-string 1 current))))
    identifier))

(cl-defmethod xref-backend-definitions ((_backend (eql arxml)) symbol)
  (remove nil
          (mapcar #'(lambda (def)
                      (if def
                          (xref-make symbol
                                     (xref-make-file-location (cdr (assoc 'file def))
                                                              (cdr (assoc 'line def))
                                                              (cdr (assoc 'col def))))))
                  (list (arxml-find-definition symbol)))))

(cl-defmethod xref-backend-references ((_backend (eql arxml)) symbol)
  (remove nil
          (mapcar #'(lambda (ref)
                      (if ref
                          (xref-make symbol
                                     (xref-make-file-location (cdr (assoc 'file ref))
                                                              (cdr (assoc 'line ref))
                                                              (cdr (assoc 'col ref))))))
                  (arxml-find-references symbol))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql arxml)))
  (hash-table-keys arxml-tags-table))

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
