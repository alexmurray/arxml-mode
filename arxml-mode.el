;;; arxml-mode --- Major mode for editing arxml files

;;; Commentary:

;; TODO: Add yasnippet integration like xproc-mode
;; https://github.com/bertfrees/xproc-mode for common snippets
;;; Code:
(require 'nxml-mode)
;; try load flycheck if available
(require 'flycheck nil t)

(define-derived-mode arxml-mode nxml-mode "arxml"
  "Major mode for editing arxml files."
  (when (boundp 'flycheck-checkers)
    (add-to-list 'flycheck-checkers 'arxml-xmllint)))

;; use local schemas.xml to find our local AUTOSAR_00042.rnc
(add-to-list 'rng-schema-locating-files (concat (file-name-directory
                                                 (or load-file-name buffer-file-name))
                                                "schemas.xml"))
(add-to-list 'auto-mode-alist '("\\.arxml\\'" . arxml-mode))

(when (fboundp 'flycheck-define-checker)
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

(provide 'arxml-mode)

;;; arxml-mode.el ends here
