;;; arxml-mode --- Major mode for editing arxml files

;;; Commentary:

;;; Code:
(require 'nxml-mode)

(define-derived-mode arxml-mode nxml-mode "arxml"
  "Major mode for editing arxml files.")

;; use local schemas.xml to find our local AUTOSAR_00042.rnc
(add-to-list 'rng-schema-locating-files (concat (file-name-directory load-file-name)
                                                "schemas.xml"))
(add-to-list 'auto-mode-alist '("\\.arxml\\'" . arxml-mode))

(provide 'arxml-mode)

;;; arxml-mode.el ends here
