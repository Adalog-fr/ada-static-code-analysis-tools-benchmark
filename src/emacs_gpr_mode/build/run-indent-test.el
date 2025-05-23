;; Ada specific utils for automating indentation and casing tests

(package-initialize) ;; for uniquify-files, installed wisi, ada-mode

(require 'wisi-run-indent-test)

(setq gpr-mode-dir (file-name-directory (locate-file "gpr-mode.el" load-path)))
(cond
 ((string-match "elpa" gpr-mode-dir)
  (setq gpr-process-parse-exec (expand-file-name "~/.local/bin/gpr_mode_wisi_parse")))

 (t
  (setq gpr-process-parse-exec (expand-file-name "bin/gpr_mode_wisi_parse" gpr-mode-dir))))

(setq wisi-incremental-parse-enable t)

(setq eval-expression-debug-on-error nil)
(setq debug-on-error nil)
(setq-default wisi-parser-verbosity "debug=1")
(setq-default compare-tree-text t)
(setq-default wisi-process-time-out 30.0) ;; running with debug/assert is slow

(setq wisi-debug 1) ;; abort on non-syntax errors

(provide 'run-indent-test)
;; end of file
