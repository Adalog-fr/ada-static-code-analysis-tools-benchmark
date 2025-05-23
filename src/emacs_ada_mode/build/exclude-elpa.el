;; Disable installed ELPA packages, except uniquify-files
(defvar package-load-list '(all))
(push '(ada-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)
(push '(gnat-compiler . nil) package-load-list)
(push '(gpr-mode . nil) package-load-list)
(push '(gpr-query . nil) package-load-list)

;; FIXME: testing devel eglot
(push '(eglot . nil) package-load-list)

;; also disable native compilation; it just slows things down
(setq native-comp-deferred-compilation nil)

;; end of file
