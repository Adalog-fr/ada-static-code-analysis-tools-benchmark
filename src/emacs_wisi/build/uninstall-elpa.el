;; uninstall ada-mode, wisi from ~/.emacs.d/elpa, for
;; reinstall with different emacs version

(require 'package)

(package-initialize)

(setq ada-mode-version (getenv "ADA_MODE_VERSION"))
(setq ada-ref-man-version (getenv "ADA_REF_MAN_VERSION"))
(setq wisi-version (getenv "WISI_VERSION"))
(setq uniquify-files-version (getenv "UNIQUIFY_FILES_VERSION"))

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-dir-clean (name version)
  (let ((dir (pkg-dir name version)))
    (when (file-exists-p dir)
      (delete-directory dir t))
    )
  (let* ((elpa-dir (locate-user-emacs-file "elpa"))
	 (files (file-name-all-completions (concat name "-" version) elpa-dir)))
    (dolist (file files)
      (delete-file (concat elpa-dir "/" file)))))

(pkg-dir-clean "ada-mode" ada-mode-version)
(pkg-dir-clean "ada-ref-man" ada-ref-man-version)
(pkg-dir-clean "wisi" wisi-version)
(pkg-dir-clean "uniquify-files" uniquify-files-version)

;; end of file
