;;
;; Custom File Cache
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;

(load-file "~/.emacs.d/find-best-root.el")
(require 'filecache)
(setq d-current-file-cache "")

(defun d-set-my-file-cache ()
  "This function sets or creates a file cache based on the currently open buffer."
  (interactive)

  ;; The file 'makefile' exists at the root of the source tree.  If this file isn't
  ;; found in some parent directory, custom caching won't be enabled.
  (setq d-marker-file "makefile")

  (setq d-file-cache-name "FILES")
  (setq d-dev-root (find-best-root d-marker-file))
  (setq d-file-cache-abs-path (concat d-dev-root d-file-cache-name))


  ;; The caching should take effect for development trees, but many files
  ;; are not part of a development tree.  For those, let's avoid using
  ;; the cache.  In those cases, this function will return without
  ;; taking any action.
  (if (file-exists-p (concat d-dev-root d-marker-file))
      (if (file-exists-p d-file-cache-abs-path)
          (progn
            (if (string= d-file-cache-abs-path d-current-file-cache)
                (message (concat "Same cache, '" d-file-cache-abs-path "' nothing to do."))
              (progn
                (message (concat "Switching from '" d-current-file-cache "' to '" d-file-cache-abs-path "'."))
                (file-cache-clear-cache)
                (let ((buf (find-file-noselect d-file-cache-abs-path)))
                  (setq file-cache-alist (read buf))
                  (kill-buffer buf))
                (setq d-current-file-cache d-file-cache-abs-path)
                )
              )
            )
        (progn
          (message "Didn't find an existing cache, creating...")
          (file-cache-clear-cache)
          (file-cache-add-directory-using-find d-dev-root)
          (with-temp-file d-file-cache-abs-path
            (prin1 file-cache-alist (current-buffer)))
          (setq d-current-file-cache d-file-cache-abs-path)
          (message (concat "Cache creation complete, '" d-current-file-cache  "'."))
          )
        )
    (message "Not a valid source tree, no custom file caching.\n")
    )
)

(defun d-find-file-with-custom-file-cache ()
  "This function sets the file cache (potentially creating one) if
the current buffer is found to be in a source tree.  Regardless
of setting the cache, find-file will be invoked.  It's intended
that this function should be mapped to C-x C-f to get automatic
caching when files are opened."
  (interactive)
  (d-set-my-file-cache)
  (call-interactively 'find-file)
)