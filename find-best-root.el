;;
;; Author: Tom Weiss
;; email:  weiss@cs.wisc.edu
;;
;; Library for finding root directory of source tree.  This library is useful
;; when a machine might have multiple directory trees of source files.  If a
;; file of one of those trees in open in Emacs, certain operations should be
;; performed based on tree in which the file exists, but not any other source
;; trees.  In order to locate the root of the tree, searching begins at the
;; current directory of the file and proceeds upward until some anchor-file
;; is found.  Typical examples include searching, compiling, etc.
;;

(defun mkpath (list)
  "Return a string which is the concatenation of the list."
  (let ( (result "") )
    (while list
      (setq result (concat result (car list)))
      (setq result (concat result "/"))
      (setq list (cdr list))
      )
    result ; Since this is the final evaluation, it's the returned from the function.
    )
  )

(defun find-best-root (anchor-file &optional empty-on-failure)
  "Examines the parent directories of the current buffer.  Looks for a parent that contains the
file passed in the anchor-file argument.  This is the directory I want."

  (if (not buffer-file-name)
      ;; Certain buffer (e.g., *scratch*) return nil from buffer-file-name.  In that case,
      ;; set the best path to "/" since that's the only path which can be counted on.
      (if (eq nil empty-on-failure)
          "/"
        ""
        )
    (let ((path-depth (safe-length (split-string (file-name-directory buffer-file-name) "/" 1)))
          (best-root (if (eq nil empty-on-failure)
                         (file-name-directory buffer-file-name)
                       ""))
          (exclude-from-path 1))
      (while (<= exclude-from-path (+ path-depth 1))
        (setq path-as-list (butlast (split-string (file-name-directory buffer-file-name) "/") exclude-from-path))
        (setq potential-root (mkpath path-as-list))
        (message (concat "Checking in " potential-root))
        (if (file-exists-p (concat potential-root anchor-file))
            (progn (setq best-root potential-root)
                   (setq exclude-from-path (+ path-depth 2)) ;; Break from the loop.
                   (message (concat "Found " anchor-file)))
          (setq exclude-from-path (+ exclude-from-path 1)))
        )
      best-root
      )
    )
  )
