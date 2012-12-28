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

(defun find-best-root (anchor-file)
  "Examines the parent directories of the current buffer.  Looks for a parent that contains the
file passed in the anchor-file argument.  This is the directory from which I want."

  (if (not buffer-file-name)
      ;; Certain buffer (e.g., *scratch*) return nil from buffer-file-name.  In that case,
      ;; set the best path to "/" since that's the only path which can be counted on.
      "/"
    (let ((path-depth (safe-length (split-string (file-name-directory buffer-file-name) "/" 1)))
          (best-root (file-name-directory buffer-file-name)))
      (while (> path-depth 0)
        (setq path-as-list (butlast (split-string (file-name-directory buffer-file-name) "/") path-depth))
        (setq potential-root (mkpath path-as-list))
        (if (file-exists-p (concat potential-root anchor-file))
            (setq best-root potential-root)
          nil)
        (setq path-depth (- path-depth 1))
        )
      best-root
      )
    )
)
