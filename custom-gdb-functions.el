;;
;; Inserts various GDB commands when debugging.
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;

(load-file "~/.emacs.d/find-best-root.el")

(defun wrapper()
  (interactive)
  "Paste gdb wrapper command."
  (let ( (gdb-exec-wrapper-cmd (concat "set exec-wrapper " (find-best-root "makefile") "run" ) ) )
    (other-window 1) ;; The same as C-X o.
    (insert gdb-exec-wrapper-cmd)
    )
  )



