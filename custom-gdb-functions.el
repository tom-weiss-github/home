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

;; Next
;; It would be useful to wrap all the gdb steps.
;; - A function which would use the current build target in combination with
;;   the find-best-root as the program to debug.
;; - Split the window (if necessary) and execute the debugger command.
;; - Run the gdb wrapper function.

