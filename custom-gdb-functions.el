;;
;; GDB related functions.
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;

(load-file "~/.emacs.d/find-best-root.el")

(defun wrapper()
  (interactive)
  "Paste gdb wrapper command."
  (let ( (gdb-exec-wrapper-cmd (concat "set exec-wrapper " (find-best-root "makefile") "run" ) ) )
    (other-window 1) ;; Bound to C-X o, 'describe-bindings' to see all bindings.
    (insert gdb-exec-wrapper-cmd)
    )
  )

(defun dbug()
  "Custom steps to set up the debugger."
  (interactive)

  (let ( (dbug-target "")
         (best-path (find-best-root "makefile" t) )
         (gdb-init-file "~/.gdbinit")
         (is-gdb-running nil) )

    (setq dbug-target (read-from-minibuffer
                       (concat "Enter Debug Target: ")
                       d-compile-target))
    ;;NOTE: d-compile-target is set in custom-compile.el.

    (if (eq "" best-path)
        (error "The current buffer isn't in a source tree")
      )

    (setq dbug-target-path (concat
                            best-path
                            "build/x86-64/debug/bin/"
                            dbug-target))
    (if (not (file-exists-p dbug-target-path))
        (error (concat "Target file '" dbug-target-path "' does not exist"))
      )

    ;; Delete an existing ~/.gdbinit file if it exists.
    (if (file-exists-p gdb-init-file)
        (delete-file gdb-init-file)
      )

    ;; Create a new ~/.gdbinit file with the commands based on the current
    ;; build target.
    (with-temp-buffer
      (insert (concat "set exec-wrapper " best-path "run\n"))
      (insert (concat "file " best-path "build/x86-64/debug/bin/" dbug-target "\n"))
      (insert "set follow-fork-mode child\n")
      (insert (concat "cd " best-path "build/x86-64/debug/bin"))
      (insert "add-auto-load-safe-path /home/tweiss\n")
      (write-file gdb-init-file)
      )

    ;; If the global universal debugger window is already created, make
    ;; it visible in a split window, splitting the window if it's not split.
    (if (get-buffer "*gud*")
        (progn
          ;; Split the screen if it's not already split.
          (if (eq (window-width) (screen-width))
              (split-window-horizontally) ;; Bound to C-x 3.
            )
          (other-window 1)
          (switch-to-buffer "*gud*")
          (other-window 1))
      )

    ;; The variable mode-line-process is a buffer local variable which has a
    ;; value ":exit" when the debugger is no longer running from the *gud* buffer
    ;; and a longer value when the debugger is running.
    ;; This variable will be used to decide whether we should invoke gdb again or
    ;; not.  All buffer local variables can be viewed with
    ;; (message "%s" (buffer-local-variables))
    ;;(is-gdb-running (buffer-local-value 'mode-line-process (get-buffer "*gud*"))) )
    (if (get-buffer "*gud*")
        (progn
          (message "%s" (buffer-local-value 'mode-line-process (get-buffer "*gud*")))
          (if (string= ":exit" (buffer-local-value 'mode-line-process (get-buffer "*gud*")))
              (setq is-gdb-running nil)
            (setq is-gdb-running t)
            )
          )
      )

    ;; Call gdb if it's not already running.
    (if (not is-gdb-running)
        (call-interactively 'gdb)
      (message "gdb is already running")
      )
    )
)
