;;
;; GNU Emacs TAGS Configuration
;;
;; Author: Tom Weiss
;; email:  weiss@cs.wisc.edu
;;
;; Useful tag related commands:
;; find-tag
;; tags-search
;; tags-loop-continue (M-,)

(load-file "~/.emacs.d/find-best-root.el")

(defun set-my-tag-file-and-search (the-tag)
  (interactive "sEnter tag: " )
  "Dynamically set the emacs tag file based on the current buffer's development file tree.  Once that's set
   invoke the built in find-tag method."
  (let ( (expected-tag-file-absolute-path "")
         (count 100) )
    (progn
      (setq expected-tag-file-absolute-path (concat (find-best-root "makefile") "TAGS"))

      ;; Create the TAGS file if it doesn't exist.
      (if (not (file-exists-p expected-tag-file-absolute-path))
          (call-interactively 'create-tag-file))

      ;; Hopefully the TAGS file exists now.
      (if (file-exists-p expected-tag-file-absolute-path)
          (progn (visit-tags-table expected-tag-file-absolute-path)
                 (find-tag the-tag))
        (message (concat "Expected to find " expected-tag-file-absolute-path
                         ", but didn't, NO TAGS FOR YOU!")))
      )
    )
  )

(defun create-tag-file ()
  (interactive)
  "Create the TAGS file."

  (let ( (expected-tag-file-absolute-path "")
         (etags-command "")
         (best-path (find-best-root "makefile" t))
         (seconds-to-wait 20)
         (seconds-waited 20) )
    (progn
      (if (eq "" best-path)
          (error "The current buffer isn't in a source tree, TAGS cannot be created")
        )

      (setq expected-tag-file-absolute-path (concat best-path "TAGS"))

      ;; Delete the existing TAGS file if it exists.
      (if (file-exists-p expected-tag-file-absolute-path)
          (delete-file expected-tag-file-absolute-path)
        )

      ;; Note an '-or' is required to connect -iname options.
      (setq etags-command (concat "find " best-path " -type f "
                                  " -iname '*.h' "
                                  " | etags --language=c++ --output=" best-path "TAGS" " - "  ))
      (message etags-command)
      (start-process-shell-command "ETAGS" "ETAGS" etags-command)

      ;; I've noticed that the shell process may return before the
      ;; tags processing is fully complete.  That may cause the caller
      ;; visiting a partically completed tag file.  This loop will
      ;; wait for the command to complete, with a timeout.
      (while (not (eq seconds-waited 0))
        ;;(message (symbol-name (process-status "ETAGS")))
        (message (concat "Creating " best-path "TAGS"
                         (make-string (- seconds-to-wait seconds-waited) 46) ;; 46 is ASCII for "."
                         " (max " (int-to-string seconds-to-wait)
                         " seconds)"))
        (sit-for 1)
        (if (not (process-status "ETAGS"))
            (progn
              (setq seconds-waited 0))
          (setq seconds-waited (- seconds-waited 1))))

      (message (concat "Finished creating " best-path "TAGS"))
      )
    )
  )

;; Useful way to view the results of process-status.
;; (message (symbol-name (process-status "ETAGS"))

