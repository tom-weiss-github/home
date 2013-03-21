;;
;; GNU Emacs TAGS Configuration
;;
;; Author: Tom Weiss
;; email:  weiss@cs.wisc.edu
;;

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

      ;; while count < 100
      ;;(setq count 100)
      ;;(not (eq count 0))
      ;;(setq count (- count 1))
      ;;;(message (int-to-string count))
      ;;(or nil nil))
      ;; (while (not (eq count 0))
      ;;   (message (int-to-string count))
      ;;   (message (symbol-name (process-status "ETAGS")))
      ;;   (if (file-exists-p expected-tag-file-absolute-path)
      ;;       (progn
      ;;         (message (concat "left loop at " (int-to-string count)))
      ;;         (setq count 0))
      ;;     (setq count (- count 1))))

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
  "Create the TAGS file if it doesn't exist."

  (let ( (expected-tag-file-absolute-path "")
         (etags-command "")
         (best-path (find-best-root "makefile" t)) )
    (progn
      (if (eq "" best-path)
          (error "The current buffer isn't in a source tree, TAGS cannot be created")
        )

      (setq expected-tag-file-absolute-path (concat best-path "TAGS"))

      ;; Delete the existing TAGS file if it exists.
      (if (file-exists-p expected-tag-file-absolute-path)
          (delete-file expected-tag-file-absolute-path)
        )

      (setq etags-command (concat "find "
                                  best-path
                                  " -type f "
                                  " -iname '*.c' -or "
                                  " -iname '*.cpp' -or "
                                  " -iname '*.h' "
                                  " | etags "
                                  " --output="
                                  best-path
                                  "TAGS"
                                  " - "
                                  ))
      (message (concat "Creating " best-path "TAGS... "))
      (start-process-shell-command "ETAGS" "ETAGS" etags-command)
      (setq count 1000)
      (while (not (eq count 0))
        ;;(message (int-to-string count))
        ;;(message (symbol-name (process-status "ETAGS")))
        (sit-for 1)
        (if (not (process-status "ETAGS"))
            (progn
              ;;(message (concat "left loop at " (int-to-string count)))
              (setq count 0))
          (setq count (- count 1))))

      (message (concat "Finished creating " best-path "TAGS"))
      )
    )
  )

;; find-tag
;; tags-search
;; tags-loop-continue (M-,)

;; TAGS
;; Documentation:
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/List-Tags.html#List-Tags
;; find . \( -name "*.cpp" -o -name "*.h" -o -name "*.c" -o -name "*.inl" \) -print | c:/emacs-24.1/bin/etags.exe --output=pcr-119133-tags --language=c++ -
;;                                                                                               (leave out for TAGS to be the name)
