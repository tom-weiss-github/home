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
  (let ( (expected-tag-file-absolute-path "") )
    (progn
      (setq expected-tag-file-absolute-path (concat (find-best-root "TAGS") "TAGS"))
      (if (file-exists-p expected-tag-file-absolute-path)
          (progn (visit-tags-table expected-tag-file-absolute-path)
                 (find-tag the-tag)
                  )
        (message (concat "Expected to find " expected-tag-file-absolute-path ", but didn't, NO TAGS FOR YOU!"))
        )
      )
    )
)


;; TAGS
;; Documentation:
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/List-Tags.html#List-Tags
;; find . \( -name "*.cpp" -o -name "*.h" -o -name "*.c" -o -name "*.inl" \) -print | c:/emacs-24.1/bin/etags.exe --output=pcr-119133-tags --language=c++ -
;;                                                                                               (leave out for TAGS to be the name)
