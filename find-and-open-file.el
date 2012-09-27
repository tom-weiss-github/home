;; 
;; Custom Find File
;; 
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;

(load-file "~/.emacs.d/find-best-root.el")

(defun walk-path (dir action)
  "walk DIR executing ACTION with (dir file)
This method was largely derived from an example 
at http://emacswiki.org/emacs/ElispCookbook."
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t))
               fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (walk-path fullname action)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))


(defun test-if-file-is-the-desired-one (dir file)
  "This method checks to see if this file is the one we are looking for."
  ;; (if (string= file the-desired-file-to-open) 
  ;;     (setq the-desired-file-to-open-full-path (concat dir file))
  ;;   nil )
  (if (string= file the-desired-file-to-open)
      (progn
        ;;(message (concat "Match: " file " " the-desired-file-to-open))
        (setq the-desired-file-to-open-full-path (concat dir file)))
    ;;(message (concat "No Match: " file " " the-desired-file-to-open)))
    "Scanning..." )
    ;;nil)
)




(defun fao (desired-file)
 "Find-And-Open, the purpose of this method is to search for a file in
the source tree of the current buffer.  If found, the file is opened.
This function is useful for source files in large source trees when 
it's faster to type the name of the file rather than naviate to its
path."
 (interactive "sEnter file: " )
 (setq the-desired-file-to-open desired-file)
 (setq the-desired-file-to-open-full-path "")
 
 (setq tt-dev-home (find-best-root "manifest.json"))
 
 ;; Create a list of directories to search.  Invoke walk-path on each one.
 ;; If the file is found, stop.
 (dolist (directory-to-search '( "gw"
                                 "misc/os_common" ))
   (message (concat "Scanning " tt-dev-home directory-to-search))
   (walk-path (concat tt-dev-home directory-to-search) 'test-if-file-is-the-desired-one)
   )


 (message (concat "Found: " the-desired-file-to-open-full-path))

 (if (string= "" the-desired-file-to-open-full-path)
     (message (concat "Unable to find " desired-file "."))
   (find-file the-desired-file-to-open-full-path))
)








