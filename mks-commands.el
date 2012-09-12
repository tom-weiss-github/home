;; 
;; Author: Tom Weiss
;; email:  weiss@cs.wisc.edu
;; 
;; Library for executing common MKS Source Control commands on the 
;; current buffer in Emacs.
;; 

(defun mksco ()
  (interactive)
  "Perform MKS check out on the current buffer."
  (let ((mks-command "si"))
    (start-process "MKS-Checkout" "MKS-Checkout" mks-command "co" "--gui" "--revision=:working" (buffer-file-name))
    (setq echo-cmd (concat "si co --gui --revision=:working " (buffer-file-name)))
    (setq buffer-read-only nil)
    (message echo-cmd)
    )
  )

(defun mksci ()
  (interactive)
  "Performs MKS check in on the current buffer."
  (let ((mks-command "si" ))
    (start-process "MKS-Checkin" "MKS-Checkin" mks-command "ci" "--gui" (buffer-file-name))
    (setq echo-cmd (concat "si ci --gui " (buffer-file-name)))
    (setq buffer-read-only t)
    (message echo-cmd)
    )
  )

(defun mksvh ()
  (interactive)
  "Performs MKS View History on the current buffer."
  (let ((mks-command "si" ))
    (start-process "MKS-ViewHistory" "MKS-ViewHistory" mks-command "viewhistory" (concat "--cwd=" (file-name-directory buffer-file-name)) "--gui" (buffer-file-name))
    (setq echo-cmd (concat "si viewhistory --cwd=$" (file-name-directory buffer-file-name) "--gui" (buffer-file-name)))
    (message echo-cmd)
    )
  )

(defun mksvhm ()
  (interactive)
  "Performs MKS View History on the current buffer (filtered to see the last month)."
  (let ((mks-command "si" ))
    (start-process "MKS-ViewHistory" "MKS-ViewHistory" mks-command "viewhistory" "--rfilter=daterange::past:1:months" (concat "--cwd=" (file-name-directory buffer-file-name)) "--gui" (buffer-file-name))
    (setq echo-cmd (concat "si viewhistory --rfilter=daterange::past:1:months --cwd=$" (file-name-directory buffer-file-name) "--gui" (buffer-file-name)))
    (message echo-cmd)
    )
  )

(defun mksvhv (RR)
  (interactive "sEnter RR (07.RR): ")
  "Performs MKS View History on the current buffer (filtered based on the 07.RR)."
  (let ((mks-command "si" ))
    (start-process "MKS-ViewHistory" "MKS-ViewHistory" mks-command 
                   "viewhistory" 
                   (concat "--rfilter=labellike:*-07." RR "*,daterange::past:1:months") 
                   (concat "--cwd=" (file-name-directory buffer-file-name)) 
                   "--gui" (buffer-file-name))
    (setq echo-cmd (concat "si viewhistory " 
                           (concat "--rfilter=labellike:*-07." RR "*,daterange::past:1:months")  
                           " --cwd=" 
                           (file-name-directory buffer-file-name) 
                           " --gui " 
                           (buffer-file-name)))
    ;; Not sure why this interactive function does not echo the command if it's the last
    ;; list evaluated in the function, but using the message function causes the command
    ;; to be sent to the message area.
    (message echo-cmd) 
    )
  )

(defun mksdiff ()
  (interactive)
  "Performs MKS File Difference on the current buffer."
  (let ((mks-command "si" ))
    (start-process "MKS-ViewDifference" "MKS-ViewDifference" mks-command 
                   "diff" 
                   "--gui" (buffer-file-name))
    (setq echo-cmd (concat "si viewhistory --gui " (buffer-file-name)))
    (message echo-cmd)
    )
  )

(defun mkssb ()
  (interactive)
  "Opens MKS sandbox of the current buffer."
  (let ((mks-command "si" ))
    (start-process "MKS-OpenSandbox" "MKS-OpenSandbox" mks-command 
                   "viewsandbox"
                   (concat "--cwd=" (file-name-directory buffer-file-name))
                   "--gui")
    (setq echo-cmd (concat "si viewsandbox " 
                           (concat "--cwd=" (file-name-directory buffer-file-name))
                           " --gui"))
    (message echo-cmd)
    )
  )



;; Future Investigations:
;; Trying to figure out how to determine if the file is already branched on a private branch.  
;; The viewhistory command is what I believe I need to use.
;; Here's a sample:
;; si viewhistory --filter=locked --cwd=c:/tt-dev/dir file.h

;; This command will allow one to know the current member revision.
;; si memberinfo --cwd=c:/tt-dev/dir file.h

;; If this file is checked out by me, this command will tell me, and the revision too.
;; si.exe viewsandbox --cwd=C:/tt-dev/dir/dev --recurse --filter=changed:all --fields=name,workingrev

;; This page, http://emacswiki.org/emacs/esh-myparser.el, may have some good functions to investigate.

