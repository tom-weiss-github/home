;; 
;; Dynamically sets the find-grep to be useful for the buffer.
;; 
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;
;; Notes about using find and grep.
;; --------------------------------
;; Cygwin's grep/find/xargs seem to work with GNU Emacs as long as xargs is used.  It's important to
;; add c:\cygwin\bin to the Window's path so that those are found before Windows' find.
;; If cygwin is not an option, native utilities can be found from http://gnuwin32.sourceforge.net/.  Click to 
;; http://sourceforge.net/projects/getgnuwin32/files/ and download GetGnuWin32-0.6.3.exe.  Run this program
;; to download all the utilities.  Install the utilities to c:\gnuwin32 and then add c:\gnuwin32\bin to the
;; path (cygwin will still find its own).
;; NOTE:  I observe that if I do find-grep with a file open in the c:/emacs-21.3/bin directory, it seems
;; to use window's find and therefore not work.  Not sure why.


(load-file "~/.emacs.d/find-best-root.el")

(setq find-program "c:/cygwin/bin/find.exe")


;; Load the grep-apply-setting function.
(autoload 'grep-apply-setting "grep"
  "Set SYMBOL to VALUE, and update 'grep-host-defaults-alist'.
SYMBOL should be one of 'grep-command', 'grep-template',
'grep-use-null-device', 'grep-find-command',
'grep-find-template', 'grep-find-use-xargs', or
'grep-highlight-matches'.")

;; Once fg() is run, these settings don't seem to have any effect.  However, if find-grep is
;; run before fg(), then these are the settings which will be used.  Even though I may not
;; use them, it's a good idea to leave these in case I want to revert back to more simple
;; find-grep execution.
(grep-apply-setting 'grep-command "grep -nHi -e ")
(grep-apply-setting 'grep-use-null-device nil)
(grep-apply-setting 'grep-find-use-xargs t)



;; The -print0 in find causes it to put NULLs at the end of the file names (which helps with spaces in the names).  The -0 in
;; xargs then uses the NULL instead of spaces.  This particular method solves a problem the previous two suffer from.  I noticed
;; that if a file with an extension I want to filter out was in the same directory as the file which was open, then find would
;; return an error.
(defun fg ()
  "A custom find grep that dynamically sets the search path based on the buffer."
  (interactive)
  (let ((fg-tt-filters " -type f         -path \"*/misc/xerces\" -prune -o -path \"*/misc/boost\" -prune -o -path \"*/misc/xmlhelp\" -prune -o -path \"*/misc/ace\" -prune -o -path \"*/misc/cppunit\" -prune -o -path \"*/misc/performance_logger\" -prune -o -path \"*/misc/pfx\" -prune -o -path \"*/misc/unittestframework\" -prune -o \"(\" -not -name TAGS -and -not -regex \".*log\" -and -not -regex \".*idb\" -and -not -regex \".*pch\" -and -not -regex \".*sbr\" -and -not -regex \".*ipch\" -and -not -regex \".*zip\" -and -not -regex \".*xml\" -and -not -regex \".*exe\" -and -not -regex \".*dll\" -and -not -regex \".*manifest\" -and -not -regex \".*csv\" -and -not -regex \".*ilk\" -and -not -regex \".*bsc\" -and -not -regex \".*map\" -and -not -regex \".*sdf\" -and -not -regex \".*vsd\" -and -not -regex \".*lib\" -and -not -regex \".*obj\" -and -not -regex \".*pdb\" \")\"  -print0 | xargs -0 grep -nHi -e " ))
    (setq my-find-grep-command "find \"")
    (setq my-find-grep-command (concat my-find-grep-command (find-best-root "manifest.json")))
    (setq my-find-grep-command (concat my-find-grep-command "\""))
    (setq my-find-grep-command (concat my-find-grep-command fg-tt-filters))
    (grep-apply-setting 'grep-find-command my-find-grep-command)
    (call-interactively 'find-grep)
  )
)


;; This command seems to fine on the command line, but doesn't seem to work correctly
;; when running through emacs.  My solution is to disable the backup files, since I've
;; never used them.
;; -name \"*~\" -o

