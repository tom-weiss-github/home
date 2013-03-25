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

;; Useful Commands in the Grep Window (which is in Compilation Mode):
;; M-g n: Move and visit next error message or match.  I map to F9.
;; M-g p: Move and visit prevous erroor message or match.
;; M-n: Move to next error message or match, but don't visit.
;; M-p: Move to previous error message or match, but don't visit.

(load-file "~/.emacs.d/find-best-root.el")

(if (eq system-type 'windows-nt)
    (progn (setq find-program "c:/cygwin/bin/find.exe")))


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

;; Since this is going to traverse up to find the root of this source tree, I need to provide some
;; marker file to denote when the root is reached.  This file may be different on different platforms.
(if (eq system-type 'windows-nt)
    (progn (setq marker-file "manifest.json")
           (setq find-filters " -type f         -path \"*/misc/xerces\" -prune -o -path \"*/misc/boost\" -prune -o -path \"*/misc/xmlhelp\" -prune -o -path \"*/misc/ace\" -prune -o -path \"*/misc/cppunit\" -prune -o -path \"*/misc/performance_logger\" -prune -o -path \"*/misc/pfx\" -prune -o -path \"*/misc/unittestframework\" -prune -o \"(\" -not -name TAGS -and -not -regex \".*log\" -and -not -regex \".*idb\" -and -not -regex \".*pch\" -and -not -regex \".*sbr\" -and -not -regex \".*ipch\" -and -not -regex \".*zip\" -and -not -regex \".*xml\" -and -not -regex \".*exe\" -and -not -regex \".*dll\" -and -not -regex \".*manifest\" -and -not -regex \".*csv\" -and -not -regex \".*ilk\" -and -not -regex \".*bsc\" -and -not -regex \".*map\" -and -not -regex \".*sdf\" -and -not -regex \".*vsd\" -and -not -regex \".*lib\" -and -not -regex \".*obj\" -and -not -regex \".*pdb\" \")\"  -print0 | xargs -0 grep -nHi -e ")
           ))

(if (eq system-type 'gnu/linux)
    (progn (setq marker-file "makefile")
           (setq find-filters (concat " -type d "
                                      "-path \"*/build\" -prune -o "
                                      "-path \"*/.git\" -prune -o "
                                      "-path \"*/ext\" -prune -o "
                                      "-path \"*/pycommon\" -prune -o "
                                      "\"(\" "
                                      "\! -iname \"BROWSE\" "
                                      "-and \! -iname \"FILES\" "
                                      "-and \! -iname \"TAGS\" "
                                      "-and \! -iname \"*.a\" "
                                      "-and \! -iname \"*.bin\" "
                                      "-and \! -iname \"*.cs\" "
                                      "-and \! -iname \"*.css\" "
                                      "-and \! -iname \"*.d\" "
                                      "-and \! -iname \"*.dat\" "
                                      "-and \! -iname \"*.html\" "
                                      "-and \! -iname \"*.ico\" "
                                      "-and \! -iname \"*.jar\" "
                                      "-and \! -iname \"*.json\"  "
                                      "-and \! -iname \"*.o\" "
                                      "-and \! -iname \"*.pdf\" "
                                      "-and \! -iname \"*.php\" "
                                      "-and \! -iname \"*.png\" "
                                      "-and \! -iname \"*.pyc\" "
                                      "-and \! -iname \"*.so\" "
                                      "-and \! -iname \"*.sql\" "
                                      "-and \! -iname \"*.txt\" "
                                      "-and \! -iname \"*.xml\" "
                                      "\")\" "
                                      "-print0 | xargs -0 grep -nHi -e "))
           ))

(setq custom-find-grep-path-suffix "")
(defun set-fg-suffix (suffix)
  "Set an optional suffix for the search.  This is useful for more fine grained searching."
  (interactive "sEnter Search Suffix: ")
  (if (string= "" suffix)
      (message "The optional search suffix is now empty.")
    (message (concat "The optional search suffix is now '" suffix "'."))
    )
  (setq custom-find-grep-path-suffix suffix)
)

;; The -print0 in find causes it to put NULLs at the end of the file names (which helps with spaces in the names).  The -0 in
;; xargs then uses the NULL instead of spaces.  This particular method solves a problem the previous two suffer from.  I noticed
;; that if a file with an extension I want to filter out was in the same directory as the file which was open, then find would
;; return an error.
(defun fg ()
  "A custom find grep that dynamically sets the search path based on the buffer.
Regular Expression Examples:
-E \"struct.*hash\"  When using special characters, enclose regexp in quotes.
-E \"^text$\"        ^ Matches beginning of line,  $ matches end of line.
-E \"main\\(.*\\)\"  .* Matches everything, parenthesis require escaping."
  (interactive)
  (let ((fg-tt-filters find-filters ))
    (setq my-find-grep-command "find \"")
    (setq my-find-grep-command (concat my-find-grep-command (find-best-root marker-file)))
    (setq my-find-grep-command (concat my-find-grep-command custom-find-grep-path-suffix))
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

