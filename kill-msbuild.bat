;; Author: Tom Weiss
;; email:  weiss@cs.wisc.edu
;;
;; Created this batch file because Gnu Emacs isn't always able to kill MSBuild.exe
;; when the compilation was started from Emacs.  

taskkill /im msbuild.exe /f /t