;;
;; Custom Compile
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;

(load-file "~/.emacs.d/find-best-root.el")

;; Cause compilation window to scroll with the output.
(setq compilation-scroll-output 'first-error)

;; Causes the compile function to execute the compile command
;; without prompting for user approval.
(setq compilation-read-command nil)


;; Possible Values: 'order' (order server), 'price' (price server)
(setq custom-compile-msbuild-project "order")
;; Possible Values: 'Build', 'Rebuild'
(setq custom-compile-msbuild-target "Build")
;; Possible Values: 'Debug', 'Release'
(setq custom-compile-msbuild-configuration "Debug")

(defun set-custom-compile-options (settings)
  (interactive "sEnter Custom Compile Settings (o|p|-  b|r|-  d|r|-): ")
  "Sets custom compile options. A dash keeps the existing value."
  (if (eq 3 (length settings) )
      (let ( (project (substring settings 0 1) )
             (target  (substring settings 1 2) )
             (config  (substring settings 2 3) ) )
        (progn
          (setq custom-compile-msbuild-project (cond ((string= "o" project) "order")
                                                     ((string= "p" project) "price")
                                                     (t custom-compile-msbuild-project) ) )

          (setq custom-compile-msbuild-target (cond ((string= "b" target) "Build")
                                                    ((string= "r" target) "Rebuild")
                                                    (t custom-compile-msbuild-target) ) )

          (setq custom-compile-msbuild-configuration (cond ((string= "d" config) "Debug")
                                                           ((string= "r" config) "Release")
                                                           (t custom-compile-msbuild-configuration) ) )

          (message (concat custom-compile-msbuild-target "ing " custom-compile-msbuild-project " server " custom-compile-msbuild-configuration) )
          )
        )
    (message (concat "Failed to set custom compile options, the input must be exactly 3 characters, received '"
                     settings "', which is " (number-to-string (length settings)) ".") )
    )
)

(defun create-lisp-env-file (windows-env-file lisp-env-file)
  "This method converts a Windows specific file which contains a collection
of environment variables and their values into a form which can be used to
by emacs lisp to set the environment variables.  These variables and values
are specific to each source tree."
  (setq the-file-path (concat (find-best-root windows-env-file)))
  (setq the-file-name (concat the-file-path windows-env-file))
  (setq the-buffer (create-file-buffer the-file-name))
  (switch-to-buffer-other-window the-buffer)
  (insert-file-contents the-file-name)
  (setq my-current-buffer (current-buffer))

  ;; Adds quotes to the value of the environment variable.  SET
  ;; TT_SMQ_VERSION=2.1 becomes SET TT_SMQ_VERSION="2.1").  The regular
  ;; expression in re-search-forward is anything after '=' to the end
  ;; of the line '$'.  The '.*' is put inside a group so that the
  ;; group can be used on the replace-match.  The replace then puts
  ;; a '=', whatever matched inside the 1st subexpression, and
  ;; a ')'.
  (goto-char (point-min))
  (while (re-search-forward "=\\(.*\\)$" nil t)
    (replace-match "=\"\\1\"\)" nil nil))

  ;; This replaces 'SET' with '(setenv'.
  ;; The 't' as second argument causes case insensitive.
  (goto-char (point-min))
  (while (search-forward "SET" nil t)
    (replace-match "(setenv" t t))

  (goto-char (point-min))
  (flush-lines "^@echo off")

  ;; This removes the lines which begin with 'IF NOT DEFINED'.
  (goto-char (point-min))
  (flush-lines "^IF NOT DEFINED.*")

  ;; This removes the lines which begin with 'IF DEFINED'.
  (goto-char (point-min))
  (flush-lines "^IF DEFINED.*")

  ;; This adds quotes around the environment variables which are of
  ;; the form TT_*_VERSION.  Setting the 3rd argument of replace-match
  ;; to nil causes the 1st argument to be treated specially if a '\'
  ;; is found, in my case I wish to surround what was matched with
  ;; quotes.
  (goto-char (point-min))
  (while (re-search-forward "\\(TT_.*\\)=" nil t)
    (replace-match "\"\\1\"=" nil nil))
  ;; Previous version, just keeping it around for a while, this
  ;; older version only kept the envs which ended in version,
  ;; but I learned I need all of them.
  ;; (while (re-search-forward "TT_.*_VERSION" nil t)
  ;;   (replace-match "\"\\&\"" nil nil))


  (goto-char (point-min))
  (while (search-forward "=" nil t)
    (replace-match " " nil t))

  ;; Remove all the lines which have the pattern.
  (goto-char (point-min))
  (flush-lines ".*setenv TT_.*$")

  ;; Remove all the lines which have the pattern.
  (goto-char (point-min))
  (flush-lines ".*PRODVRM.*")

  ;; I need the environment variables with paths to have
  ;; double backslashes.
  (goto-char (point-min))
  (while (search-forward "\\" nil t)
    (replace-match "\\\\\\\\" nil nil))

  ;; This works, but leaves a blank line.  I switched to flush-lines
  ;; instead.  Kept it for reference.
  ;; (goto-char (point-min))
  ;; (while (re-search-forward ".*setenv TT_.*$" nil t)
  ;;   (delete-region (line-beginning-position) (line-end-position)))

  (write-file (concat the-file-path lisp-env-file))
  (kill-buffer the-buffer)
  ;; Do M-x describe-bindings to see the functions for the key bindings.
  ;; Great if you know the key stroke, but not the lisp function.
  (call-interactively 'other-window)            ;; C-x o
  (delete-other-windows)                        ;; C-x 1

  (concat the-file-path lisp-env-file)
  )
;; (create-lisp-env-file "SetEnv.cmd" "SetEnv.lisp")


(defun eval-build-envs (lisp-env-full-path)
  "This method sets environment variables which are contained in a
file.  These variables and their values are specific to each source
tree root and must be set dynamically since the files from multiple
source trees may be open at the same time and compilation should use
only the variables and values for that file's source tree."
  (setq the-buffer (create-file-buffer lisp-env-full-path))
  (switch-to-buffer-other-window the-buffer)
  (insert-file-contents lisp-env-full-path)
  (setq my-current-buffer (current-buffer))

  (eval-buffer nil)

  (kill-buffer the-buffer)
  ;; Do M-x describe-bindings to see the functions for the key bindings.
  ;; Great if you know the key stroke, but not the lisp function.
  (call-interactively 'other-window)            ;; C-x o
  (delete-other-windows)                        ;; C-x 1

  (message (concat "Evaluated environment variables from " lisp-env-full-path))
)
;; (eval-build-envs "h:/.emacs.d/SetEnv.lisp")

(defun find-vs-proj ()
  "This method sets the project to build based on environment variables and other
input."
  ;; Since I just set the SetEnv.lisp environment variables, I can use their
  ;; values to figure out what to build.
  (let ( (vs-proj "")
         (gw-version "unset")
         (gw-name "unset")
         (gw-vcxproj "unset")
         (btec-version (getenv "TT_BTEC_VERSION"))
         (ose-version (getenv "TT_OSE_VERSION"))
         (sgx-version (getenv "TT_SGX_VERSION"))
         (tocom-version (getenv "TT_TOCOM_VERSION")) )

    (setq vs-proj (concat vs-proj (getenv "TT_DEV_HOME")))
    (setq vs-proj (replace-regexp-in-string "\\\\" "/" vs-proj))
    (setq vs-proj (concat vs-proj "/gw"))

    (if (not (equal btec-version nil))
        (progn
          (setq gw-name "/btec")
          (setq gw-version btec-version)
          (if (string= "order" custom-compile-msbuild-project)
              (setq gw-vcxproj "btec_orders.vcxproj")
            )
          (if (string= "price" custom-compile-msbuild-project)
              (setq gw-vcxproj "btec_prices.vcxproj")
            )
          )
      )

    (if (not (equal ose-version nil))
        (progn
          (setq gw-name "/ose")
          (setq gw-version ose-version)
          (if (string= "order" custom-compile-msbuild-project)
              (setq gw-vcxproj "ose_orders.vcxproj")
            )
          (if (string= "price" custom-compile-msbuild-project)
              (setq gw-vcxproj "ose_prices.vcxproj")
            )
          )
      )

    (if (not (equal sgx-version nil))
        (progn
          (setq gw-name "/sgx")
          (setq gw-version sgx-version)
          (if (string= "order" custom-compile-msbuild-project)
              (setq gw-vcxproj "sgx_orders.vcxproj")
            )
          (if (string= "price" custom-compile-msbuild-project)
              (setq gw-vcxproj "sgx_prices.vcxproj")
            )
          )
      )

    (if (not (equal tocom-version nil))
        (progn
          (setq gw-name "/tocom")
          (setq gw-version tocom-version)
          (if (string= "order" custom-compile-msbuild-project)
              (setq gw-vcxproj "tocom_orders.vcxproj")
            )
          (if (string= "price" custom-compile-msbuild-project)
              (setq gw-vcxproj "tocom_prices.vcxproj")
            )
          )
      )

    (setq vs-proj (concat vs-proj gw-name))
    (setq vs-proj (concat vs-proj "/"))
    (setq vs-proj (concat vs-proj gw-version))
    (setq vs-proj (concat vs-proj "/dev"))

    (if (string= "order" custom-compile-msbuild-project)
        (setq vs-proj (concat vs-proj "/orders"))
      )
    (if (string= "price" custom-compile-msbuild-project)
        (setq vs-proj (concat vs-proj "/prices"))
      )

    (setq vs-proj (concat vs-proj "/"))
    (setq vs-proj (concat vs-proj gw-vcxproj))

    vs-proj
    )
)
;; (find-vs-proj)


(defun gw-compile ()
  "Create a custom command to compile the Visual Studio Project based on the current
buffer, some user settings, and the source tree that the file lives in."
  (interactive)
  (setq file-name-cmd "SetEnv.cmd")
  (setq file-name-lisp "SetEnv.lisp")

  (setq windows-env-file (concat (find-best-root file-name-cmd) file-name-cmd))
  (setq lisp-env-file (concat (find-best-root file-name-cmd) file-name-lisp))

  (if (file-exists-p windows-env-file)
      (progn
        (if (file-exists-p lisp-env-file)
            (message (concat lisp-env-file " already exists.")) ;; This will go to the message window.
          (create-lisp-env-file file-name-cmd file-name-lisp))

        ;; Reset these environment variables because they are used to determine which gateway the
        ;; current buffer is supposed to build.  However, if a single emacs instance is building
        ;; more than one, each of them might be set.  After evaluating the lisp environment file
        ;; the correct one will be set again.
        (setenv "TT_BTEC_VERSION" nil)
        (setenv "TT_OSE_VERSION" nil)
        (setenv "TT_SGX_VERSION" nil)
        (setenv "TT_TOCOM_VERSION" nil)
        (eval-build-envs lisp-env-file)

        (setq my-compile-command "c:/Windows/Microsoft.NET/Framework/v4.0.30319/MSBuild.exe ")
        (setq my-compile-command (concat my-compile-command (find-vs-proj)))
        (setq my-compile-command (concat my-compile-command " /target:" custom-compile-msbuild-target))
        (setq my-compile-command (concat my-compile-command " /property:Configuration=" custom-compile-msbuild-configuration))

        (setq compile-command my-compile-command)
        (call-interactively 'compile)
        my-compile-command)
    (message "I don't know how to compile this file.")
    )
)
;; (gw-compile)


(setq d-compile-target "invalid")

(defun d-set-compile-target (target)
 "Set the compile target."
 (interactive "sEnter Target(s) or ENTER to view: ")
 (if (string= "" target)
     (message (concat "Target remains '" d-compile-target "'."))
   (progn
     (setq d-compile-target target)
     (message (concat "Target changed to '" d-compile-target "'."))))
)

(defun d-compile ()
  "Compile using existing target."
  (interactive)
  (setq d-marker-file "makefile")
  (setq d-dev-root (find-best-root d-marker-file))
  (setq d-marker-abs-path (concat d-dev-root d-marker-file))

  (message d-marker-abs-path)

  (if (file-exists-p d-marker-abs-path)
      (progn
        (setq d-compile-command "make -C ")
        (setq d-compile-command (concat d-compile-command d-dev-root))
        (setq compile-command (concat d-compile-command " " d-compile-target))
        (call-interactively 'compile)
        (message compile-command))
    (message "I don't know how to compile this file.")
    )
)





