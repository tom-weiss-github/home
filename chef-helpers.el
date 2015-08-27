
;; The incrementing can be done with replace-regexp with the following:
;; version\(.*\)'\([0-9]+\)\.\([0-9]+\)\.\([0-9]+\)' -> version\1'\2.\3.\,(1+ \#4)'

(defun plus-chef-major-cookbook-version ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "version\\(.*\\)'\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)'" nil t)
    (replace-match (concat "version"
                           (match-string 1) "'" ;; The spaces between version and single quote.
                           ;; The major version *incremented* and '.'.
                           (int-to-string (+ 1 (string-to-int(match-string 2)))) "."
                           (match-string 3) "." ;; The minor version and '.'.
                           (match-string 4) "'" ;; The patch version and "'".
                           )))
)

(defun plus-chef-minor-cookbook-version ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "version\\(.*\\)'\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)'" nil t)
    (replace-match (concat "version"
                           (match-string 1) "'" ;; The spaces between version and single quote.
                           (match-string 2) "." ;; The major version and '.'.
                           ;; The minor version *incremented* and '.'.
                           (int-to-string (+ 1 (string-to-int(match-string 3)))) "."
                           (match-string 4) "'" ;; The patch version and "'".
                           )))
)

(defun plus-chef-patch-cookbook-version ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "version\\(.*\\)'\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)'" nil t)
    (replace-match (concat "version"
                           (match-string 1) "'" ;; The spaces between version and single quote.
                           (match-string 2) "." ;; The first version and '.'.
                           (match-string 3) "." ;; The second version and '.'.
                           ;; The patch version *incremented* and the "'".
                           (int-to-string (+ 1 (string-to-int(match-string 4)))) "'"
                           )))
)

(load-file "~/.emacs.d/find-best-root.el")

(defun ooc ()
  (interactive)
  (message buffer-file-name)
  ;; Look for '*/deploy/chef/cookbooks/<something>/*' in the path and message whether I found it and
  ;; if so what is something.
  (if (string-match ".*deploy/chef/cookbooks/.*" buffer-file-name)
      (progn (message "this is a cookbook")
             (setq cookbook (replace-regexp-in-string
                             "/.*" ""
                             (replace-regexp-in-string ".*/deploy/chef/cookbooks/" "" buffer-file-name)))
             (message (concat "The cookbook is '" cookbook "'."))
             (setq cookbook_metadata_fname (concat (find-best-root "makefile")
                                                   "deploy/chef/cookbooks/"
                                                   cookbook
                                                   "/metadata.rb"))
             (message (concat "The cookbook metadata file is "
                              cookbook_metadata_fname))
             (find-file cookbook_metadata_fname)
             (goto-char (point-min))
             (while (re-search-forward "version\\(.*\\)'\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)'" nil t)
               (replace-match (concat "version          '0.88.88'")))
             (write-file buffer-file-name)
             )
    (message "Oops, this file is not part of a cookbook."))
)

;; - Change some cookbook <CB>.
;; - Open its metadata.rb file (no change yet).
;; - Find all cookbooks that depend on <CB>.
;;     find-grep-dired .../cookbooks  depends.*<CB>
;; - Update each cookbook found with <CB>'s new version x.y.z.
;;   t (toggle all files selected)
;;   Q (query-replace-regexp)
;;   depends\(.*\)<CB>\(.*\)"=.*
;;   depends\1<CB>\2"= x.y.z"
;; - For each cookbook that was updated repeat to update its dependencies.
;; - Once all of the dependencies are updated, update the version of each cookbook with the increment-chef-cookbook-version program.
;; - Save with C-x s !