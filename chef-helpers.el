
;; The incrementing can be done with replace-regexp with the following:
;; version\(.*\)'\([0-9]+\)\.\([0-9]+\)\.\([0-9]+\)' -> version\1'\2.\3.\,(1+ \#4)'
(defun plus-chef-cookbook-version ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "version\\(.*\\)'\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)'" nil t)
    (replace-match (concat "version"
                           (match-string 1) "'" ;; The spaces between version and single quote.
                           (match-string 2) "." ;; The first version and '.'.
                           (match-string 3) "." ;; The second version and '.'.
                           (int-to-string (+ 1 (string-to-int(match-string 4)))) "'"
                           )))
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