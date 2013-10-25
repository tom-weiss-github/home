
;; The incrementing can be done with replace-regexp with the following:
;; version\(.*\)'\([0-9]+\)\.\([0-9]+\)\.\([0-9]+\)' -> version\1'\2.\3.\,(1+ \#4)'
(defun increment-chef-cookbook-version ()
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