;;
;; Custom Electric Buffer List
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;
;; The purpose of this code is to make sure the window which contains the
;; electric-buffer-list is wide enough to show the complete file names.
;; In some cases the end of the name doesn't fit on the screen.  Once the
;; buffer width is 160 or greater the electric-buffer-list is displayed
;; in a buffer split vertically.

;; split-height-threshold - This variable determines when display-buffer may split a window, if there
;; are multiple windows. display-buffer always splits the largest window if it has at least this
;; many lines. If the largest window is not this tall, it is split only if it is the sole window and
;; pop-up-windows is non-nil. Setting a value of nil will ensure that the split will be horizontal.

;;(message (number-to-string emacs-major-version))
;;(message (number-to-string emacs-minor-version))

;; If 23.4 or newer, then use display-pixel-width, else use screen-width.
;; (if (and (>= emacs-major-version 24) (>= emacs-minor-version 3))
;;     (screen-width)
;;   (display-pixel-width))


(defun custom-electric-buffer-list ()
  (interactive)
  "Make sure the electric buffer list's window is wide enough."
  ;; Determine if the current window is taking up the entire width.
  (if (eq (window-width) (screen-width))
      (progn
        (message "Current window is full width, force electric buffer list window to be full width.")
        (let ((temp-split-width-threshold split-width-threshold ))
          (setq split-width-threshold nil)

          ;; Setting inhibit-quit to a non-nil value ensure that if the user does C-q during
          ;; the invocation of electric-buffer-list, control will return so that the window
          ;; can be shrunk again.
          (let ((inhibit-quit t))
            (call-interactively 'electric-buffer-list)
            )

          (setq split-width-threshold temp-split-width-threshold)
          )
        )
    (progn
      (message (concat "Current window is " (int-to-string (window-width)) " of " (int-to-string (screen-width)) ", let's widen."))
      ;; The amount to widen is 3/4 of the screen width minis the window with.  That is,
      ;; use 3/4 of the other widow for the extra space for the electric buffer list.
      (setq amount-to-widen (truncate (* .75 (- (screen-width) (window-width)))))
      (enlarge-window-horizontally amount-to-widen)

      ;; Setting inhibit-quit to a non-nil value ensure that if the user does C-q during
      ;; the invocation of electric-buffer-list, control will return so that the window
      ;; can be shrunk again.
      (let ((inhibit-quit t))
        (call-interactively 'electric-buffer-list)
        )

      (shrink-window-horizontally amount-to-widen)
      )
    )
)


;; What follows are the notes I created as I explored how to solve this problem.  I left
;; for future reference.

;; Forces all splits to be horizontal.  It works great to cause the electric buffer list
;; to utilize the full width, but does not work as I intend when the screen is already
;; split in half vertically.
;; (setq split-width-threshold nil)
;; (setq split-width-threshold 80)

;; To view what a key is mapped to, run describe-key and provide the key.  It will
;; display what the key is mapped to.
;; C-x 1 is bound to the function delete-other-windows, which will close the other
;; windows.
;; C-x 3 is bound to split-window-horizontally

;; Return the name of the current buffer.
;; (buffer-name)

;; Seems to provide the same result as window-list.
;; (get-buffer-window-list nil t t)

;; Displays each window and the buffer inside each window.
;; (window-list)
;; This will return the number of windows I have open.
;; (length (window-list))

;; (window-tree)

;; This mode seems to allow me to go back to some previous setup.  That would be useful
;; except after I widen the electric buffer list I want to have at least one new buffer
;; open.
;; (winner-mode nil)

;; Displays available height.
;; (screen-height)

;; Displays available width.
;; (screen-width) - This was not available on all Windows emacs.
;; (display-pixel-width) - This is available on Windows, but gives a very different
;;                         value for emacs in the console than its own window.
;; (display-pixel-height)

;; Displays the current's window's width.
;; (window-width)

;; This command will expand or shrink the current window.
;; (enlarge-window-horizontally 20)
;; (call-interactively 'enlarge-window-horizontally)

;; (shrink-window-horizontally 20)
;; (call-interactively 'shrink-window-horizontally)
