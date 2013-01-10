;;
;; Custom File Cache
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;
;; The purpose of this code is to make sure the window which contains the
;; electric-buffer-list is wide enough to show the complete file names.
;; In some cases the end of the name doesn't fit on the screen.  Once the
;; buffer width is 160 or greater the electric-buffer-list is displayed
;; in a buffer split vertically.

;; A new idea to explore is to use the commands (to be found) which widen
;; the current window, show the buffer list, and then narrow the width.
;; It would also be useful to see if the current window is as wide


;; split-height-threshold - This variable determines when display-buffer may split a window, if there
;; are multiple windows. display-buffer always splits the largest window if it has at least this
;; many lines. If the largest window is not this tall, it is split only if it is the sole window and
;; pop-up-windows is non-nil.

;; split-width-threshold - Setting a value of nil will ensure that the split will be horizontal.



(defun cebl ()
  (interactive)
  "tbd"
  (let ((orig-tbd split-height-threshold))
    ;; (message orig-tbd)
    ;;(setq split-height-threshold nil)
    ;; (message (concat "Saving split-height-threshold value of %d" orig-tbd))
    (message (int-to-string split-height-threshold))
    ;; (electric-buffer-list)
    ;; (call-interactively 'electric-buffer-list)
    ;;(setq split-height-threshold orig-tbd)
    ;;(message (concat "Setting split-height-threshold back to " split-height-threshold))
    )
)

;; Forces all splits to be horizontal.  It works great to cause the electric buffer list
;; to utilize the full width, but does not work as I intend when the screen is already
;; split in half vertically.
(setq split-width-threshold nil)
(setq split-width-threshold 80)

;; To view what a key is mapped to, run describe-key and provide the key.  It will
;; display what the key is mapped to.
;; C-x 1 is bound to the function delete-other-windows, which will close the other
;; windows.
;; C-x 3 is bound to split-window-horizontally

;; Return the name of the current buffer.
(buffer-name)

;; Seems to provide the same result as window-list.
(get-buffer-window-list nil t t)

;; Displays each window and the buffer inside each window.
(window-list)
;; This will return the number of windows I have open.
(length (window-list))

(window-tree)

;; This mode seems to allow me to go back to some previous setup.  That would be useful
;; except after I widen the electric buffer list I want to have at least one new buffer
;; open.
(winner-mode nil)

;; Displays available height.
(screen-height)

;; Displays available width.
(screen-width)

;; Displays the current's window's width.
(window-width)

;; This command will expand or shrink the current window.
(enlarge-window-horizontally NUMBER)
(call-interactively 'enlarge-window-horizontally)

(call-interactively 'shrink-window-horizontally)
