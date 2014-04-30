;; Author: Tom Weiss
;; Email:  weiss@cs.wisc.edu
;;
;; This file is not part of GNU Emacs, duh.

;; This forces emacs to save files in UNIX format.
;;(global-set-key (quote[f5]) (set-buffer-file-coding-system (quote undecided-unix) nil))


;; Should force emacs to display line numbers.
(line-number-mode 1)

;; Map F1 to open the electric buffer list, perhaps wider than normal.
(load-file "~/.emacs.d/custom-electric-buffer-list.el")
(if (eq system-type 'gnu/linux)
    (global-set-key (quote[f1]) (quote custom-electric-buffer-list))
  (global-set-key (quote[f1]) (quote electric-buffer-list))
)

(if (eq system-type 'gnu/linux)
    (load-file "~/.emacs.d/custom-gdb-functions.el")
)

(load-file "~/.emacs.d/chef-helpers.el")

;; Load Scala on linux.
(if (eq system-type 'gnu/linux)
    (progn (add-to-list 'load-path "~/.emacs.d/scala-mode")
           (require 'scala-mode-auto)))

;; map f2 to goto line number
(global-set-key (quote[f2]) (quote goto-line))

;; Map f11/f12 to set background/foreground: this will prompt for
;; the color on the bottom.
(global-set-key (quote[f11]) (quote comment-region))
(global-set-key (quote[f12]) (quote uncomment-region))

;; Map C-x C-f to set the correct file cache and then call open-file.
(load-file "~/.emacs.d/custom-file-cache.el")
(global-set-key (kbd "C-x C-f") (quote d-find-file-with-custom-file-cache))

;; This might be a better way to change both colors with one button.
;;(global-set-key [f9]
;;                '(lambda ()
;;                   (interactive)
;;                   (set-background-color "white")
;;                   (set-foreground-color "black")))

;; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(fset 'yes-or-no-p 'y-or-n-p)


;; To set a buffer to be unix
;; ESC-x set-buffer-file-coding-system  RET  emacs-mule-unix

;; To set a buffer to be dos
;; ESC-x set-buffer-file-coding-system  RET  emacs-mule-dos

;; Remove the menu bar.
(menu-bar-mode 0)

;; For further experimentation:
;; Move between windows with shift+arrows.
;; (windmove-default-bindings)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Remove the tool bar if not running inside a terminal.
(if window-system
    (tool-bar-mode 0))


;; only use spaces for indenting, no tabs
(setq-default indent-tabs-mode nil)
;; use tabs for indenting
;;(setq-default indent-tabs-mode t)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; The lines-tail option highlights columns that go beyond
;; the variable whitespace-line-column, whose default value
;; is 80.  To also see trailing whitespace, do (lines-tail trailing).
;; Setting the modes to c-mode and c++-mode means whitespace
;; mode only takes effect in those buffers.
;; Turned this off for now since too many lines exceed 80
;; characters and no strict enforcement is in place.
;; (require 'whitespace)
;; (setq whitespace-style '(lines-tail))
;; (global-whitespace-mode t)
;; (setq whitespace-global-modes '(c-mode c++-mode))
;; (defun my-c-mode-setup ()
;;   (set-buffer-file-coding-system 'utf-8-unix nil 1)
;;   (global-whitespace-mode t)
;; )
;;(add-hook 'c-mode-hook 'my-c-mode-setup)

;; File Cache
;; To be explored further.  Doing M-tab when opening
;; a file will look in the cache.
;; (file-cache-add-directory-recursively "directory")
;; (file-cache-add-directory-recursively "some/path/")

;; auto-magic copyright updates
(load-library "copyright")
(add-hook 'write-file-hooks 'copyright-update)

;; These are some packages which allow for cygwin paths in gnu emacs
;; and windows paths in cygwin emacs.  The cygwin-mount.el and
;; windows-path.el must be in the $HOME/.emacs.d directory.
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin) )
    (progn (add-to-list 'load-path "~/.emacs.d")
           (require 'windows-path)
           (windows-path-activate)))

;; Load the MKS commands.  The ~ is the %HOME%/$HOME
;; environment variable (available via (getenv "HOME")).
(if (eq system-type 'windows-nt)
    (progn (load-file "~/.emacs.d/mks-commands.el")))


;; Customizing ask-user-about-supersession-threat because whenever
;; I check out a file, my function changes it to writable and this
;; causes ask-user-about-supersession-threat to notice it changed
;; on disk.
(defun ask-user-about-supersession-threat (fn)
  (message "Ignoring that the current buffer changed on disk.")
  )

;; Load my custom find grep.
(load-file "~/.emacs.d/custom-find-grep.el")
(global-set-key (quote[f3]) (quote set-fg-suffix))
(global-set-key (quote[f4]) (quote fg))

;; Was experimenting with these two but never had a good use for them,
;; hence they haven't been added to any keyboard binding.
(defun select-next-window ()
  "Switch to the next window."
  (interactive)
  (select-window (next-window))
)
(defun select-previous-window ()
  "Switch to the previous window."
  (interactive)
  (select-window (previous-window))
)

;; Load my custom compile commands.
(load-file "~/.emacs.d/custom-compile.el")
(if (eq system-type 'gnu/linux)
    (global-set-key (quote[f7]) (quote d-compile))
  (global-set-key (quote[f7]) (quote gw-compile)))

(if (eq system-type 'gnu/linux)
    (global-set-key (quote[f8]) (quote d-set-compile-target))
  (global-set-key (quote[f8]) (quote set-custom-compile-options)))

(global-set-key (quote[f9]) (quote next-error))

;; Disable backup and autosave.  The motivation was false positives
;; in searches when a string is found in the file.txt~ file.
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Disable the warning about large files.
(setq large-file-warning-threshold nil)

;; Load my custom TAGS commands.
(load-file "~/.emacs.d/custom-tags.el")
(global-set-key (quote[f5]) (quote set-my-tag-file-and-search))

;; Load my custom find and open.
(load-file "~/.emacs.d/find-and-open-file.el")

;; Dimensions & Position
;; =====================
(if window-system
    (progn (set-frame-height (selected-frame) 80)
           (set-frame-width (selected-frame)  120)))
;; (setq default-frame-alist
;;       '((top . 5) (left . 3)
;;         (width . 220) (height . 63)))

(setq ruby-indent-level 4)

;; C++ Stuff
;; =========

;; Load C++ mode for header files.
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c++-mode) auto-mode-alist))
(setq c-basic-offset 4)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq comment-column 80)
             (c-set-style "ellemtel")           ; set indentation style
	     (c-set-offset 'defun-block-intro 4 nil)
	     (c-set-offset 'statement-block-intro 4 nil)
	     (c-set-offset 'comment-intro 0 nil)
	     (c-set-offset 'case-label 4 nil)
	     (c-set-offset 'brace-list-intro 4 nil)
	     (c-set-offset 'topmost-intro 0 nil)
	     (c-set-offset 'innamespace 0 nil)
             (c-set-offset 'inclass 4 nil)
	     ;;(c-set-offset 'arglist-cont-nonempty c-lineup-arglist nil)
             ))

;; topmost-intro - This seems to control indent inside a namespace, and
;; was producing 3 spaces, but I want 4, so I added one.

;; Changes the default column width used when invoking fill-paragraph,
;; bound to M-q.
(setq-default fill-column 100)

;; c-set-style choices:
;; gnu, k&r, bsd, whitesmith, stroustrup, ellemtel, linux, python, java
;; If the indent is not working, go to the bad line, and do C-c C-o
;; this will say what is controlling the indent, and then it can be
;; changed above.

;; C-c C-s will display the syntactic information of a line.
;; http://www.xemacs.org/Documentation/packages/html/cc-mode_10.html#SEC38
;; This URL is the complete list of syntactic symbols.

;; useful links
;; http://www.xemacs.org/Links/tutorials_1.html
;; http://infolab.stanford.edu/~manku/dotemacs.html

;; End C++ Stuff

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

(setq inhibit-splash-screen t)

;; The following command is useful to execute (C-x C-e) inside the
;; scratch buffer to list all the available fonts.
;; (insert (prin1-to-string (x-list-fonts "*")))
;; Another useful font:
;; (setq my-os-dependent-font "Monospace-14")
(setq my-os-dependent-font (if (eq system-type 'windows-nt)
                               "Source Code Pro"
                             "-misc-fixed-medium-r-normal--25-*-75-75-c-90-iso8859-1" ))

(cond (window-system
       (set-face-background 'default "black")
       (set-face-foreground 'default  "skyblue")
       ;;(set-face-background 'zmacs-region "green") ; When selecting w/ mouse
       ;;(set-face-foreground 'emacs-region "black")
       (set-face-font       'default      my-os-dependent-font)
       (set-cursor-color "red")
       (set-mouse-color "green")
       (set-face-background (quote modeline) "thistle4")
       (set-face-foreground (quote modeline) "black")
       ;;(set-face-background (quote region) "green4")
       (setq hilit-mode-enable-list '(not text-mode)
             hilit-background-mode   'dark
             hilit-inhibit-hooks     nil
             hilit-inhibit-rebinding nil)
       ))

;; Regarding cursor color.  When I moved to 24.1, set-cursor-color in this context
;; was no longer effective.  In order to get the cursor to be red, I did describe-function
;; on set-cursor-color, and then 'customize this face', which allowed me to set red ad
;; the background.  I found the below example and this seems to work.
(custom-set-faces
 '(cursor ((t (:background "red" :width extra-expanded)))))

;; inl file should be c++ code
(setq auto-mode-alist (cons '("\\.inl$" . c++-mode) auto-mode-alist))

(setq ediff-split-window-function 'split-window-horizontally)

;; Turn on colors by default.  To toggle colors for a specific
;; buffer, do M-x font-lock-mode.
(require 'font-lock)

;; set java hook
;;(add-hook 'java-mode-hook 'turn-on-font-lock)

;;set the title and task bar icon to contain the name of the file that is
;;being edited
(setq frame-title-format "emacs %b")
(setq icon-title-format  "emacs %b")

;;comment one line (to use, esc-x c return)
(fset 'c
   [?\C-a ?/ ?* ?\C-e ?* ?/ tab])


;;uncomment one line (to use, esc-x u return)
(fset 'u
   [?\C-a tab ?\C-d ?\C-d ?\C-e ?\C-b ?\C-b ?\C-d ?\C-d tab])

(fset 'p
   [?/ ?* escape ?f ?* ?/])


(fset 'i
   ".c_str()")

;; Start emacs server.  If the error 'The directory ... is unsafe.' happens, go
;; to the directory and change the owner from Administrator to your user account.
(if window-system
    (server-start))

;; Prevent prompting about emacsclientw still waiting for file to be edited when killing buffer.
;; This must be done after starting the server.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Turn on the mode that automatically show parenthesis matching.
(show-paren-mode)
;; Use M C-f if on a beginning paren to find the end paren.
;; Use M C-b if on an end paren to find the beginning paren.
;; M-x show-paren-mode will toggle mode on an off.

;; Highlights the region in parenthesis.
;;(paren-set-mode 'sexp)
;; (forward-sexp) moves to the next matching parenthesis.
;; (backward-sexp) moves to the previous matching parenthesis.

;; Causing the matching parenthesis to blink, forever.
;;(paren-set-mode 'blink-paren)

;; I am picky about what colors to use when matching parens.
(set-face-foreground 'show-paren-match-face "Red")
(set-face-background 'show-paren-match-face "Black")

(set-face-foreground 'show-paren-mismatch-face "Red")
(set-face-background 'show-paren-mismatch-face "Black")

(put 'scroll-left 'disabled nil)

;; JUMP TO MATCHING PAREN: When standing _on_ paren, press '%' (shift-5)
;;(global-set-key "%" 'match-paren)
;;(defun match-paren (arg)
;;  "Go to the matching parenthesis if on parenthesis otherwise insert %."
;;  (interactive "p")
;;  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;        (t (self-insert-command (or arg 1)))))

;; This should set the 'backspace' key to behave the same
;; in emacs when it is in its own X window and when it is
;; started in -nw mode.  Without this, when emacs is started
;; in -nw mode, the 'backspace' key brings up some help command
;;(global-set-key "^H" (quote delete-backward-char))
;;(global-set-key [delete] 'delete-char)
;;(global-set-key [kp-delete] 'delete-char)
;;(global-set-key [backspace] 'delete-char)
;;(normal-erase-is-backspace-mode nil)

;; Debugging with GDB
;; M-x gdb
;; Watching a variable: move point to variable and do M-x gud-watch
;; the expression is watched in the speedbar.
;; gdb-show-changed-values to non-nil highlights recently changed
;; values.
;; gdb-speedbar-auto-raise to non-nil raises the speedbar everytime
;; a watched expression changes.


;; Useful emacs commands.
;; To create and save a keyboard macro in emacs.
;; C-x '('   type macro now    C-x ')'
;; ESC-x name-last-kbd-macro   name here
;; ESC-x insert-kbd-macro      prints the macro, do this in the .emacs file
;; ESC-x [macro name]   executes the named keyboard macro


;; Tail a file:
;; auto-revert-tail-mode

;; repeat n times: ctrl-u <number of times> command

;; To copy and paste a rectange:
;;
;; set mark at upper left
;; move to lower right
;; Esc-x kill-rectangle
;; move to where you want it pasted
;; Esc-x yank-rectangle

;; Bookmark commands
;; Not sure why m, b, and l are the commands, so I call them make, bounce, and list to remember.
;; C-x r m Make a bookmark at current cursor position.
;; C-x r b Bounce to a bookmark.
;; C-x r l List bookmarks.

;; To change read-write:
;; toggle-read-only

;; Get help on some function
;; C-h f (describe function)

;; Regular Expressions
;; M-x re-builder is an interactive regular expression builder.
;; http://www.xemacs.org/Documentation/21.5/html/xemacs_15.html#SEC125 - Has the regular expression syntax.
;; M C-s  --> regular expression forward incremental search (isearch-forward-regexp).
;; example M C-s .*text
;; example of multiline regular expression search of:
;; MessageType: J
;; MessageBodyLength: 52
;; MarketID: 197140
;;
;; sv_.*[0123456789]\{8\}.*
;; (Above was used to find log message helpers, starts with sv_, then an 8 digit number.)
;;
;; do M C-s  MessageType: JC-qC-jMessage.*C-qC-jMarketID: 197140
;; The 'C-qC-j' part is the newline.
;; M-x replace-regexp --> regular expression search-replace.
;; example of regular expression search-replace:
;; M-x replace-regexp TEXT[ ]* <enter> ' = ' (that's space equals space)

;; replace-regexp example:
;; had:
;; <port>1</port>
;; <port>2</port>
;; ...
;; <port>N</port>
;; wanted:
;; <port>1</port>
;; <tag>1</tag>
;; <port>2</port>
;; <tag>2</tag>
;; ...
;; <port>N</port>
;; <tag>N</tag>
;; <port>\([0-9]+\)</port>
;;         group 1 in parenthesis
;; <port>\1</port>C-qC-j<tag>\1</tag>
;;       \1 means what was matched in group 1
;;       C-qC-j is newline (CTRL+q, CTRL+j)

;; Toggle line wrap
;; M-x toggle-truncate-lines

;; Delete rectangle:
;; set mark, move point, do M-x delete-rectangle (or kill-rectangle)
;; to paste, go to spot and do M-x yank-rectangle

;; To remove ^M characters, use CTRL-Q CTRL-M to specify the ^M character.

;; Replace regular expression across file:
;; find-grep-dired
;; t (toggle all) or m (mark individual)
;; Q (query-replace-regexp)
;; (proceed with query-replace-regexp syntax)
;; (C-x s !) to save all buffers modified.

