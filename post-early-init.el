;;; -*- lexical-binding: t; -*-

;; Scrolling behaviour
(setq-default scroll-preserve-screen-position t
              scroll-conservatively 1 ; affects scroll-step'
              scroll-margin 0
              next-screen-context-lines 0)

(repeat-mode 1)
(global-font-lock-mode 1)

(setq-default ;; Use setq-default to define global default
 ;; Who I am
 user-mail-address "gtkndcbfhr@gmail.com"
 user-full-name "Nestor Liao"
 ;; Enable all disabled commands
 disabled-command-function nil
 word-wrap-by-category t
 ;; unsafe theme
 custom-safe-themes t
 ;; Enable recursive minibuffer edit
 enable-recursive-minibuffers t
 ;; Don't show scratch message, and use fundamental-mode for *scratch*
 ;; Remove splash screen and the echo area message
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message 'nil
 initial-major-mode 'fundamental-mode
 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 indent-tabs-mode nil ; don't use tabs to indent
 tab-width 4 ; but maintain correct appearance
 ;; Use one space as sentence end
 sentence-end-double-space 'nil
 ;; Newline at end of file
 require-final-newline t
 ;; Don't adjust window-vscroll to view tall lines.
 auto-window-vscroll nil
 ;; Don't create lockfiles.
 ;; recentf frequently prompts for confirmation.
 create-lockfiles nil
 ;; Leave some rooms when recentering to top, useful in emacs ipython notebook.
 recenter-positions '(middle 1 bottom)
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show column number
 column-number-mode t
 ;; Don't break lines for me, please
 truncate-lines t
 ;; More message logs
 message-log-max 16384
 ;; Don't prompt up file dialog when click with mouse
 use-file-dialog nil
 ;; Place all auto-save files in one directory.
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; more useful frame title, that show either a file or a
 ;; buffer name (if the buffer isn't visiting a file)
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; Don't create backup files
 make-backup-files nil ; stop creating backup~ files
 ;; Remember my location when the file is last opened
 ;; activate it for all buffers
 save-place-file (expand-file-name "saveplace" user-emacs-directory)
 ;; smooth scrolling
 scroll-conservatively 101
 ;; Reserve one line when scrolling
 ;; scroll-margin 1
 ;; turn off the bell
 ring-bell-function 'ignore
 ;; Smoother scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 scroll-step 1 ;; keyboard scroll one line at a time
 view-read-only t ;; make read-only buffers in view mode
 ;; Native comp
 package-native-compile t
 comp-async-report-warnings-errors nil
 ;; Ignore 'ad-handle-definition' redefined warnings
 ad-redefinition-action 'accept
 ;; mouse auto follow
 mouse-autoselect-window t
 focus-follow-mouse 'auto-raise
 ;; Hide warnings and display only errors
 warning-minimum-level :error
 ;; Display of line numbers in the buffer:
 display-line-numbers-mode 1
 ;; Track changes in the window configuration, allowing undoing actions such as
 ;; closing windows.
 winner-mode 1
 ;; Replace selected text with typed text
 delete-selection-mode 1
 ;; Configure Emacs to ask for confirmation before exiting
 confirm-kill-emacs 'y-or-n-p
 ;; Paren match highlighting
 show-paren-mode 1
 ;; Display the time in the modeline
 display-time-mode -1
 dictionary-server "localhost"
 browse-url-firefox-program "firefox-beta"
 dired-movement-style 'cycle
 line-number-mode t
 column-number-mode t
 mode-line-position-column-line-format '("%l:%C")
 isearch-allow-scroll t
 package-install-upgrade-built-in t
 ;;; to the of the compilation
 compilation-scroll-output nil
 ;;; no message of revert buffer
 auto-revert-verbose nil
 ;;; no fringe bookmark
 bookmark-fringe-mark nil
 ;;; wdired
 wdired-allow-to-change-permissions t
 wdired-create-parent-directories t
 )

;; (load-theme 'purezen t)  ;; Replace with your preferred theme
