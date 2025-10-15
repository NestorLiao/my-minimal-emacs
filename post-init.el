;;; -*- lexical-binding: t; -*-
(require 'use-package)
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :ensure t
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
  ;; the top of your init file.
  ;; (push "/init.el" compile-angel-excluded-files)
  ;; (push "/early-init.el" compile-angel-excluded-files)
  ;; (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  ;; (push "/pre-early-init.el" compile-angel-excluded-files)
  ;; (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

(use-package envrc :defer 2
  :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

(use-package vterm
  :ensure t
  :defer t
  :commands (vterm--internal)
  :bind (:map vterm-mode-map
              ("C-p" . vterm-copy-mode)
              :map vterm-copy-mode-map
              ("C-p" . vterm-previous-prompt )
              ("C-f" . vterm-next-prompt )
              ("C-<return>" . compile-goto-error))
  :config
  (setq vterm-timer-delay 0.01)
  (with-eval-after-load 'vterm
    (setq vterm-kill-buffer-on-exit t)
    (advice-add 'vterm :after
                (lambda (buf)
                  (with-current-buffer buf
                    (set-process-query-on-exit-flag
                     (get-buffer-process (current-buffer)) nil))))))
(add-hook 'vterm-mode-hook (lambda () (compilation-shell-minor-mode 1) (define-key vterm-copy-mode-map (kbd "C-<return>") 'compile-goto-error)))

(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package vertico-flat
  :after vertico
  :ensure nil
  :init
  (vertico-flat-mode))

(use-package marginalia
  :ensure t
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ;; ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-$" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g d" . consult-dir)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose nil))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package eglot
  :ensure nil
  :defer t
  :bind (:map eglot-mode-map
              ("C-c f" . eglot-format)
              ("C-c d" . eldoc-doc-buffer)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)
              ("C-c l" . eglot-command-map))
  :hook (eglot-managed-mode . (lambda ()
                                ;; (add-hook 'eglot-managed-mode-hook #'eldoc-mode)
                                (eglot-inlay-hints-mode -1)
                                (eldoc-mode -1)
                                ;; (add-to-list 'eglot-stay-out-of 'flymake)
                                ;; (add-hook 'before-save-hook 'eglot-format nil nil)
                                ))
  :custom
  (eglot-sync-connect 0)
  (eldoc-mode -1)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider
                                       :colorProvider
                                       :inlayHintProvider))
  :config
  (setq flymake-no-changes-timeout 1)  ; Don't run on idle
  (setq flymake-start-on-flymake-mode t) ; Don't auto-start
  (setq eglot-server-programs
        '((c-mode . ("clangd"))
          (c++-mode . ("clangd"))
          (zig-mode . ("zls"))
          (latex-mode . ("texlab"))
          (rust-mode . ("rust-analyzer"))
          (nix-mode . ("nil"))))

  (defvar-keymap eglot-command-map
    :prefix 'eglot-command-map
    ;; workspaces
    "w q" #'eglot-shutdown
    "w r" #'eglot-reconnect
    "w s" #'eglot
    "w d" #'eglot-show-workspace-configuration

    ;; formatting
    "= =" #'eglot-format-buffer
    "= r" #'eglot-format

    ;; goto
    "g a" #'xref-find-apropos
    "g d" #'eglot-find-declaration
    "g g" #'xref-find-definitions
    "g i" #'eglot-find-implementation
    "g r" #'xref-find-references
    "g t" #'eglot-find-typeDefinition

    ;; actions
    "a q" #'eglot-code-action-quickfix
    "a r" #'eglot-code-action-rewrite
    "a i" #'eglot-code-action-inline
    "a e" #'eglot-code-action-extract
    "a o" #'eglot-code-action-organize-imports)

  :commands (eglot
             eglot-ensure
             eglot-rename
             eglot-format-buffer))

(use-package helpful
  :ensure t
  :defer t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package which-key
  :ensure nil ; builtin
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  (which-key-use-C-h-commands nil))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package dired
  :ensure nil
  :commands (dired)
  :bind
  ( :map dired-mode-map
    ("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command)
    ("," . dired-omit-mode)
    )
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . dired-omit-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  :custom
  (dired-omit-files
   (rx (or (seq bol ".")                      ; dotfiles
           (seq ".js" (? ".meta") eos)        ; .js.meta
           (seq "." (or "elc" "a" "o" "pyc" "pyo" "swp" "class") eos)
           (seq bol ".DS_Store")
           (seq bol "." (or "svn" "git") eos)
           (seq bol ".ccls-cache" eos)
           (seq bol "__pycache__" eos)
           (seq bol ".project" (? "ile") eos)
           (seq bol (or "flake.lock" "Cargo.lock" "LICENSE") eos)
           (seq bol (or "flycheck_" "flymake_"))))))

(use-package dired-subtree
  :after dired
  :bind ( :map dired-mode-map
          ("<tab>" . dired-subtree-toggle)
          ("TAB" . dired-subtree-toggle)
          ("<backtab>" . dired-subtree-remove)
          ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trashed-sort-key '("Date deleted" . t)
        trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-folded 'showeverything)
  (org-agenda-span 'week)
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  ;; (org-startup-truncated nil)
  (org-fontify-done-headline nil)
  (org-fontify-todo-headline nil)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  (org-agenda-files (directory-files-recursively "~/talkbase/" "\\.org$"))
  (org-todo-keywords
   '((sequence "TODO(t)"  "|" "DONE(d)")
     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-file-apps '("\\.html\\'" . eww-open-file))
    (defun insert-date()
      (interactive)
      (insert (format-time-string "%Y-%m-%d"))))
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (shell . t)
     (python . t))))

(use-package tldr
  :bind ( :map tldr-mode-map
          ("SPC" . scroll-up-command)
          ("DEL" . scroll-down-command)))

(use-package ligature
  :config
  (let ((ligs '("ff" "fi" "ffi" "fl" "ffl")))
    (ligature-set-ligatures 't ligs))
  (global-ligature-mode 1))

(defvar monitor-state 'read
  "Current monitor state, either 'read or 'watch.")

(defvar monitorpath "/dev/i2c-4")
(defvar monitorcli "paperlike-cli -i2c ")
(defvar monitorarg '(" -contrast " " -speed " " -mode "))
(defvar deepink '("9" "5" "1"))
(defvar shallow '("2" "2" "3"))

(defun read-monitor ()
  (progn
    (dotimes (number 3)
      (shell-command (concat
                      monitorcli
                      monitorpath
                      (car (nthcdr number monitorarg))
                      (car (nthcdr number deepink))))
      (sleep-for 0.5)
      ))
  (setq monitor-state 'read))

(defun watch-monitor ()
  (progn
    (dotimes (number 3)
      (shell-command (concat
                      monitorcli
                      monitorpath
                      (car (nthcdr number monitorarg))
                      (car (nthcdr number shallow))))
      (sleep-for 0.5)
      ))
  (setq monitor-state 'watch))

(defun toggle-monitor ()
  (interactive)
  (if (eq monitor-state 'read)
      (progn
        (watch-monitor)
        (start-process "notify" nil "notify-send" "Reminder" "move your mouse, now!")
        (run-at-time "15 second" nil
                     (lambda ()
                       (start-process "notify" nil "notify-send" "Reminder" "did you missed that?")
                       (read-monitor)
                       (sleep-for 1))))
    (progn
      (read-monitor)
      (sleep-for 1)))
  (progn
    (shell-command (concat monitorcli monitorpath " -clear"))
    (sleep-for 1)
    (delete-other-windows)))

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(use-package avy
  :ensure t
  :bind (
         ("M-g M-g" . avy-goto-line)
         ("M-g M-k" . avy-kill-whole-line))
  :config
  (setq avy-all-windows t)
  (setq avy-keys '( ?r ?s ?t ?d ?h ?n ?e ?i))
  (avy-setup-default)
  (setq isearch-allow-motion t)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?c avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?C avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  (defun dictionary-search-dwim (&optional arg)
    "Search for the definition of a word based on context.
- With a prefix argument (C-u), prompt for a word to search.
- If a region is active, search for the selected text.
- If the point is on a word, look up that word's definition.
- Otherwise, display a message indicating no word was found."
    (interactive "P")
    (cond
     (arg
      (dictionary-search nil))  ;; Prompt user for a word
     ((use-region-p)
      (dictionary-search (buffer-substring-no-properties (region-beginning) (region-end))))
     ((thing-at-point 'word)
      (dictionary-lookup-definition))
     (t
      (message "No word found at point or in region"))))

  (defun avy-dictionary-search (pt)
    "Jump to the point PT and search the definition of the word at that point."
    (interactive "d")
    (goto-char pt)
    (dictionary-search-dwim))

  ;; Bind avy-dictionary-search to the '=' key in avy-dispatch-alist
  (setf (alist-get ?D avy-dispatch-alist) 'avy-dictionary-search)

  (defun avy-quick-sdcv-search-at-point (pt)
    "Jump to the point PT and search the definition of the word at that point."
    (interactive "d")
    (goto-char pt)
    (quick-sdcv-search-at-point))

  ;; Bind avy-dictionary-search to the '=' key in avy-dispatch-alist
  (setf (alist-get ?q avy-dispatch-alist) 'avy-quick-sdcv-search-at-point)

  (defun tldr-at-point ()
    "Show tldr for the word at point."
    (interactive)
    (let ((word (current-word)))
      (if word
          (tldr word)
        (message "No word at point for tldr."))))

  (defun avy-action-tldr (pt)
    "Avy action to show tldr for the word at PT."
    (save-excursion (goto-char pt) (tldr-at-point))
    (select-window (cdr (ring-ref avy-ring 0))) t)

  (setf (alist-get ?l avy-dispatch-alist) 'avy-action-tldr)

  (defun man-at-point ()
    "Open the manual page for the word at point."
    (interactive)
    (let ((word (current-word)))
      (if word
          (man word)
        (message "No word at point to look up in the manual."))))

  (defun avy-action-man (pt)
    (save-excursion
      (goto-char pt)
      (man-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?M avy-dispatch-alist) 'avy-action-man)

  (defun avy-action-dired (pt)
    (save-excursion
      (goto-char pt)
      (dired-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?l avy-dispatch-alist) 'avy-action-dired)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  (defun avy-copy-region-between-words ()
    "Use avy to mark region between two words (including both words) and copy to kill ring."
    (interactive)
    (let ((orig-pos (point)))
      (avy-goto-word-0 1)
      (forward-word)  ; Move to end of first word
      (let ((start (point)))
        (avy-goto-word-0 1)
        (forward-word)  ; Move to end of second word
        (copy-region-as-kill start (point))
        (goto-char orig-pos))))

  (defun avy-action-copy-between-words (pt)
    (save-excursion
      (goto-char pt)
      (let ((start (point)))
        (avy-goto-word-0 1)
        (forward-word 1)  ; Move to end of first word
        (copy-region-as-kill start (point))))
    t)

  (setf (alist-get ?o avy-dispatch-alist) 'avy-action-copy-between-words))

(use-package aggressive-indent
  :ensure nil
  :config
  (global-aggressive-indent-mode nil)
  (add-to-list 'aggressive-indent-excluded-modes 'c-mode))

(use-package ediff
  :ensure nil
  :custom (ediff-window-setup-function 'ediff-setup-windows-plain "Do actions from single frame"))

(use-package diff-mode :ensure nil
  :custom
  (diff-default-read-only t)
  (diff-font-lock-syntax 'hunk-also)
  (diff-font-lock-prettify nil))

(use-package magit
  :config
  (setq
   magit-bury-buffer-function 'magit-restore-window-configuration
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-diff-refine-hunk  'all))

(defun url-to-hosts-line ()
  "Convert a URL on the current line to '    0.0.0.0 hostname' format."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (url (when line
                (string-match "https?://\\([^/]+\\)" line)
                (match-string 0 line)))
         (hostname (when url
                     (replace-regexp-in-string
                      "^https?://\\([^/]+\\).*" "\\1" url))))
    (when hostname
      (beginning-of-line)
      (kill-line)
      (insert (concat "0.0.0.0 " hostname))
      (beginning-of-line))))

(use-package syntax-subword
  :init
  (setq syntax-subword-skip-spaces t)
  :config
  (global-syntax-subword-mode 0))

(use-package quick-sdcv
  :vc (:url "https://github.com/jamescherti/quick-sdcv.el"
            :rev :newest)
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼ ")
  :bind
  ( :map quick-sdcv-mode-map
    ("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command)
    ("q" . delete-window)
    ("b" . quick-sdcv-search-at-point)))

(use-package consult-dir
  :bind (("<f6>" . consult-dir)
         :map vertico-map
         ("<f5>" . consult-dir-jump-file)))

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode
             elisp-autofmt-buffer
             elisp-autofmt-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-$"        . mc/skip-to-next-like-this)
         ("C-^"         . mc/skip-to-previous-like-this)))

(use-package eww
  :ensure nil
  :config
  (setq browse-url-browser-function 'eww-browse-url
        shr-use-colors nil
        eww-header-line-format ""
        shr-bullet "• "
        shr-folding-mode t
        shr-use-fonts nil
        shr-inhibit-images nil
        shr-width 80
        eww-search-prefix nil
        ;; url-privacy-level '(email agent cookies lastloc)
        url-privacy-level 'none
        url-cookie-untrusted-urls '(".*")
        eww-auto-rename-buffer 'url
        eww-prompt-history '(
                             "http://zig.doc:3003/" ; "https://ziglang.org/documentation/master/"
                             "http://c.doc:3001/" ; "https://en.cppreference.com/w/c"
                             "http://cpp.doc:3002/" ; "https://en.cppreference.com/w/cpp"
                             "http://linux.doc:3000/" ;"https://www.kernel.org/doc/html/latest/"
                                        ; C-h I "https://www.gnu.org/software/emacs/manual/"
                             ;; "https://docs.espressif.com/projects/esp-idf/en/latest/"
                             ;; "https://wiki.osdev.org/Main_Page"
                             ;; "https://docs.zephyrproject.org/latest/"
                             ;; "https://docs.lvgl.io/"
                             ))
  (with-eval-after-load 'eww
    (defun eww-toggle-images ()
      "Toggle whether or not to display images."
      (interactive nil eww-mode)
      (setq shr-inhibit-images (not shr-inhibit-images))
      (eww-reload)
      (message "Images are now %s"
               (if shr-inhibit-images "off" "on"))
      (if shr-inhibit-images  (donothing) (run-at-time "5" nil
                                                       (lambda () (progn
                                                                    (start-process "notify" nil "notify-send" "Reminder" "看一眼就够了")
                                                                    (setq shr-inhibit-images (not shr-inhibit-images))
                                                                    (eww-reload))))))
    (defun my-eww-edit-url ()
      "Edit the current EWW URL and reload the page."
      (interactive)
      (unless (derived-mode-p 'eww-mode)
        (user-error "Not in EWW buffer"))
      (let ((current-url (plist-get eww-data :url)))
        (setq eww-data (plist-put eww-data :url
                                  (read-string "Edit URL: " current-url)))
        (eww-reload))))
  (add-hook 'eww-after-render-hook 'eww-readable)  ;; Enable readable mode by default
  :bind (:map eww-mode-map
              ("e" . my-eww-edit-url)))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(defun my/suppress-recentf-messages (orig-fun &rest args)
  "Run `recentf-cleanup` without showing messages in the echo area."
  (let ((inhibit-message t)  ;; Prevent messages from appearing in the echo area
        (message-log-max nil))  ;; Prevent logging to *Messages* buffer
    (apply orig-fun args)))

(advice-add 'recentf-cleanup :around #'my/suppress-recentf-messages)

(use-package super-save
  :config
  (super-save-mode +1)
  (setq
   super-save-auto-save-when-idle t
   super-save-silent t
   super-save-all-buffers  t
   super-save-remote-files t))

(defun switch-to-gptel()
  (interactive)
  (if (equal  (current-buffer) (gptel "*deepseek*"))
      (previous-buffer)
    (switch-to-buffer "*deepseek*" )))

(defun tavily-search-async (callback query &optional search-depth max-results exclude_domains country include_domains)
  "Perform a search using the Tavily API and return results as JSON string.
API-KEY is your Tavily API key.
QUERY is the search query string.
Optional SEARCH-DEPTH is either \"basic\" (default) or \"advanced\".
Optional MAX-RESULTS is the maximum number of results (default 5)."
  (require 'plz)
  (let* ((plz-curl-default-args (cons "-k" plz-curl-default-args))
                                        ;"chunks_per_source": 3,
                                        ;"include_raw_content": true
                                        ;"time_range": "month"
                                        ;"start_date": "2025-01-01",
                                        ;"end_date": "2025-02-01"
                                        ;"topic": "news",
                                        ;"days": 1
                                        ;"auto_parameters": true,
                                        ;"search_depth": "basic", // Overrides 'advanced'
                                        ;"include_answer": true,
                                        ;"max_results": 10
                                        ;"include_domains": ["linkedin.com/in"]
                                        ;"include_domains": [ "crunchbase.com", "techcrunch.com", "pitchbook.com" ]
                                        ;"exclude_domains": ["espn.com","vogue.com"]
                                        ;"include_domains": ["*.com"]
                                        ;"exclude_domains": ["*.is"]
                                        ;"topic": "general",
                                        ;"country": "united states"
         (url "https://api.tavily.com/search")
         (search-depth (or search-depth "advanced"))
         (max-results (or max-results 1))
         (include_anwser  t)
         (country (or country "united states"))
         (include_domains (or include_domains '("nixos.org" "freertos.org" "zephyrproject.org" "contiki-ng.org" "riot-os.org" "nuttx.apache.org" "mynewt.apache.org" "ziglang.org" "python.org" "lua.org" "elixir-lang.org" "erlang.org" "haskell.org" "cmake.org" "gnu.org" "llvm.org" "gcc.gnu.org" "qt.io" "gtk.org" "sdl.org" "libsdl.org" "qemu-project.org" "cppreference.com" "opensource.org" "ietf.org" "w3.org" "ansi.org" "iso.org" "ieee.org" "man7.org" "discourse.nixos.org" "ziggit.dev" "emacs-china.org" "lwn.net" "kernel.org" "sourceware.org" "debian.org" "archlinux.org" "github.com" "osdev.org" "opencores.org" "riscv.org" "musl-libc.org" "newlib.sourceware.org" "uclibc-ng.org" "hackaday.com" "raspberrypi.org" "arduino.cc" "espressif.com" "gentoo.org")))
         (request-data
          `(("api_key" . ,tavily-api-key)
            ("query" . ,query)
            ("search_depth" . ,search-depth)
            ("country" . ,country)
            ("include_domains" . ,include_domains)
            ("include_anwser" . ,include_anwser)
            ("exclude_domains" . ,exclude_domains)
            ("max_results" . ,max-results))))
    (plz 'post url
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-data)
      :as 'string
      :then (lambda (result) (funcall callback result)))))

(defun tavily-search (query)
  (interactive "sQuery: ")
  (tavily-search-async
   (lambda (result)
     ;; (split-window)
     (let ((buf (get-buffer-create "*tavily-search-result*")))
       (switch-to-buffer buf)
       (read-only-mode 0)
       (erase-buffer)
       (org-mode)
       (insert (tavily-result-to-org result))
       (goto-char (point-min))
       (read-only-mode 1)
       ))
   query))

(defun tavily-result-to-org (json-result)
  (let* ((data (json-read-from-string json-result))
         (results (alist-get 'results data)))
    (mapconcat (lambda (item)
                 (format "* [[%s][%s]]\n  %s"
                         (alist-get 'url item)
                         (alist-get 'title item)
                         (alist-get 'content item)))
               results
               "\n\n")))

(setq tavily-api-key
      (with-temp-buffer
        (insert-file-contents "/run/secrets/tavily_apikey")
        (buffer-string)))

(use-package gptel
  :init
  (require 'gptel-org)
  :config
  (with-eval-after-load 'gptel
    (gptel-make-tool
     :category "web"
     :name "search"
     :async t
     :function (lambda (cb keyword)
                 (tavily-search-async cb keyword "basic" 5 nil nil nil))
     :description "Search the Internet; If you used any search results, be sure to include the references in your response."
     :args (list '(:name "keyword"
                         :type string
                         :description "The keyword to search")))

    (gptel-make-tool
     :name "create_python_repl"
     :function (lambda ()
                 (run-python nil t)
                 (pop-to-buffer (python-shell-get-buffer)))
     :description "Create a new python repl for this session"
     :args nil
     :category "emacs")

    (gptel-make-tool
     :name "send_python_to_repl"
     :function (lambda (code)
                 (python-shell-send-string code))
     :args (list '(:name "code"
                         :type string
                         :description "python code to execute"))
     :description "Send some python code to the python repl for this session and execute it"
     :category "emacs")

    (gptel-make-tool
     :function (lambda (url)
                 (with-current-buffer (url-retrieve-synchronously url)
                   (goto-char (point-min)) (forward-paragraph)
                   (let ((dom (libxml-parse-html-region (point) (point-max))))
                     (run-at-time 0 nil #'kill-buffer (current-buffer))
                     (with-temp-buffer
                       (shr-insert-document dom)
                       (buffer-substring-no-properties (point-min) (point-max))))))
     :name "read_url"
     :description "Fetch and read the contents of a URL"
     :args (list '(:name "url"
                         :type "string"
                         :description "The URL to read"))
     :category "web")

    (gptel-make-tool
     :function (lambda (buffer text)
                 (with-current-buffer (get-buffer-create buffer)
                   (save-excursion
                     (goto-char (point-max))
                     (insert text)))
                 (format "Appended text to buffer %s" buffer))
     :name "append_to_buffer"
     :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
     :args (list '(:name "buffer"
                         :type "string"
                         :description "The name of the buffer to append text to.")
                 '(:name "text"
                         :type "string"
                         :description "The text to append to the buffer."))
     :category "emacs")

    ;; Message buffer logging tool
    (gptel-make-tool
     :function (lambda (text)
                 (message "%s" text)
                 (format "Message sent: %s" text))
     :name "echo_message"
     :description "Send a message to the *Messages* buffer"
     :args (list '(:name "text"
                         :type "string"
                         :description "The text to send to the messages buffer"))
     :category "emacs")

    ;; buffer retrieval tool
    (gptel-make-tool
     :function (lambda (buffer)
                 (unless (buffer-live-p (get-buffer buffer))
                   (error "Error: buffer %s is not live." buffer))
                 (with-current-buffer  buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
     :name "read_buffer"
     :description "Return the contents of an Emacs buffer"
     :args (list '(:name "buffer"
                         :type "string"
                         :description "The name of the buffer whose contents are to be retrieved"))
     :category "emacs")

    (gptel-make-tool
     :function (lambda (directory)
                 (mapconcat #'identity
                            (directory-files directory)
                            "\n"))
     :name "list_directory"
     :description "List the contents of a given directory"
     :args (list '(:name "directory"
                         :type "string"
                         :description "The path to the directory to list"))
     :category "filesystem")

    (gptel-make-tool
     :function (lambda (parent name)
                 (condition-case nil
                     (progn
                       (make-directory (expand-file-name name parent) t)
                       (format "Directory %s created/verified in %s" name parent))
                   (error (format "Error creating directory %s in %s" name parent))))
     :name "make_directory"
     :description "Create a new directory with the given name in the specified parent directory"
     :args (list '(:name "parent"
                         :type "string"
                         :description "The parent directory where the new directory should be created, e.g. /tmp")
                 '(:name "name"
                         :type "string"
                         :description "The name of the new directory to create, e.g. testdir"))
     :category "filesystem")

    (gptel-make-tool
     :function (lambda (path filename content)
                 (let ((full-path (expand-file-name filename path)))
                   (with-temp-buffer
                     (insert content)
                     (write-file full-path))
                   (format "Created file %s in %s" filename path)))
     :name "create_file"
     :description "Create a new file with the specified content"
     :args (list '(:name "path"
                         :type "string"
                         :description "The directory where to create the file")
                 '(:name "filename"
                         :type "string"
                         :description "The name of the file to create")
                 '(:name "content"
                         :type "string"
                         :description "The content to write to the file"))
     :category "filesystem")

    (gptel-make-tool
     :function (lambda (filepath)
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name filepath))
                   (buffer-string)))
     :name "read_file"
     :description "Read and display the contents of a file"
     :args (list '(:name "filepath"
                         :type "string"
                         :description "Path to the file to read.  Supports relative paths and ~."))
     :category "filesystem"))

  (defun ant/gptel-save-buffer ()
    "Save the current GPTEL buffer with the default directory
set to ~/note."
    (interactive)
    (let ((default-directory "~/talkbase/gpt/"))
      (call-interactively #'save-buffer)))

  (defun ant/gptel-load-session ()
    "Load a gptel session from ~/notes directory."
    (interactive)
    (let ((default-directory "~/.leetcode/code/"))
      (let* ((files (directory-files default-directory t ".+\\.org$"))
             (file (completing-read "Select session file: " files nil t)))
        (when file
          (find-file file)
          (gptel-mode)))))

  (setq  gptel-default-mode 'org-mode)
  ;; OPTIONAL configuration

  (setq deepseek-api-key
        (with-temp-buffer
          (insert-file-contents "/run/secrets/deepseek_apikey")
          (buffer-string)))

  (setq gptel-model   'deepseek-chat
        gptel-backend (gptel-make-deepseek "deepseek"
                        :stream t
                        :key deepseek-api-key))

  (require 'url-util)

  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")
          (bug . "You are a large language model and a careful programmer. The supplied code doesn't work, or contains bugs. Describe each problem using only one sentence. Provide fixes without changing the old behavior.")))
  (setq  gptel-stream nil)
  :bind (:map gptel-mode-map
              ("C-c C-d" . ant/load-gptel-directives-from-org)
              ("C-x C-s" . ant/gptel-save-buffer)))

(use-package undo-fu
  :ensure t
  :defer t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(use-package undo-fu-session
  :ensure t
  :defer t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

(use-package disproject
  :ensure t
  :bind
  ([remap disproject-switch-to-buffer] . consult-project-buffer)
  ([remap disproject-shell-command] . project-shell)
  :config
  (with-eval-after-load 'disproject
    (defun my-project-shell ()
      "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
      (interactive)
      (require 'comint)
      (let* ((default-directory (project-root (project-current t)))
             (default-project-shell-name (project-prefixed-buffer-name "shell"))
             (shell-buffer (get-buffer default-project-shell-name)))
        (if (and shell-buffer (not current-prefix-arg))
            (if (comint-check-proc shell-buffer)
                (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
              (vterm shell-buffer))
          (vterm (generate-new-buffer-name default-project-shell-name)))))
    (advice-add 'project-shell :override #'my-project-shell))
  )

(defvar my-toggled-buffer nil
  "Temporarily stores the buffer that was hidden by `my-toggle-other-window`.")

(defun my-toggle-other-window ()
  "Toggle the visibility of the other window's buffer.
If two windows are present, hide the other window (and save its buffer).
If only one window is visible and a buffer was hidden earlier, restore that buffer
in a new window.

This function is designed for situations where, for example, one window shows a shell
and the other shows a browsing buffer. Calling the function toggles the visibility
of the non-current window."
  (interactive)
  (if (> (count-windows) 1)
      ;; Two or more windows exist: hide the one that is not the selected window.
      (let ((other-window (next-window)))
        (setq my-toggled-buffer (window-buffer other-window))
        (delete-window other-window))
    ;; Only one window is visible.
    (if (and my-toggled-buffer (buffer-live-p my-toggled-buffer))
        (progn
          (display-buffer my-toggled-buffer)
          (setq my-toggled-buffer nil))
      (message "No toggled buffer available."))))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("p" . 'scroll-down-command)
              ("n" . 'scroll-up-command)
              ("b" . (lambda () (interactive) (toggle-monitor)))
              ("ESC <prior>" . (lambda () (interactive) (bookmark-set "epub")))
              ("C-M-i" .              (lambda () (interactive) (bookmark-jump "epub")))
              ("<prior>" . nov-scroll-down)
              ("<next>" . nov-scroll-up))
  :config
  (setq nov-header-line-format ""))

;; pen
;; 1. <tab>
;;    - double: <enter>
;;    - long: C-M-i
;; 2. <prior>
;;    - long: <esc>/<f5>
;; 3. <next>
;;    - long: <b>
;; 4. laser

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init (pdf-loader-install)
  :commands (pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("n" . pdf-view-next-page)
              ("p" . pdf-view-previous-page)
              ("b" . (lambda () (interactive) (toggle-monitor)))
              ("ESC <prior>" . (lambda () (interactive) (bookmark-set "pdf")))
              ("C-M-i" .              (lambda () (interactive) (bookmark-jump "pdf")))
              ("<prior>" . pdf-view-previous-page)
              ("<next>" . pdf-view-next-page))
  :config (add-to-list 'revert-without-query ".pdf"))

(use-package info
  :bind (:map Info-mode-map
              ("<mouse-8>" . scroll-up-record)
              ("<mouse-9>" . scroll-down-record)
              ("<right-fringe><mouse-8>" . scroll-up-record)
              ("<right-fringe><mouse-9>" . scroll-down-record)
              ("C-M-i" . Info-history-back)
              ("<prior>" . scroll-down-record)
              ("<next>" . scroll-up-record)
              ("b" . Info-next-preorder)
              )
  :config
  (with-eval-after-load 'info
    (defun Info-save (&optional arg)
      (interactive "P" Info-mode)
      (unless Info-current-node
        (user-error "No current Info node"))
      (let ((node (if (stringp Info-current-file)
		              (concat (file-name-sans-extension
			                   (file-name-nondirectory Info-current-file))
			                  ))))
        (bookmark-set node)))

    (defun scroll-up-record()
      "record info place to bookmark"
      (interactive)
      (Info-scroll-up)
      (Info-save))

    (defun scroll-down-record()
      "record info place to bookmark"
      (interactive)
      (Info-scroll-down)
      (Info-save))))

(add-to-list 'completion-at-point-functions #'elisp-completion-at-after)

(with-eval-after-load 'vertico
  ;; ... other vertico keybindings and configurations ...
  ;; Vertico repeat key bindings
  (define-key vertico-map (kbd "C-,") #'vertico-repeat-previous)
  ;; (define-key vertico-map (kbd "C-.") #'vertico-repeat-next)

  ;; Vertico suspend key binding
  ;; (define-key vertico-map (kbd "C-'") #'vertico-suspend)
  (keymap-global-set "C-'" 'vertico-suspend)

  ;; For Embark user: avoid adding redundant repeat history after embark-act.
  (defun vertico-repeat--filter-explicit (session)
    (and (cadr session) (eq this-command real-this-command) session))

  ;; Adjust vertico-repeat transformers and filters
  (setq vertico-repeat-transformers '(vertico-repeat--filter-commands vertico-repeat--filter-explicit))
  (setq vertico-repeat-filter '(vertico-repeat vertico-repeat-select))

  ;; Backup minibuffer/command history to preserve candidate order
  (defvar minibuffer-history-repeat-backup minibuffer-history)
  (defvar extended-command-history-repeat-backup extended-command-history)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq minibuffer-history-repeat-backup minibuffer-history)
              (setq extended-command-history-repeat-backup extended-command-history)
              (setq face-name-history-backup face-name-history)))

  ;; Advise vertico-repeat to respect backed up history
  (advice-add 'vertico-repeat :around
              (lambda (orig &rest args)
                (let ((minibuffer-history minibuffer-history-repeat-backup)
                      (extended-command-history extended-command-history-repeat-backup)
                      (face-name-history face-name-history-backup)
                      (consult-async-split-styles-alist
                       (mapcar (lambda (plist)
                                 (cons (car plist)
                                       (consult--plist-remove '(:initial) (cdr plist))))
                               consult-async-split-styles-alist)))
                  (apply orig args)))))

(defun xah-clean-whitespace ()
  (interactive)
  (let (xbegin xend)
    (if (region-active-p)
        (setq xbegin (region-beginning) xend (region-end))
      (setq xbegin (point-min) xend (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region xbegin xend)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n"))
        (goto-char (point-max))
        (while (eq (char-before) 32) (delete-char -1)))))
  (message "%s done" real-this-command))

(keymap-global-set "M-o" 'other-window)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-<backspace>" 'avy-goto-word-0)
(global-set-key  [remap list-buffers] 'ibuffer)
(global-set-key  [remap switch-to-buffer] 'consult-buffer)
(global-set-key  [remap project-switch-to-buffer] 'consult-project-buffer)
(global-set-key  [remap kill-buffer] 'kill-current-buffer)
(keymap-global-set "M-i" 'imenu)
(keymap-global-set "C-<tab>" 'previous-buffer)
(keymap-global-set "C-<iso-lefttab>" 'next-buffer)
(keymap-global-set "<Cut>" 'kill-region)
(keymap-global-set "<Copy>" 'kill-ring-save)
(keymap-global-set "<Paste>" 'yank)
;; (keymap-global-set "C-z" 'undo-fu-only-undo)
;; (keymap-global-set "C-S-Z" 'undo-fu-only-redo)
(keymap-global-set "<Undo>" 'undo-fu-only-undo)
(keymap-global-set "<redo>" 'undo-fu-only-redo)

(keymap-global-set "ESC <f5>" 'hibernatecall)

(keymap-global-set "C-x C-d" (lambda ()
                               (interactive)
                               (duplicate-line)
                               (next-line)))

(keymap-global-set "<f8>" 'my-toggle-other-window)
(keymap-global-set "<WakeUp>" 'wakeupcall)
(keymap-global-set "<mouse-8>" 'scroll-up-command)
(keymap-global-set "<mouse-9>" 'scroll-down-command)
(defun donothing () (interactive)(message ""))

(move-text-default-bindings)

(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

(add-hook 'after-init-hook (lambda () (load-theme 'purezen t)))
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'global-hide-mode-line-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(add-hook 'after-init-hook #'window-divider-mode)

(defun hibernatecall()
  (interactive)
  (find-file "~/.hibernate")
  (goto-char (point-max))  (beginning-of-line)
  (insert (message "Good Bey! The PC Hibernate At %S\n" (current-time-string)))
  (setq hibernatetime (current-time))
  (setq monitor-state 'read)
  (sleep-for 1)
  (shell-command "systemctl hibernate"))

(defun wakeupcall()
  (interactive)
  (find-file "~/.hibernate")
  (goto-char (point-max))
  (beginning-of-line)
  (insert (message "Sleep For %S Hour, Have A Nice Day!\n"
                   (/ (time-to-seconds (time-since hibernatetime) ) 3600)))
  (shell-command "notify-send -t 3000 'Have A Nice Time!'")
  (delete-other-windows)
  ;; (previous-line)
  ;; (kill-whole-line)
  (save-excursion
    (find-file "~/.paperlike_state")
    (erase-buffer)
    (insert "read")
    (save-buffer)
    (kill-buffer)
    (donothing))
  (sleep-for 1)
  (bookmark-jump "pdf")
  (clear-minibuffer-message))

(use-package pyim
  :ensure t
  :custom
  (default-input-method "pyim")
  :config

  (define-key pyim-mode-map ";"
              (lambda ()
                (interactive)
                (pyim-select-word-by-number 2)))

  (setq pyim-indicator-list (list #'my-pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))

  (defun hd()
    "Show hmdz for the word at point."
    (interactive)
    (let ((char (char-after)))
      (if char
          (search-hmdz (char-to-string  char))
        (message "No word at point for tldr."))))

  (defun search-hmdz(char)
    "search hmdz for the input"
    (interactive "p\ncChar: ")
    (let ((old (current-buffer))
          (exsist 0))
      (save-excursion
        (find-file "~/.emacs.d/hmdz.pyim")
        (beginning-of-buffer)
        (search-forward char nil (setq exsist 1))
        (when (= exsist 1)
          (search-backward "/")
          (right-char)
          (message "%s" (string-trim (current-word) "hmdz/")))
        (when (= exsist 0)
          (message "no such shit"))
        (kill-buffer)
        (switch-to-buffer old))))

  (pyim-scheme-add
   '(hmdz
     :document
     "虎码单字"
     :class xingma
     :code-prefix "hmdz/"
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :code-prefix-history ("_")
     :code-split-length 4
     :code-maximum-length 4))
  ;;----'默认码表'
  (pyim-default-scheme 'hmdz)
  (setq pyim-process-autoselector nil)
  (setq pyim-dhook-verbose nil)
  (setq pyim-dicts nil)  ; Initialize the list if it's not already defined
  (setq pyim-cloudim nil)
  (setq pyim-candidates-search-buffer-p nil)
  (setq pyim-enable-shortcode nil)
  (setq-default pyim-english-input-switch-functions '(pyim-probe-program-mode))
  (setq pyim-punctuation-dict '())
  ;; (setq pyim-punctuation-dict
  ;;       '(("'" "‘" "’")
  ;;         ("\"" "“" "”")
  ;;         ("_" "——")
  ;;         ("^" "…")
  ;;         ;; ("]" "】")
  ;;         ("]" "]")
  ;;         ;; ("[" "【")
  ;;         ("[" "[")
  ;;         ("@" "◎")
  ;;         ("?" "？")
  ;;         ;; (">" "》")
  ;;         (">" ">")
  ;;         ;; ("=" "＝")
  ;;         ("=" "=")
  ;;         ;; ("<" "《")
  ;;         ("<" "<")
  ;;         (";" "；")
  ;;         (":" "：")
  ;;         ("\\" "、")
  ;;         ("." "。")
  ;;         ("-" "-")
  ;;         ("," "，")
  ;;         ;; ("+" "＋")
  ;;         ("+" "+")
  ;;         ("*" "*")
  ;;         ;; (")" "）")
  ;;         (")" ")")
  ;;         ("(" "(")
  ;;         ;; ("(" "（")
  ;;         ("&" "※")
  ;;         ("%" "％")
  ;;         ("$" "￥")
  ;;         ;; ("#" "＃")
  ;;         ("#" "#")
  ;;         ("!" "！")
  ;;         ("`" "・")
  ;;         ("~" "～")
  ;;         ("}" "』")
  ;;         ("|" "÷")
  ;;         ("{" "『")))
  (add-to-list 'pyim-dicts
               '(:name "hmdz" :file "~/.emacs.d/hmdz.pyim")))

(use-package pomm
  :ensure t
  :custom
  (alert-default-style 'libnotify)
  (pomm-third-time-csv-history-file "~/.history.csv")
  (pomm-audio-enabled t)
  :commands (pomm pomm-third-time))

(setq pomm-audio-files
      '((work . "/nix/store/xp2xczaznv55d97f4wc81par6082qv15-emacs-packages-deps/share/emacs/site-lisp/elpa/pomm-20250202.1202/resources/tick.wav")
        (tick . "/nix/store/xp2xczaznv55d97f4wc81par6082qv15-emacs-packages-deps/share/emacs/site-lisp/elpa/pomm-20250202.1202/resources/tick.wav")
        (short-break . "/nix/store/xp2xczaznv55d97f4wc81par6082qv15-emacs-packages-deps/share/emacs/site-lisp/elpa/pomm-20250202.1202/resources/tick.wav")
        (break . "/nix/store/xp2xczaznv55d97f4wc81par6082qv15-emacs-packages-deps/share/emacs/site-lisp/elpa/pomm-20250202.1202/resources/tick.wav")
        (long-break . "/nix/store/xp2xczaznv55d97f4wc81par6082qv15-emacs-packages-deps/share/emacs/site-lisp/elpa/pomm-20250202.1202/resources/tick.wav")
        (stop . "/nix/store/xp2xczaznv55d97f4wc81par6082qv15-emacs-packages-deps/share/emacs/site-lisp/elpa/pomm-20250202.1202/resources/tick.wav")))

(defun switchepubinfo ()
  "Switch between *info* buffer and a specific EPUB file in nov-mode."
  (interactive)
  (let ((epub-file "/home/leeao/嵌入式C语言自我修养：从芯片、编译器到操作系统.epub"))
    (cond
     ;; If we are in nov-mode, switch to *info* buffer
     ((derived-mode-p 'nov-mode)
      (if (get-buffer "*info*")
          (switch-to-buffer "*info*")
        (info)))  ;; If no *info* buffer exists, open Info
     ;; If we are in Info mode, open the EPUB
     ((derived-mode-p 'Info-mode)
      (if (file-exists-p epub-file)
          (find-file epub-file)
        (message "EPUB file not found: %s" epub-file)))
     ;; Otherwise, just open the EPUB by default
     (t
      (if (file-exists-p epub-file)
          (find-file epub-file)
        (message "EPUB file not found: %s" epub-file))))))

(keymap-global-set "ESC <next>" 'switchepubinfo)

(defface my-fringe-gdb
  '((t (:background "white" :foreground "black")))
  "Fringe face for GDB mode."
  :group 'my-faces)

(defface my-fringe-default
  '((t (:background "white" :foreground "white")))
  "Fringe face for all other modes."
  :group 'my-faces)

(defun my-set-fringe-face ()
  "Set fringe face depending on major mode."
  (if (derived-mode-p 'gdb-mode)
      (set-face-attribute 'fringe nil
                          :background (face-attribute 'my-fringe-gdb :background)
                          :foreground (face-attribute 'my-fringe-gdb :foreground))
    (set-face-attribute 'fringe nil
                        :background (face-attribute 'my-fringe-default :background)
                        :foreground (face-attribute 'my-fringe-default :foreground))))

;; Hook into mode changes
(add-hook 'after-change-major-mode-hook #'my-set-fringe-face)

;; Also apply immediately on startup
(my-set-fringe-face)

(use-package yasnippet
  :ensure t
  :vc (:url "https://github.com/joaotavora/yasnippet"
            :rev :newest)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package consult-gh
  :after consult
  :custom
  (consult-gh-confirm-before-clone nil)
  (consult-gh-default-clone-directory "~/codebase")
  (consult-gh-ask-for-path-before-save nil)
  (consult-gh-default-save-directory "~/codebase")
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-confirm-name-before-fork nil)
  (consult-gh-notifications-show-unread-only nil)
  (consult-gh-default-interactive-command)
  (consult-gh-prioritize-local-folder nil)
  (consult-gh-issues-state-to-show "all") ; show readmes in their original format
  (consult-gh-group-dashboard-by :reason)
  ;;;; Optional
  (consult-gh-repo-preview-major-mode nil) ; show readmes in their original format
  (consult-gh-preview-major-mode 'org-mode) ; use 'org-mode for editing comments, commit messages, ...
  ;; :config
  (require 'consult-gh-transient)
  ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
  (consult-gh-enable-default-keybindings))

(use-package consult-gh-forge
  :after consult-gh
  :config
  (consult-gh-forge-mode +1))

(use-package consult-gh-embark
  :after consult-gh
  :config
  (consult-gh-embark-mode +1)
  (setq consult-gh-forge-timeout-seconds 20))

(defun extract-html-hrefs ()
  "Extract all href links from the current HTML buffer, printing them line by line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((links '()))
      (while (re-search-forward "href=[\"']\\([^\"']+\\)[\"']" nil t)
        (push (match-string 1) links))
      (with-output-to-temp-buffer "*HTML Links*"
        (dolist (link (nreverse links))
          (princ link)
          (princ "\n"))))))

(defun mark-current-line (arg)
  "Mark the current line, or extend the region by lines.
If no region is active, mark the current line.
If region is active:
  - With no prefix ARG, extend the region downward by one line.
  - With prefix ARG (C-u), extend the region upward by one line."
  (interactive "P")
  (cond
   ;; If region already active
   ((use-region-p)
    (if arg
        ;; Extend upward
        (set-mark (save-excursion
                    (goto-char (region-beginning))
                    (forward-line -1)
                    (point)))
      ;; Extend downward
      (set-mark (region-beginning))
      (goto-char (save-excursion
                   (goto-char (region-end))
                   (forward-line 1)
                   (point)))))
   ;; No region active: just select current line
   (t
    (set-mark (line-beginning-position))
    (goto-char (line-end-position))
    (exchange-point-and-mark))))

(defun my/nix-store-shorten-paths ()
  "Replace long /nix/store paths with shortened ...-pkg-version."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (re-search-forward
            "/nix/store/[a-z0-9]+-\\([^[:space:]]+\\)" nil t)
      (replace-match "...-\\1" t nil))))

(defun my/compilation-filter-hook ()
  (my/nix-store-shorten-paths))

(add-hook 'compilation-filter-hook #'my/compilation-filter-hook)
(remove-hook 'compilation-filter-hook #'my/compilation-filter-hook)

(define-prefix-command 'nav-map)
(keymap-global-set "C-c C-~" 'nav-map)
(define-key nav-map (kbd "g") #'er/expand-region)
(define-key nav-map (kbd "m") #'toggle-truncate-lines)
(define-key nav-map (kbd "v") (lambda () (interactive) (recenter-top-bottom 123)))
(define-key nav-map (kbd "b") #'quick-sdcv-search-at-point)
(define-key nav-map (kbd "j") #'quick-sdcv-search-at-point)
(define-key nav-map (kbd "l") #'quick-sdcv-search-at-point)
(define-key nav-map (kbd "u") #'quick-sdcv-search-at-point)
(define-key nav-map (kbd "y") #'quick-sdcv-search-at-point)
(define-key nav-map (kbd "'") #'quick-sdcv-search-at-point)

(define-prefix-command 'mos-map)
(keymap-global-set "C-c C-;" 'mos-map)
(define-key mos-map (kbd "q") #'gptel-menu)
(define-key mos-map (kbd "w") #'consult-gh)
(define-key mos-map (kbd "f") #'rg-dwim)
(define-key mos-map (kbd "p") #'disproject-dispatch)
(define-key mos-map (kbd "b") #'ibuffer)
(define-key mos-map (kbd "j") #'global-font-lock-mode)
(define-key mos-map (kbd "l") #'tldr)
(define-key mos-map (kbd "u") #'delete-all-space)
(define-key mos-map (kbd "y") #'global-hide-mode-line-mode)
(define-key mos-map (kbd "'") #'vertico-flat-mode)
(define-key mos-map (kbd "a") #'comment-line)
(define-key mos-map (kbd "r") #'keyboard-quit)
(define-key mos-map (kbd "s") #'find-file)
(define-key mos-map (kbd "t") #'consult-buffer)
(define-key mos-map (kbd "m") #'man)
(define-key mos-map (kbd "z") #'search-at-point)
(define-key mos-map (kbd "c") #'compile)
(define-key mos-map (kbd "d") #'dired)
(define-key mos-map (kbd "v") #'multi-vterm-project)
(define-key mos-map (kbd "k") #'kill-current-buffer)
(define-key mos-map (kbd "SPC") #'indent-rigidly)

(keymap-global-set "s-m" #'switch-to-gptel)
(keymap-global-set "s-M" (lambda () (interactive) (toggle-monitor)))
(keymap-global-set "s-t"  (lambda () (interactive) (recenter-top-bottom 0)))
(keymap-global-set "s-w"  #'eww)
(keymap-global-set "<Tools>" #'tavily-search)
(keymap-global-set "<Launch5>" #'trashed)
(keymap-global-set "<Launch6>" #'one-build)
(keymap-global-set "<Launch7>" #'xah-clean-whitespace)
(keymap-global-set "<Launch8>" #'delete-duplicate-lines)
(keymap-global-set "<Launch9>" #'my-toggle-font)
(keymap-global-set "<AudioMicMute>" #'magit-log-buffer-file)
(keymap-global-set "<TouchpadToggle>" #'transient-copy-menu-text)
(keymap-global-set "<f7>" (lambda () (interactive)  (set-mark-command (universal-argument))))

(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)

(setq gdb-many-windows nil)
(setq gdb-show-main t)
(setq compile-command "")
(setq enable-dir-local-variables nil)

(defvar my-alternate-font "-DAMA-UbuntuMono Nerd Font-regular-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(defvar my-default-font "bookerly")
(defvar fontfont 1)
(defun my-toggle-font ()
  "Toggle between UbuntuMono and bookerly fonts."
  (interactive)
  (if (= fontfont 1)
      (progn (set-face-attribute 'default nil :font my-default-font :height 160) (setq fontfont 0))
    (progn (set-face-attribute 'default nil :font my-alternate-font :height 160) (setq fontfont 1))))

(set-face-attribute 'default nil :font my-default-font)

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))

(defun extract-base-urls ()
  "Extract base URLs from the current buffer containing text file links."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((urls '()))
      (while (re-search-forward "https?://\\([^:/]+\\)" nil t)
        (push (match-string 0) urls))
      (with-output-to-temp-buffer "*Base URLs*"
        (dolist (url (delete-dups (nreverse urls)))
          (princ url)
          (princ "\n"))))))

(use-package one :ensure t
  :config
  (defun one-build ()
    "Build website of the current buffer under `./public/' subdirectory.
Specifically:
1) clean `./public/' subdirectory (if it exists),
2) copy `./assets/' files into `./public/' subdirectory and
3) call `one-render-pages' once.
See `one-render-pages'."
    (interactive)
    (when (file-exists-p "./public/")
      (delete-directory "./public/blog" t)
      (delete-file "index.html")
      ;; (dolist (file (directory-files "./public/" 'full "^[^.]"))
      ;;   (unless (string-match-p "/\\.git/?$" file)
      ;;     (if (file-directory-p file)
      ;;         (delete-directory file t)
      ;;       (delete-file file))))
      )
    (one-copy-assets-to-public)
    (one-render-pages)))

(use-package no-emoji
  :ensure t
  :config
  (setq no-emoji-display-table (make-display-table))
  (global-no-emoji-minor-mode 1))

(setq shell-command-switch "-ic")

(add-hook 'fundamental-mode-hook
          (lambda ()
            (setq-local comment-start "#")
            (setq-local comment-end "")))

(defun swtichinsoque()
  (interactive)
  (if(string=  (c-get-current-file) "solution")
      (find-file "question.org")(find-file "solution.cpp")))

(defun swtichinlang()
  (interactive)
  (if(string=  (file-name-nondirectory (buffer-file-name))
               "solution.cpp")
      (find-file "solution.rs")(find-file "solution.cpp")))

(keymap-global-set "s-s" 'swtichinsoque)
(keymap-global-set "s-S" 'swtichinlang)

(defalias 'nextleetcode
  (kmacro
   "C-n d <return> ^ <down> <return> M-< C-s q u e s t i o n <up> <down> <return>"))

(keymap-global-set "s-n" 'nextleetcode)

(defalias 'preleetcode
  (kmacro
   "C-n d <return> ^ <up> <return> M-< C-s q u e s t i o n <end> <left> <return>"))

(keymap-global-set "s-p" 'preleetcode)

(setq face-font-rescale-alist '(("Source Han" . 0.9)))

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(setq resize-mini-windows nil)
(global-eldoc-mode -1)
(setq vterm-always-compile-module t)

(use-package face-remap
  :ensure nil
  :config
  (defun text-scale-adjust (inc)
    (interactive "p")
    (let ((ev last-command-event)
	      (echo-keystrokes nil))
      (let* ((base (event-basic-type ev))
             (step
              (pcase base
                ((or ?+ ?=) inc)
                (?- (- inc))
                (?0 0)
                (_ inc))))
        (text-scale-increase step)
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (dolist (mods '(() (control)))
             (dolist (key '(?+ ?= ?- ?0)) ;; = is often unshifted +.
               (define-key map (vector (append mods (list key)))
                           (lambda () (interactive) (text-scale-adjust (abs inc))))))
           map)
         nil nil
         "")))))

(use-package simple
  :ensure nil
  :config)
