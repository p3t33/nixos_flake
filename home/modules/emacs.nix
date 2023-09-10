{ pkgs, ... }:
{
    programs.emacs = {
        enable = true;
        package = pkgs.emacs;  # replace with pkgs.emacs-gtk, or a version provided by the community overlay if desired.

        extraPackages = epkgs: [
            epkgs.use-package
            epkgs.evil
            epkgs.evil-collection
            epkgs.general
            epkgs.toc-org
            epkgs.org-bullets
            epkgs.which-key
            epkgs.sudo-edit
            epkgs.counsel
            epkgs.ivy
            epkgs.all-the-icons-ivy-rich
            epkgs.ivy-rich
            epkgs.all-the-icons
            epkgs.all-the-icons-dired
            epkgs.eshell-syntax-highlighting
            epkgs.vterm
            epkgs.vterm-toggle
            epkgs.rainbow-mode
            epkgs.dashboard
            epkgs.projectile
            epkgs.diminish
            epkgs.nord-theme
        ];
        extraConfig = ''
            ;; disabling the default built int plugin manager
            ;; in context of nix these settings are essentially ensuring a
            ;; clean slate" in terms of package management behavior in Emacs
            ;; They ensure that certain built-in package manager features that
            ;; might conflict with an external management tool are turned
            ;; off or are in their most permissive settings.
            ;;
            ;; This should be always at the top of the file before any
            ;; other settings have the chance to run.
            (setq package-quickstart nil)
            (setq package-menu-async nil)
            (setq package-load-list '(all))

            ;; Ensure the use-package macro is available
            ;; This needs to be the first plugin related settings as
            ;; every pluging that uses this macro in order to use it
            ;; for its own configuration needs to be loaded only after
            ;; this macro has been loaded.
            (require 'use-package)

            ;; Configure and initialize evil-mode using use-package
            (use-package evil
                :init
                ;; by default evil has some basic intergration with other
                ;; packages and modes but as I am delegating the intergration
                ;; to evil-collection this basic intergration should be
                ;; disabled.
                (setq evil-want-integration nil)
                (setq evil-want-keybinding nil)

                (setq evil-vsplit-window-right t)
                (setq evil-split-window-below t)
                :config
                (evil-mode 1))

            (use-package evil-collection
                :after evil
                :config
                ;; limites the mode list to intergrate with, might chagne this
                ;; in the future.
                (setq evil-collection-mode-list '(dashboard dired ibuffer))
                (evil-collection-init))

            (use-package sudo-edit)

            (use-package general
                 :config
                 (general-evil-setup)

                 ;; I treid to use general-create-definer and had some issues
                 ;; so for now general-def will do.
                 (general-def :states '(normal insert visual emacs)
                  :keymaps 'override
                  :prefix "SPC"
                  :global-prefix "M-SPC"
                  ;;
                  ;; general
                  ;; -------
                  "SPC" '(counsel-M-x :wk "Counsel M-x")
                  "." '(find-file :wk "Find file")
                  "f r" '(counsel-recentf :wk "Find recent files")
                  "TAB TAB" '(comment-line :wk "Comment lines")
                  ;;
                  ;; sudo on files
                  ;; -------------
                  "fu" '(sudo-edit-find-file :wk "Sudo find file")
                  "fU" '(sudo-edit :wk "Sudo edit file")
                  ;; Buffer
                  ;; -----
                  "b" '(:ignore t :wk "buffer")
                  "bb" '(switch-to-buffer :wk "Switch buffer")
                  "bi" '(ibuffer :wk "Ibuffer")
                  "bk" '(kill-this-buffer :wk "Kill this buffer")
                  "bn" '(next-buffer :wk "Next buffer")
                  "bp" '(previous-buffer :wk "Previous buffer")
                  "br" '(revert-buffer :wk "Reload buffer")
                  ;;
                  ;; emacs help
                  ;; ---
                  "h" '(:ignore t :wk "Help")
                  "h f" '(describe-function :wk "Describe function")
                  "h v" '(describe-variable :wk "Describe variable")
                  ;;
                  ;; Toggle
                  ;; ------
                  "t" '(:ignore t :wk "Toggle")
                  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
                  "t t" '(visual-line-mode :wk "Toggle truncated lines")
                  "t v" '(vterm-toggle :wk "Toggle vterm")
                  ;;
                  ;; Windows
                  ;; -------
                  "w" '(:ignore t :wk "Windows")
                  ;; Window splits
                  "w c" '(evil-window-delete :wk "Close window")
                  "w n" '(evil-window-new :wk "New window")
                  "w s" '(evil-window-split :wk "Horizontal split window")
                  "w v" '(evil-window-vsplit :wk "Vertical split window")
                  ;; Window motions
                  "w h" '(evil-window-left :wk "Window left")
                  "w j" '(evil-window-down :wk "Window down")
                  "w k" '(evil-window-up :wk "Window up")
                  "w l" '(evil-window-right :wk "Window right")
                  "w w" '(evil-window-next :wk "Goto next window")
                  ;; Move Windows
                  "w H" '(buf-move-left :wk "Buffer move left")
                  "w J" '(buf-move-down :wk "Buffer move down")
                  "w K" '(buf-move-up :wk "Buffer move up")
                  "w L" '(buf-move-right :wk "Buffer move right")
                  ;;
                  ;; org-mode keybindings
                  "m" '(:ignore t :wk "Org")
                  "m a" '(org-agenda :wk "Org agenda")
                  "m e" '(org-export-dispatch :wk "Org export dispatch")
                  "m i" '(org-toggle-item :wk "Org toggle item")
                  "m t" '(org-todo :wk "Org todo")
                  "m B" '(org-babel-tangle :wk "Org babel tangle")
                  "m T" '(org-todo-list :wk "Org todo list")
                  "m b" '(:ignore t :wk "Tables")
                  "m b -" '(org-table-insert-hline :wk "Insert hline in table")
                  "m d" '(:ignore t :wk "Date/deadline")
                  "m d t" '(org-time-stamp :wk "Org time stamp")

                  ;; Evaluate
                  ;; -------
                  "e" '(:ignore t :wk "Eshell/Evaluate")
                  "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
                  "ed" '(eval-defun :wk "Evaluate defun containing or after point")
                  "ee" '(eval-expression :wk "Evaluate and elisp expression")
                  "eh" '(counsel-esh-history :which-key "Eshell history")
                  "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
                  "er" '(eval-region :wk "Evaluate elisp in region")
                  "es" '(eshell :which-key "Eshell"))
            )

            ;; Iconns
            ;; ======
            (use-package all-the-icons
                :ensure t
                :if (display-graphic-p))

            (use-package all-the-icons-dired
                :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))


            ;; Dashboard
            ;; ============
            ;; DOTO: Right now I do not see icons in the welcome screen.
            (use-package dashboard
                :ensure t
                :init
                (setq initial-buffer-choice 'dashboard-open)
                (setq dashboard-set-heading-icons t)
                (setq dashboard-set-file-icons t)
                (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
                (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
                ;; I will need to first create it using home manager and it is a very
                ;; low priority. For the time being I will be using the defatul log for emacs.
                ;;(setq dashboard-startup-banner "some path to costum logo")  ;; use custom image as banner
                (setq dashboard-center-content nil) ;; set to 't' for centered content
                (setq dashboard-items '((recents . 5)
                                        (agenda . 5 )
                                        (bookmarks . 3)
                                        (projects . 3)
                                        (registers . 3)))
                :custom
                (dashboard-modify-heading-icons '((recents . "file-text")
                                                  (bookmarks . "book")))
                :config
                (dashboard-setup-startup-hook))

            (use-package diminish)

            (use-package projectile
                 :config
                 (projectile-mode 1))

            ;; Font settings
            ;; ============
            (set-face-attribute 'default nil
                :font "JetBrainsMono Nerd Font"
                :height 110
                :weight 'medium)
            (set-face-attribute 'variable-pitch nil
                :font "Ubuntu Nerd Font"
                :height 120
                :weight 'medium)
            (set-face-attribute 'fixed-pitch nil
                :font "JetBrainsMono Nerd Font"
                :height 110
                :weight 'medium)
            ;; Makes commented text and keywords italics.
            ;; This is working in emacsclient but not emacs.
            ;; Your font must have an italic face available.
            (set-face-attribute 'font-lock-comment-face nil
                :slant 'italic)
            (set-face-attribute 'font-lock-keyword-face nil
                :slant 'italic)

            ;; This sets the default font on all graphical frames created after restarting Emacs.
            ;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
            ;; are not right unless I also add this method of setting the default font.
            (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-11"))

            ;; Uncomment the following line if line spacing needs adjusting.
            (setq-default line-spacing 0.12)

            (menu-bar-mode -1)
            (tool-bar-mode -1)
            (scroll-bar-mode -1)

            (setq display-line-numbers-type 'relative)
            (global-display-line-numbers-mode 1)
            (global-visual-line-mode t)
            ;;================================

            ;; simple keys to zoom in and out
            ;; ==============================
            (global-set-key (kbd "C-=") 'text-scale-increase)
            (global-set-key (kbd "C--") 'text-scale-decrease)
            (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
            (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
            ;; =============================

            ;; ivy settings
            ;; ============
            (use-package counsel
                :after ivy
                :config (counsel-mode))

            (use-package ivy
                :bind
                ;; ivy-resume resumes the last Ivy-based completion.
                (("C-c C-r" . ivy-resume)
                 ("C-x B" . ivy-switch-buffer-other-window))
                :custom
                (setq ivy-use-virtual-buffers t)
                (setq ivy-count-format "(%d/%d) ")
                (setq enable-recursive-minibuffers t)
                :config
                (ivy-mode))

            (use-package all-the-icons-ivy-rich
                :ensure t
                :init (all-the-icons-ivy-rich-mode 1))

            (use-package ivy-rich
                :after ivy
                :ensure t
                :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
                :custom
                (ivy-virtual-abbreviate 'full
                 ivy-rich-switch-buffer-align-virtual-buffer t
                 ivy-rich-path-style 'abbrev)
                :config
                (ivy-set-display-transformer 'ivy-switch-buffer
                 'ivy-rich-switch-buffer-transformer))
            ;; ====================

            ;; org-mode enhancement
            ;; ====================
            (use-package toc-org
                :commands toc-org-enable
                :init (add-hook 'org-mode-hook 'toc-org-enable))

            (add-hook 'org-mode-hook 'org-indent-mode)
            (use-package org-bullets)
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

            (electric-indent-mode -1)
            (setq org-edit-src-content-indentation 0)

            ;; provides support for "easy org mode templates".
            (require 'org-tempo)

            (use-package rainbow-mode
                :hook
                ((org-mode prog-mode) . rainbow-mode))

            ;;=======================

            ;; shell settings
            ;;=======================
            ;; TODO: this setting made some erros and I will need to look into it.
            ;;(use-package eshell-syntax-highlighting
            ;;    :after esh-mode
            ;;    :config
            ;;    (eshell-syntax-highlighting-global-mode +1))
            ;;    ;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.

                ;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
                ;; eshell-aliases-file -- sets an aliases file for the eshell.
            (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
                eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
                eshell-history-size 5000
                eshell-buffer-maximum-lines 5000
                eshell-hist-ignoredups t
                eshell-scroll-to-bottom-on-input t
                eshell-destroy-buffer-when-process-dies t
                eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

            (use-package vterm
                :config
                (setq shell-file-name "zsh"
                 vterm-max-scrollback 5000))

            (use-package vterm-toggle
                :after vterm
                :config
                (setq vterm-toggle-fullscreen-p nil)
                (setq vterm-toggle-scope 'project)
                (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                         (let ((buffer (get-buffer buffer-or-name)))
                          (with-current-buffer buffer
                           (or (equal major-mode 'vterm-mode)
                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                     (display-buffer-reuse-window display-buffer-at-bottom)
                     ;;(display-buffer-reuse-window display-buffer-in-direction)
                     ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                     ;;(direction . bottom)
                     ;;(dedicated . t) ;dedicated is supported in emacs27
                     (reusable-frames . visible)
                     (window-height . 0.3))))



            ;;=======================


            (use-package which-key
                :init
                    (which-key-mode 1)
                :diminish
                :config
                    (setq which-key-side-window-location 'bottom
                    which-key-sort-order #'which-key-key-order-alpha
                    which-key-sort-uppercase-first nil
                    which-key-add-column-padding 1
                    which-key-max-display-columns nil
                    which-key-min-display-lines 6
                    which-key-side-window-slot -10
                    which-key-side-window-max-height 0.25
                    which-key-idle-delay 0.8
                    which-key-max-description-length 25
                    which-key-allow-imprecise-window-fit nil
                    which-key-separator " → " ))

            (load-theme 'nord t)
            (message "Loading init.el...")

            (require 'windmove)

            ;;;###autoload
            (defun buf-move-up ()
                "Swap the current buffer and the buffer above the split.
                If there is no split, ie now window above the current one, an
                error is signaled."
             ;;  "Switches between the current buffer, and the buffer above the
             ;;  split, if possible."
                (interactive)
                (let* ((other-win (windmove-find-other-window 'up))
                   (buf-this-buf (window-buffer (selected-window))))
                (if (null other-win)
                    (error "No window above this one")
                    ;; swap top with this one
                    (set-window-buffer (selected-window) (window-buffer other-win))
                    ;; move this one to top
                    (set-window-buffer other-win buf-this-buf)
                    (select-window other-win))))

            ;;;###autoload
            (defun buf-move-down ()
             "Swap the current buffer and the buffer under the split.
             If there is no split, ie now window under the current one, an
             error is signaled."
             (interactive)
             (let* ((other-win (windmove-find-other-window 'down))
                    (buf-this-buf (window-buffer (selected-window))))
              (if (or (null other-win)
                   (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
               (error "No window under this one")
               ;; swap top with this one
               (set-window-buffer (selected-window) (window-buffer other-win))
               ;; move this one to top
               (set-window-buffer other-win buf-this-buf)
               (select-window other-win))))

            ;;;###autoload
            (defun buf-move-left ()
             "Swap the current buffer and the buffer on the left of the split.
             If there is no split, ie now window on the left of the current
             one, an error is signaled."
             (interactive)
             (let* ((other-win (windmove-find-other-window 'left))
                    (buf-this-buf (window-buffer (selected-window))))
              (if (null other-win)
               (error "No left split")
               ;; swap top with this one
               (set-window-buffer (selected-window) (window-buffer other-win))
               ;; move this one to top
               (set-window-buffer other-win buf-this-buf)
               (select-window other-win))))

            ;;;###autoload
            (defun buf-move-right ()
             "Swap the current buffer and the buffer on the right of the split.
             If there is no split, ie now window on the right of the current
             one, an error is signaled."
             (interactive)
             (let* ((other-win (windmove-find-other-window 'right))
                    (buf-this-buf (window-buffer (selected-window))))
              (if (null other-win)
               (error "No right split")
               ;; swap top with this one
               (set-window-buffer (selected-window) (window-buffer other-win))
               ;; move this one to top
               (set-window-buffer other-win buf-this-buf)
               (select-window other-win))))
        '';
    };

    services.emacs = {
        enable = true;
        package = pkgs.emacs; # replace with emacs-gtk, or a version provided by the community overlay if desired.
    };

}

