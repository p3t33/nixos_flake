{ pkgs, ... }:
{
    programs.emacs = {
        enable = true;
        package = pkgs.emacs29;

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
            epkgs.dired-open
            epkgs.peep-dired
            epkgs.neotree
            epkgs.eshell-syntax-highlighting
            epkgs.vterm
            epkgs.vterm-toggle
            epkgs.rainbow-mode
            epkgs.dashboard
            epkgs.projectile
            epkgs.diminish
            epkgs.lua-mode
            epkgs.flycheck
            epkgs.lsp-mode
            epkgs.company
            epkgs.company-box
            epkgs.nord-theme
            epkgs.vscode-dark-plus-theme
            epkgs.doom-themes
            epkgs.doom-modeline
            epkgs.elfeed
            epkgs.elfeed-goodies
            epkgs.git-timemachine
            epkgs.magit
            epkgs.hl-todo
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


            ;; By default, Emacs creates automatic backups of files in their
            ;; original directories, such “file.el” and the backup “file.el~”.
            ;; This leads to a lot of clutter, so this setting defines a single
            ;; path to put all the backup into.
            (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

            ;; Ensure the use-package macro is available
            ;; This needs to be the first plugin related settings as
            ;; every pluging that uses this macro in order to use it
            ;; for its own configuration needs to be loaded only after
            ;; this macro has been loaded.
            (require 'use-package)

            ;; Configure and initialize evil-mode
            ;; =================================
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

            ;; Using RETURN to follow links in Org/Evil
            ;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
            (with-eval-after-load 'evil-maps
                (define-key evil-motion-state-map (kbd "SPC") nil)
                (define-key evil-motion-state-map (kbd "RET") nil)
                (define-key evil-motion-state-map (kbd "TAB") nil))
            ;; Setting RETURN key in org-mode to follow links
            (setq org-return-follows-link  t)
            ;; =================================

            ;; git
            ;; ============
            (use-package git-timemachine
                :after git-timemachine
                :hook (evil-normalize-keymaps . git-timemachine-hook)
                :config
                    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
                    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
            )
            (use-package magit)
            ;; ============

            (use-package magit)

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

                  ;; Buffer/bookmarks
                  ;; -----
                  "b" '(:ignore t :wk "Bookmarks/Buffers")
                  "b b" '(counsel-switch-buffer :wk "Switch buffer")
                  "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
                  "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
                  "b d" '(bookmark-delete :wk "Delete bookmark")
                  "b i" '(ibuffer :wk "Ibuffer")
                  "b k" '(kill-this-buffer :wk "Kill this buffer")
                  "b K" '(kill-some-buffers :wk "Kill multiple buffers")
                  "b l" '(list-bookmarks :wk "List bookmarks")
                  "b m" '(bookmark-set :wk "Set bookmark")
                  "b n" '(next-buffer :wk "Next buffer")
                  "b p" '(previous-buffer :wk "Previous buffer")
                  "b r" '(revert-buffer :wk "Reload buffer")
                  "b R" '(rename-buffer :wk "Rename buffer")
                  "b s" '(basic-save-buffer :wk "Save buffer")
                  "b S" '(save-some-buffers :wk "Save multiple buffers")
                  "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
                  "b y" '(yank-buffer :wk "Yank buffer")

                  ;; git
                  ;; ---------
                  "g" '(:ignore t :wk "Git")
                  "g /" '(magit-displatch :wk "Magit dispatch")
                  "g ." '(magit-file-displatch :wk "Magit file dispatch")
                  "g b" '(magit-branch-checkout :wk "Switch branch")
                  "g c" '(:ignore t :wk "Create")
                  "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
                  "g c c" '(magit-commit-create :wk "Create commit")
                  "g c f" '(magit-commit-fixup :wk "Create fixup commit")
                  "g C" '(magit-clone :wk "Clone repo")
                  "g f" '(:ignore t :wk "Find")
                  "g f c" '(magit-show-commit :wk "Show commit")
                  "g f f" '(magit-find-file :wk "Magit find file")
                  "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
                  "g F" '(magit-fetch :wk "Git fetch")
                  "g g" '(magit-status :wk "Magit status")
                  "g i" '(magit-init :wk "Initialize git repo")
                  "g l" '(magit-log-buffer-file :wk "Magit buffer log")
                  "g r" '(vc-revert :wk "Git revert file")
                  "g s" '(magit-stage-file :wk "Git stage file")
                  "g t" '(git-timemachine :wk "Git time machine")
                  "g u" '(magit-stage-file :wk "Git unstage file")

                  ;; emacs help
                  ;; ----------
                  "h" '(:ignore t :wk "Help")
                  "h a" '(counsel-apropos :wk "Apropos")
                  "h b" '(describe-bindings :wk "Describe bindings")
                  "h c" '(describe-char :wk "Describe character under cursor")
                  "h d" '(:ignore t :wk "Emacs documentation")
                  "h d a" '(about-emacs :wk "About Emacs")
                  "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
                  "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
                  "h d m" '(info-emacs-manual :wk "The Emacs manual")
                  "h d n" '(view-emacs-news :wk "View Emacs news")
                  "h d o" '(describe-distribution :wk "How to obtain Emacs")
                  "h d p" '(view-emacs-problems :wk "View Emacs problems")
                  "h d t" '(view-emacs-todo :wk "View Emacs todo")
                  "h d w" '(describe-no-warranty :wk "Describe no warranty")
                  "h e" '(view-echo-area-messages :wk "View echo area messages")
                  "h f" '(describe-function :wk "Describe function")
                  "h F" '(describe-face :wk "Describe face")
                  "h g" '(describe-gnu-project :wk "Describe GNU Project")
                  "h i" '(info :wk "Info")
                  "h I" '(describe-input-method :wk "Describe input method")
                  "h k" '(describe-key :wk "Describe key")
                  "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
                  "h L" '(describe-language-environment :wk "Describe language environment")
                  "h m" '(describe-mode :wk "Describe mode")
                  "h r" '(:ignore t :wk "Reload")
                  "h t" '(load-theme :wk "Load theme")
                  "h v" '(describe-variable :wk "Describe variable")
                  "h w" '(where-is :wk "Prints keybinding for command if set")
                  "h x" '(describe-command :wk "Display full documentation for command")

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

                  ;; dired
                  ;; -----
                  "d" '(:ignore t :wk "Dired")
                  "d d" '(dired :wk "Open dired")
                  "d j" '(dired-jump :wk "Dired jump to current")
                  "d n" '(neotree-dir :wk "Open directory in neotree")
                  "d p" '(peep-dired :wk "Peep-dired")

                  ;; Open
                  ;; ----
                  "o" '(:ignore t :wk "Open")
                  "o d" '(dashboard-open :wk "Dashboard")
                  "o e" '(elfeed :wk "Elfeed RSS")
                  "o f" '(make-frame :wk "Open buffer in new frame")
                  "o F" '(select-frame-by-name :wk "Select frame by name")

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


            ;; Adding highlighting to TODO and related words
            (use-package hl-todo
                :hook ((org-mode . hl-todo-mode)
                    (prog-mode . hl-todo-mode))
                :config
                    (setq hl-todo-highlight-punctuation ":"
                     hl-todo-keyword-faces
                     `(("TODO"       warning bold)
                      ("FIXME"      error bold)
                      ("HACK"       font-lock-constant-face bold)
                      ("REVIEW"     font-lock-keyword-face bold)
                      ("NOTE"       success bold)
                      ("DEPRECATED" font-lock-doc-face bold))))

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

            ;; RSS feed
            ;; ====================
            (use-package elfeed
                :config
                (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
                    elfeed-feeds (quote
                        (("https://www.reddit.com/r/linux.rss" reddit linux)
                         ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                         ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                         ("https://hackaday.com/blog/feed/" hackaday linux)
                         ("https://opensource.com/feed" opensource linux)
                         ("https://linux.softpedia.com/backend.xml" softpedia linux)
                         ("https://www.computerworld.com/index.rss" computerworld linux)
                         ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                         ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                         ("http://lxer.com/module/newswire/headlines.rss" lxer linux)))))

            (use-package elfeed-goodies
                :init
                (elfeed-goodies/setup)
                :config
                (setq elfeed-goodies/entry-pane-size 0.5))
            ;; ====================

            ;; Language support
            ;; ====================
            ;;
            ;; Syntax static error checking
            ;; -------------------
            (use-package flycheck
                :ensure t
                :defer t
                :diminish
                :init (global-flycheck-mode))

            ;; syntax highlighting
            ;; -------------------
            ;; Not all languages are supported by default such as lua.
            (use-package lua-mode)

            ;; Language Servers (LSP)
            ;; ----------------------
            ;; this is the client side support for something like clgand
            ;; server.
            (use-package lsp-mode
                :hook ((c-mode c++-mode) . lsp)
                :commands lsp)

            ;; company can be used to provide the frontend for code
            ;; completions that the Language Server (like clangd for C++)
            ;; sends to the client (in this case, lsp-mode in Emacs).
            (use-package company
                :defer 2
                :diminish
                :custom
                (company-begin-commands '(self-insert-command))
                (company-idle-delay .1)
                (company-minimum-prefix-length 2)
                (company-show-numbers t)
                (company-tooltip-align-annotations 't)
                (global-company-mode t))

            ;; company-box is an extension for company that provides a visually
            ;; enhanced dropdown for completions. It makes the completion menu
            ;; more aesthetically pleasing and can display additional information,
            ;; such as icons for different types of completion
            ;; candidates (e.g., a function, variable, or class).
            (use-package company-box
                :after company
                :diminish
                :hook (company-mode . company-box-mode))
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

            ;; neotree
            ;; =====================
            (use-package neotree
                :config
                (setq neo-smart-open t
                 neo-show-hidden-files t
                 neo-window-width 55
                 neo-window-fixed-size nil
                 inhibit-compacting-font-caches t
                 projectile-switch-project-action 'neotree-projectile-action)
                ;; truncate long file names in neotree
                (add-hook 'neo-after-create-hook
                    #'(lambda (_)
                        (with-current-buffer (get-buffer neo-buffer-name)
                         (setq truncate-lines t)
                         (setq word-wrap nil)
                         (make-local-variable 'auto-hscroll-mode)
                         (setq auto-hscroll-mode nil)))))

            ;; dired
            ;; =====================
            ;; used to open files that are not text using the desired software
            ;; this is mostly an example a I did not installed sxiv or mpv.
            (use-package dired-open
                :config
                (setq dired-open-extensions '(("gif" . "sxiv")
                                              ("jpg" . "sxiv")
                                              ("png" . "sxiv")
                                              ("mkv" . "mpv")
                                              ("mp4" . "mpv"))))

            ;; Used to provide a preview of a file.
            ;; looks like this plugin is archived.
            (use-package peep-dired
            :after dired
            :hook (evil-normalize-keymaps . peep-dired-hook)
            :config
                (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
                (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
                (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
                (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

            ;; =====================

            ;; Theme
            ;; =====================
            ""
            (use-package doom-themes
                :config
                (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
                 doom-themes-enable-italic t)) ; if nil, italics is universally disabled

            ;; this is the line that loads the theme(E.g 'vscode-dark-plus)
            ;; TODO: I had a few warnings when using a theme from the pack so at least for now
            ;; I will be using a theme that I had no warning for as this is a low priority.
            ;; TODO: My current theme gets a few warning
            (load-theme 'doom-one t)
            ;; =====================

            ;; Status bar
            ;; =========
            (use-package doom-modeline
                :ensure t
                :init (doom-modeline-mode 1)
                :config
                (setq doom-modeline-height 35      ;; sets modeline height
                 doom-modeline-bar-width 5    ;; sets right bar width
                 doom-modeline-persp-name t   ;; adds perspective name to modeline
                 doom-modeline-persp-icon t)) ;; adds folder icon next to persp name
            ;; =====================

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
        package = pkgs.emacs29;
    };

}

