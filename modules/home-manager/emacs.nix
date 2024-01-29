{ pkgs, config, ... }:
let
    #xdg.dataHome used ro specify a path while xdg.dataFile used for creating files
    # in the same path.
    autoSaveDirectoryName = "emacs_autosave";
    autoSaveDirectoryPath = "${config.xdg.dataHome}/${autoSaveDirectoryName}";
    autoBackupDirectoryPatch = "${config.xdg.dataHome}/Trash/files";
    orgRoamDirctoryPath = "~/Sync/dev_resources/roam_notes";

in
{

    # This is a fix to make the system use universal-ctags package instead
    # of the ctags package that is provided by emacs. The order in which
    # the packges are listed is important and by listing universal-ctags first
    # I am making sure that it will be the one used by the system.
    #
    # Note I am not sure if this is the right way to go about this problem
    # and I am not sure how this will effect Emacs when it will try to call
    # ctags.
    home.packages = with pkgs; [
        universal-ctags

        # support for LaTeX
        texlive.combined.scheme-full

        # search
        ripgrep
        fd
        fzf

        plantuml
        graphviz
        jdk11
    ];

    # Will create the dedined path for emacs auto save
    # an empty .keep file is a workaround to create an empty directory.
    xdg.dataFile."${autoSaveDirectoryName}/.keep".text = "";

    services.emacs.enable = true;

    programs.emacs = {
        enable = true;
        package = pkgs.emacs29;

        extraPackages = epkgs: with epkgs; [
            use-package
            evil
            evil-collection
            general
            toc-org
            org-bullets
            which-key
            sudo-edit
            counsel
            ivy
            all-the-icons-ivy-rich
            ivy-rich
            counsel
            prescient
            ivy-prescient
            all-the-icons
            all-the-icons-dired
            dired-open
            peep-dired
            neotree
            eshell-syntax-highlighting
            vterm
            vterm-toggle
            rainbow-mode
            rainbow-delimiters
            dashboard
            projectile
            diminish
            lua-mode
            flycheck
            lsp-mode
            company
            company-box
            nord-theme
            vscode-dark-plus-theme
            doom-themes
            doom-modeline
            elfeed
            elfeed-goodies
            git-timemachine
            magit
            hl-todo
            tldr
            org-roam
            org-download
            org-tree-slide
            undo-tree
            ox-jira
            plantuml-mode
            websocket
            org-roam-ui
        ];
    };

   # programs.emcas.extraConfig is responsible to create a configuration file for
   # Emacs but this file, although interpreted as init.el(in logs) isn't located in ~/.emacs.d.
   # And although it gets picked up by Emacs this does not work so well for Emacs daemon that
   # is executed as a systemd unit.
   #
   # So I just created ~/.emacs.d/init.el using home.file directive.
   home.file = {
     ".emacs.d/init.el".text = ''
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
            (add-hook 'org-mode-hook (lambda ()
                           (message "org-mode hook executed")
                           (org-redisplay-inline-images)))

            ;; Set UTF-8 as the default encoding
            (prefer-coding-system 'utf-8)
            (set-default-coding-systems 'utf-8)
            (set-terminal-coding-system 'utf-8)
            (set-keyboard-coding-system 'utf-8)

            ;; smoth scroll with mergin of lines
            ;; =================================
            (setq scroll-margin 8)
            (setq-default scroll-conservatively 100 scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)
            (pixel-scroll-mode 1)
            (pixel-scroll-precision-mode 1)
            ;; =================================

            ;; plantuml setitngs
            ;; =================================

            ;; general settings for plantuml
            ;; mostly used for standalone work with .plantuml files.
            ;; -------------------------------------
            (use-package plantuml-mode
             :ensure t
             :mode ("\\.plantuml\\'" . plantuml-mode)
             :config
             ;; If you have the PlantUML executable instead of a jar file, set the command

             (setq plantuml-executable-path "${pkgs.plantuml}/bin/plantuml")
             (setq plantuml-default-exec-mode 'executable))

            ;; function to show preview of diagram on every save of .plantuml file.
            (defun plantuml-preview-on-save ()
             (when (eq major-mode 'plantuml-mode)
              (plantuml-preview-buffer 4)))

            (add-hook 'after-save-hook 'plantuml-preview-on-save)
            ;; -------------------------------------

            ;; org-mode integration
            ;; -------------------------------------
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((plantuml . t)))

            ;; note that org-mode only supports the use of plantuml.jar
            ;; and there is no org-* variable to set plantuml like I did for standalone .plantuml
            ;; Set the command for Org-mode to use PlantUML
            (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
            ;; Tell Org-mode to use the jar method
            (setq org-plantuml-exec-mode 'jar)
            ;; =================================

            (use-package ox-jira)
            (use-package undo-tree
             :ensure t)
             (global-undo-tree-mode)

            ;; Set evil-undo-system to use undo-tree
            (setq evil-undo-system 'undo-tree)

            (setq org-image-actual-width nil)
            (require 'org-tree-slide)

            ;; Custom keybindings for navigating slides
            (with-eval-after-load "org-tree-slide"
             (define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-mode)
             (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
             (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

            ;; Start or stop the slide mode with F8
            (global-set-key (kbd "<f8>") 'org-tree-slide-mode)

            ;; Move to the previous slide with F9
            (global-set-key (kbd "<f9>") 'org-tree-slide-move-previous-tree)

            ;; Move to the next slide with F10
            (global-set-key (kbd "<f10>") 'org-tree-slide-move-next-tree)

            ;; Optional: Start with a simple profile
            ;; Uncomment the following line if you want to use the simple profile by default
            ;; (org-tree-slide-simple-profile)

            (use-package org-download
             :ensure t
             :config
             ;; where to save files?
             ;; when  org-download-method is set to "attach"
             ;; then org-roam will handle where to store the
             ;; file and creat place for it.
             ;; is you want to specify the directory then
             ;; org-download-method needs to be chagned to directory
             ;; and you will need to set org-download-image-dir, E.g:
             ;; Set the default directory where images will be downloaded
             ;;(setq-default org-download-image-dir "${orgRoamDirctoryPath}/images/")

             ;; Set the method for handling downloaded images
             ;; 'attach integrates with Org's attachment system
             (setq org-download-method 'attach)

             ;; Ensure the org-download package is loaded
             (require 'org-download))

            (with-eval-after-load 'org
                (setq org-agenda-files '("${orgRoamDirctoryPath}")))
            (setq org-todo-keywords
                '((sequence "TODO" "|" "DONE")
                  (sequence "QUESTION" "|" "ANSWERED")))

            (use-package org-roam
             :ensure t
             :custom
             (org-roam-directory (file-truename "${orgRoamDirctoryPath}"))
             :bind (("C-c n l" . org-roam-buffer-toggle)
                    ("C-c n f" . org-roam-node-find)
                    ("C-c n g" . org-roam-graph)
                    ("C-c n i" . org-roam-node-insert)
                    ("C-c n c" . org-roam-capture)
                    ;; Dailies
                    ("C-c n j" . org-roam-dailies-capture-today))
             :config
             ;; puting the remplate setitngs under :costum did not work for me.
             (setq org-roam-capture-templates
              '(
                  ("d" "default" plain "* Table of Contents :toc:noexport:\n\n%?"
                   :target (file+head "%<%d%m%Y%H%M%S>-''${slug}.org"
                       "#+date: [%<%d-%m-%Y %a %H:%M>]\n#+category: ''${title}\n#+filetags:\n#+title: ''${title}\n") :unnarrowed t)
                  ("s" "story" plain "* Table of Contents :toc:noexport:\n\n* Resources\n** [%?[][jira]]\n\n* Overview\n\n* AC\n\n* Testing\n\n"
                   :target (file+head "class/%<%d%m%Y%H%M%S>-''${slug}.org"
                       "#+date: [%<%d-%m-%Y %a %H:%M>]\n#+category: ''${title}\n#+filetags:\n#+title: ''${title}\n") :unnarrowed t)
                  )
                  )
             ;; Daily notes (journals) templates
             (setq org-roam-dailies-capture-templates
              '(("j" "journal" entry "* %<%I:%M %p> - %?"
                      :target (file+head "%<%d-%m-%Y>.org"
                          "#+title: %<%d-%m-%Y>\n"))))

             ;; If you're using a vertical completion framework, you might want a more informative completion interface
             (setq org-roam-node-display-template (concat "''${title:*} " (propertize "''${tags:10}" 'face 'org-tag)))
             (org-roam-db-autosync-mode)
             ;; If using org-roam-protocol
             (require 'org-roam-protocol))



             (use-package websocket
             :after org-roam)

             (use-package org-roam-ui
              :after org-roam ;; or :after org
              ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
              ;;         a hookable mode anymore, you're advised to pick something yourself
              ;;         if you don't care about startup time, use
              ;;  :hook (after-init . org-roam-ui-mode)
              :config
              (setq org-roam-ui-sync-theme t
               org-roam-ui-follow t
               org-roam-ui-update-on-save t
               org-roam-ui-open-on-start t))

            ;; setup spell checker
            ;; ===================
            (setq ispell-program-name "aspell")
            ;; For text modes(md, txt, org):
            (add-hook 'text-mode-hook 'flyspell-mode)

            ;; Specifically for programming modes to check comments and strings:
            (add-hook 'prog-mode-hook 'flyspell-prog-mode)


            ;; ===================

            ;; By default, Emacs creates automatic backups of files in their
            ;; original directories, such “file.el” and the backup “file.el~”.
            ;; This leads to a lot of clutter, so this setting defines a single
            ;; path to put all the backup into.
            (setq backup-directory-alist '((".*" . "${autoBackupDirectoryPatch}")))



            ;; By default, Emacs creates automatic saves of files in their
            ;; original directories, such “file.el” and the autosave “#file.el#”.
            ;; This leads to a lot of clutter, so this setting defines a single
            ;; path to put all the autosave files.
            (setq auto-save-file-name-transforms '((".*" "${autoSaveDirectoryPath}" t)))

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
                ;; evil-collection assumes evil-want-keybinding is set to nil and
                ;; evil-want-integration is set to t before loading evil and evil-collection.
                ;; Note some other packages may load evil (e.g. evil-leader) so bear that
                ;; in mind when determining when to set the variables.
                (setq evil-want-integration t) ;; This is optional since it's already set to t by default
                (setq evil-want-keybinding nil)

                (setq evil-vsplit-window-right t)
                (setq evil-split-window-below t)
                :config
                (evil-mode 1)

                ;; Define the custom function
                (defun save-and-switch-to-normal-mode ()
                 "Save the current buffer and switch to normal mode."
                 (interactive)
                 (save-buffer)
                 (evil-normal-state))

                ;; Set the key bindings for normal and insert modes
                (define-key evil-normal-state-map (kbd "C-s") 'save-and-switch-to-normal-mode)
                (define-key evil-insert-state-map (kbd "C-s") 'save-and-switch-to-normal-mode))

                ;; recenter org around search string
                ;; same as:
                ;;vim.keymap.set('n', 'n', "nzzzv", opts)
                ;; --------------------------------------
                (defadvice evil-search-next
                 (after advice-for-evil-search-next activate)
                 (evil-scroll-line-to-center (line-number-at-pos)))

                (defadvice evil-search-previous
                 (after advice-for-evil-search-previous activate)
                 (evil-scroll-line-to-center (line-number-at-pos)))
                ;; --------------------------------------

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

            (with-eval-after-load 'evil-maps
             (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up))


            (evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
            (evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
            (evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
            (evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)
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

            (use-package sudo-edit)

            (defun insert-active-timestamp ()
             "Inserts an active timestamp with the current date and time up to minutes."
             (interactive)
             (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))

            (defun insert-inactive-timestamp ()
             "Inserts an inactive timestamp with the current date and time up to minutes."
             (interactive)
             (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))


            ;; A more vim like keybindis for flyspell
            ;;---------------------------------------
            ;; dependence on evil/evil-collection
            (defun costum-flyspell-previous-and-correct ()
             "Correct previous misspelling or current if already on a misspelled word."
             (interactive)
             (unless (flyspell-overlay-p (point))
              (evil-prev-flyspell-error))
             (flyspell-correct-word-before-point))

            (defun costum-flyspell-next-and-correct ()
             "Correct next misspelling or current if already on a misspelled word."
             (interactive)
             (unless (flyspell-overlay-p (point))
              (evil-next-flyspell-error))
             (flyspell-correct-word-before-point))

            (general-def :states 'normal
             "]s" 'costum-flyspell-next-and-correct
             "[s" 'costum-flyspell-previous-and-correct)
            ;;---------------------------------------

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
                  "f f" '(counsel-fzf :wk "Find files")
                  "f s" '(counsel-rg :wk "Find string")
                  "f b" '(counsel-switch-buffer :wk "Find buffer")
                  "f h" '(counsel-org-goto :wk "Find org file header")
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
                  ;; For the time being the wk(which key) will not work and "lambda" will be displayed.

                  ;;
                  ;; Toggle
                  ;; ------
                  "t" '(:ignore t :wk "Toggle")
                  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
                  "t t" '(visual-line-mode :wk "Toggle truncated lines")
                  "t v" '(vterm-toggle :wk "Toggle vterm")
                  ;;
                  ;; Search
                  ;; ------
                  "s" '(:ignore t :wk "Search")
                  "s d" '(dictionary-search :wk "Search dictionary")
                  "s m" '(man :wk "Man pages")
                  "s t" '(tldr :wk "Lookup TLDR docs for a command")
                  "s w" '(woman :wk "Similar to man but doesn't require man")

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
                  "m l" '(org-toggle-item :wk "Org toggle list item")
                  "m u" '(org-roam-ui-mode :wk "toggle org-roam-ui-mode")
                  "m t" '(org-todo :wk "Org todo")
                  "m B" '(org-babel-tangle :wk "Org babel tangle")
                  "m T" '(org-todo-list :wk "Org todo list")
                  "m b" '(:ignore t :wk "Tables")
                  "m b -" '(org-table-insert-hline :wk "Insert hline in table")
                  "m d" '(:ignore t :wk "Date/deadline")
                  "m d t" '(insert-inactive-timestamp :which-key "Insert inactive timestamp")
                  "m d a" '(insert-active-timestamp :which-key "Insert active timestamp")
                  "m h" '(org-toggle-heading :wk "Org toggle heading")
                  "m i" '(:ignore t :wk "Org insert")
                  "m i l" '(org-insert-link :wk "Insert Org link")
                  "m i n" '(org-roam-node-insert :which-key "org-roam node insert")
                  "m i i" '(org-id-get-create :which-key "assign Org-roam ID to heading")
                  "m i c" '(org-download-clipboard :which-key "insert clipboard")



                  ;; Org-roam
                  "m r" '(org-roam-buffer-toggle :which-key "list node refernces")
                  "m f" '(org-roam-node-find :which-key "org-roam node find")
                  "m g" '(org-roam-graph :which-key "org-roam graph")
                  "m c" '(org-roam-capture :which-key "org-roam capture")
                  "m j" '(org-roam-dailies-capture-today :which-key "org-roam dailies capture today")

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
                  "ec" '(lambda () (interactive) (load-file user-init-file)) :wk "Reload config"
                  "ed" '(eval-defun :wk "Evaluate defun containing or after point")
                  "ee" '(eval-expression :wk "Evaluate and elisp expression")
                  "eh" '(counsel-esh-history :which-key "Eshell history")
                  "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
                  "er" '(eval-region :wk "Evaluate elisp in region")
                  "es" '(eshell :which-key "Eshell"))
            )

            (global-set-key (kbd "C-c c") 'org-capture)
            (setq org-capture-templates
             '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
                     "* TODO %?\n  %i\n  %a")
                 ("j" "Journal" entry (file+datetree "~/org/journal.org")
                  "* %?\nEntered on %U\n  %i\n  %a")))

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
                (setq dashboard-banner-logo-title "Emacs is a great operating system, lacking only a decent text editor")
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

            ;; When used by other plugins, can hide thier
            ;; abbreviation from the modeline.
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

            (setq display-line-numbers-type 'relative)
            ;;================================

            ;; simple keys to zoom in and out
            ;; ==============================
            (global-set-key (kbd "C-=") 'text-scale-increase)
            (global-set-key (kbd "C--") 'text-scale-decrease)
            (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
            (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
            ;; =============================

            ;; Org mode Meta keybinding
            ;; ==============================

            (with-eval-after-load 'org
             ;; Ensure "M-h" is unbound in org-mode specifically
             (define-key org-mode-map (kbd "M-h") nil)
             (define-key org-mode-map (kbd "M-h") 'org-promote-subtree)

             ;; Set "M-l" for demoting subtree globally
             (define-key org-mode-map (kbd "M-l") 'org-demote-subtree)

             ;; Bind keys for moving headings up and down in Org mode
             (define-key org-mode-map (kbd "M-j") 'org-move-subtree-down)
             (define-key org-mode-map (kbd "M-k") 'org-move-subtree-up))
            ;; ==============================

            ;; ivy settings
            ;; ============

            (use-package prescient
             :config
             (prescient-persist-mode 1))  ; Remember usage statistics across Emacs sessions

            (use-package ivy-prescient
             :after ivy
             :config
             (ivy-prescient-mode 1)  ; Enable ivy-prescient
             (setq ivy-prescient-retain-classic-highlighting t)  ; Optional: retain Ivy's highlighting style
             ;; Set regex builder for counsel-rg to ivy--regex-plus
             ;; without this I was getting error code: 2 when using counsel-rg with ivy-prescient
             (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))

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


                 (use-package counsel
                  :ensure t
                  :after ivy
                  :config
                  (setq ivy-use-virtual-buffers t)
                  (setq enable-recursive-minibuffers t)
                  ;; Bind `counsel-M-x' to M-x
                  (global-set-key (kbd "M-x") 'counsel-M-x))

            (use-package all-the-icons-ivy-rich
                :ensure t
                :init (all-the-icons-ivy-rich-mode 1))

            (use-package ivy-rich
             :after ivy
             :ensure t
             :init
             (ivy-rich-mode 1)  ;; Enable ivy-rich-mode
             :config
             ;; Recommended setting
             (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
             ;; Customize path style
             (setq ivy-rich-path-style 'abbrev))

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

            ;; mini buffer escape
            (global-set-key [escape] 'keyboard-escape-quit)

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
            ;; in in essence this is the popup box with completion suggestions.
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

            ;; Make company-mode work with org-mode's org-self-insert-command
            (with-eval-after-load 'company
             (add-to-list 'company-begin-commands 'org-self-insert-command))

            ;; Function to integrate pcomplete with company-capf
            (defun add-pcomplete-to-capf ()
             (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

            ;; Adding pcomplete integration to org-mode
            (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

            ;; Specific company backends setup for org-mode
            (add-hook 'org-mode-hook (lambda ()
                                      (setq-local company-backends '(company-capf company-dabbrev))))

            ;; ====================

            ;; org-mode enhancement
            ;; ====================
            (setq org-startup-with-inline-images t)
            (use-package toc-org
                :commands toc-org-enable
                :init (add-hook 'org-mode-hook 'toc-org-enable))

            (add-hook 'org-mode-hook 'org-indent-mode)
            (use-package org-bullets)
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

            ;; provides support for "easy org mode templates".
            ;; this is what makes <s (among other things) to work.
            (require 'org-tempo)

            ;;=======================

            ;; color enhancement
            ;; ==============
            (use-package rainbow-mode
                :hook
                ((org-mode prog-mode) . rainbow-mode))

            (use-package rainbow-delimiters
                :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
                (clojure-mode . rainbow-delimiters-mode)))
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

            ;; Sane defaults
            ;;=======================
            (delete-selection-mode 1)    ;; You can select text and delete it by typing.
            (electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
            (electric-pair-mode 1)       ;; Turns on automatic parens pairing
            ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
            ;; Otherwise, org-tempo is broken when you try to <s TAB...
            (add-hook 'org-mode-hook (lambda ()
                                      (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
            (global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
            (global-display-line-numbers-mode 1) ;; Display line numbers
            (global-visual-line-mode t)  ;; Enable truncated lines
            (menu-bar-mode -1)           ;; Disable the menu bar
            (scroll-bar-mode -1)         ;; Disable the scroll bar
            (tool-bar-mode -1)           ;; Disable the tool bar
            (setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
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
}

