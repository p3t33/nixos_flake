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
                  "." '(find-file :wk "Find file")
                  "TAB TAB" '(comment-line :wk "Comment lines")
                  ;;
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
                  ;;
                  ;; Evaluate
                  ;; -------
                  "e" '(:ignore t :wk "Evaluate")
                  "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
                  "ed" '(eval-defun :wk "Evaluate defun containing or after point")
                  "ee" '(eval-expression :wk "Evaluate and elisp expression")
                  "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
                  "er" '(eval-region :wk "Evaluate elisp in region"))
            )

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

            ;; org-mode enhancement
            ;; ====================
            (use-package toc-org
                :commands toc-org-enable
                :init (add-hook 'org-mode-hook 'toc-org-enable))

            (add-hook 'org-mode-hook 'org-indent-mode)
            (use-package org-bullets)
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

            (electric-indent-mode -1)
            ;; provides support for "easy templates".
            (require 'org-tempo)
            ;;=======================


            (use-package which-key
                :init
                    (which-key-mode 1)
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
                    which-key-allow-imprecise-window-fit t
                    which-key-separator " → " ))



            (load-theme 'nord t)
            (message "Loading init.el...")

        '';
    };

    services.emacs = {
        enable = true;
        package = pkgs.emacs; # replace with emacs-gtk, or a version provided by the community overlay if desired.
    };

}

