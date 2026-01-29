{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs.extraPackages = epkgs: with epkgs; [
    all-the-icons
    all-the-icons-dired
    dashboard
    diminish
    doom-modeline
    doom-themes
    hl-todo
    org-bullets
    rainbow-delimiters
    rainbow-mode
    which-key
  ];

  programs.emacs.extraConfig = ''
      ;; ============
      ;; Font settings
      ;; ============
      (set-face-attribute 'default nil
          :font "${config.customGlobal.font.mono}"
          :height 110
          :weight 'medium)
      (set-face-attribute 'variable-pitch nil
          :font "${config.customGlobal.font.sansSerif}"
          :height 120
          :weight 'medium)
      (set-face-attribute 'fixed-pitch nil
          :font "${config.customGlobal.font.mono}"
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
      (add-to-list 'default-frame-alist '(font . "${config.customGlobal.font.mono}-16"))
      ;;================================

      ;; When used by other plugins, can hide thier
      ;; abbreviation from the modeline.
      (use-package diminish
          :ensure nil)

      ;; =====================
      ;; Theme
      ;; =====================
      (use-package doom-themes
          :ensure nil
          :init
          (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
           doom-themes-enable-italic t)) ; if nil, italics is universally disabled

          :config
          ;; this is the line that loads the theme(E.g 'vscode-dark-plus)
          ;; TODO: I had a few warnings when using a theme from the pack so at least for now
          ;; I will be using a theme that I had no warning for as this is a low priority.
          ;; TODO: My current theme gets a few warning
          (load-theme 'doom-one t)
      ;; =====================

      ;; =========
      ;; Status bar
      ;; =========
      (use-package doom-modeline
          :ensure nil
          :after doom-themes
          :init (doom-modeline-mode 1)
          :config
          (setq doom-modeline-height 35      ;; sets modeline height
           doom-modeline-bar-width 5    ;; sets right bar width
           doom-modeline-persp-name t   ;; adds perspective name to modeline
           doom-modeline-persp-icon t)) ;; adds folder icon next to persp name
      ;; =====================


      ;; ======
      ;; Iconns
      ;; ======
      (use-package all-the-icons
          :ensure nil
          :if (display-graphic-p))

      (use-package all-the-icons-dired
          :after all-the-icons
          :ensure nil
          :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))
      ;; ======

      ;; ============
      ;; Dashboard
      ;; ============
      (use-package dashboard
          :ensure nil
          :after all-the-icons
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
      ;; ============


      ;; =====================
      ;; Highlight TODO keywords
      ;; =====================
      (use-package hl-todo
          :ensure nil
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
      ;; =====================


      ;;=======================
      ;; color enhancement
      ;;=======================
      (use-package rainbow-mode
          :ensure nil
          :hook
          ((org-mode prog-mode) . rainbow-mode))

      (use-package rainbow-delimiters
          :ensure nil
          :after prog-mode
          :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
          (clojure-mode . rainbow-delimiters-mode)))
      ;;=======================

      ;; =====================
      ;; org-bullets
      ;; =====================
      (use-package org-bullets
       :ensure nil
       :after org
       :hook (org-mode . org-bullets-mode))
      ;; =====================

      ;; =====================
      ;; Which-key
      ;; =====================
      (use-package which-key
          :ensure nil
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
      ;; =====================
  '';
};
}
