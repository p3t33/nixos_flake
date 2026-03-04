{ lib, config, pkgs, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  home.packages = with pkgs; [
    ripgrep
    fd
    fzf
  ];

  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      vertico
      consult
      orderless
      marginalia
      nerd-icons-completion
    ];


    extraConfig = lib.mkOrder 500 ''
      ;; ====================
      ;; Vertico / Consult Stack
      ;; ====================

      ;; Enable vertical completion UI
      (use-package vertico
       :ensure nil
       :custom
       (vertico-count 12)       ;; Show more candidates
       (vertico-resize t)       ;; Grow and shrink the Vertico minibuffer
       (vertico-cycle t)        ;; Enable cycling for `vertico-next/previous`
       :init
       (vertico-mode)
       :config
       ;; Allow pasting from OS clipboard directly into Vertico minibuffer
       (define-key vertico-map (kbd "C-S-v") (lambda () (interactive) (insert (gui-get-selection 'CLIPBOARD 'STRING)))))

      ;; Persist history over Emacs restarts. Vertico sorts by history position.
      (use-package savehist
       :ensure nil ; Built-in
       :init
       (savehist-mode))

      ;; Optionally use the `orderless` completion style for space-separated fuzzy matching
      (use-package orderless
       :ensure nil
       :custom
       ;; Configure a custom style dispatcher (see the Consult wiki)
       ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
       ;; (orderless-component-separator #'orderless-escapable-split-on-space)
       (completion-styles '(orderless basic))
       (completion-category-defaults nil)
       (completion-category-overrides '((file (styles partial-completion)))))

      ;; Enable rich annotations (file paths, docstrings) using the minibuffer
      (use-package marginalia
       :ensure nil
       ;; Bind `marginalia-cycle` locally in the minibuffer.  To make the binding
       ;; available in the *Completions* buffer, add it to the
       ;; `completion-list-mode-map`.
       :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
       :init
       (marginalia-mode))

      ;; Add icons to the Vertico completion menus
      (use-package nerd-icons-completion
       :ensure nil
       :after marginalia
       :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
       :init
       (nerd-icons-completion-mode))

      ;; Consult provides the advanced search commands (ripgrep, buffers, etc)
      (use-package consult
       :ensure nil
       ;; Replace bindings. Some of these are remapped in keybindings.nix, but
       ;; we set standard Emacs bindings here as a fallback.
       :bind (;; C-c bindings in `mode-specific-map'
              ("C-c M-x" . consult-mode-command)
              ("C-c h" . consult-history)
              ("C-c k" . consult-kmacro)
              ("C-c m" . consult-man)
              ("C-c i" . consult-info)
              ([remap Info-search] . consult-info)
              ;; C-x bindings in `ctl-x-map'
              ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
              ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
              ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
              ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
              ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
              ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
              ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
              ;; Custom M-# bindings for fast register access
              ("M-#" . consult-register-load)
              ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
              ("C-M-#" . consult-register)
              ;; Other custom bindings
              ("M-y" . consult-yank-pop)                ;; orig. yank-pop
              ;; M-g bindings in `goto-map'
              ("M-g e" . consult-compile-error)
              ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
              ("M-g g" . consult-goto-line)             ;; orig. goto-line
              ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
              ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
              ("M-g m" . consult-mark)
              ("M-g k" . consult-global-mark)
              ("M-g i" . consult-imenu)
              ("M-g I" . consult-imenu-multi)
              ;; M-s bindings in `search-map'
              ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
              ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
              ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
              ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
              ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
              ;; Minibuffer history
              :map minibuffer-local-map
              ("M-s" . consult-history)                 ;; orig. next-matching-history-element
              ("M-r" . consult-history))                ;; orig. previous-matching-history-element
       :hook (completion-list-mode . consult-preview-at-point-mode)
       :init
       ;; Optionally configure the register formatting. This improves the register
       ;; preview for `consult-register', `consult-register-load',
       ;; `consult-register-store' and the Emacs built-ins.
       (setq register-preview-delay 0.5
             register-preview-function #'consult-register-format)
       ;; This adds thin lines, sorting and hides the mode line of the window.
       (advice-add #'register-preview :override #'consult-register-window)

       ;; Use Consult to select xref locations with preview
       (setq xref-show-xrefs-function #'consult-xref
             xref-show-definitions-function #'consult-xref)
       :config
       ;; Use `consult-fd` for faster finding
       (setq consult-find-command "fd --color=never --full-path ARG OPTS")
       (setq consult-narrow-key "<"))

      ;; ====================

    '';
    };
};
}

