{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      lsp-mode
      lua-mode
      company
      company-box
      flycheck
    ];

 extraConfig = lib.mkOrder 500 ''
      ;; =====================
      ;; Language Support and Completion (LSP + Company)
      ;; =====================

      ;;
      ;; Syntax static error checking
      ;; -------------------
      (use-package flycheck
          :ensure nil
          :defer t
          :diminish
          :init (global-flycheck-mode))

      ;; syntax highlighting
      ;; -------------------
      ;; Not all languages are supported by default such as lua.
      (use-package lua-mode
          :ensure nil)

      ;; Language Servers (LSP)
      ;; ----------------------
      ;; this is the client side support for something like clgand
      ;; server.
      (use-package lsp-mode
          :ensure nil
          :hook ((c-mode c++-mode) . lsp)
          :commands lsp)

      ;; company can be used to provide the frontend for code
      ;; completions that the Language Server (like clangd for C++)
      ;; sends to the client (in this case, lsp-mode in Emacs).
      ;; in in essence this is the popup box with completion suggestions.
      (use-package company
       :ensure nil
       :defer 2
       :diminish
       :init
       (setq company-begin-commands '(self-insert-command)
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t)
       :config
       (global-company-mode t)
       ;; Define helper function for pcomplete integration here
       (defun add-pcomplete-to-capf ()
        "Add pcomplete to completion-at-point-functions for company-capf."
        (add-hook 'completion-at-point-functions #'pcomplete-completions-at-point nil t))

       ;; Hook 1: Integrate pcomplete via company-capf when in Org mode
       (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

       ;; Hook 2: Set specific, simpler backends when in Org mode
       (add-hook 'org-mode-hook (lambda ()
                                 (setq-local company-backends '(company-capf company-dabbrev))))
       )


      ;; company-box is an extension for company that provides a visually
      ;; enhanced dropdown for completions. It makes the completion menu
      ;; more aesthetically pleasing and can display additional information,
      ;; such as icons for different types of completion
      ;; candidates (e.g., a function, variable, or class).
      (use-package company-box
       :ensure nil
       :after company
       :diminish
       :hook (company-mode . company-box-mode))

      ;; Make company-mode work with org-mode's org-self-insert-command
      (with-eval-after-load 'company
       (add-to-list 'company-begin-commands 'org-self-insert-command))

      ;; ====================

 '';

 };
 };
}
