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
      corfu
      cape
      flycheck
    ];

 extraConfig = lib.mkOrder 600 ''
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

      ;; Corfu provides a modern, fast, and minimal in-buffer completion popup.
      ;; It perfectly replaces both company and company-box without the lag.
      (use-package corfu
       :ensure nil
       :custom
       (corfu-auto t)                 ;; Enable auto completion
       (corfu-auto-delay 0.1)         ;; Delay before popup appears
       (corfu-auto-prefix 2)          ;; Minimum length of prefix before completion kicks in
       (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous`
       (corfu-preselect 'prompt)      ;; Preselect the prompt, don't automatically select the first candidate
       :init
       (global-corfu-mode)
       :config
       (set-face-attribute 'corfu-current nil :background "#2257a0" :weight 'bold))

      ;; Cape (Completion At Point Extensions) provides the actual backend data 
      ;; (like words from open buffers) to the Corfu frontend.
      (use-package cape
       :ensure nil
       :init
       ;; Add dabbrev (dynamic abbreviation) to the global completion functions.
       ;; This brings back the "word suggestions" from open buffers that company provided.
       (add-to-list 'completion-at-point-functions #'cape-dabbrev)
       (add-to-list 'completion-at-point-functions #'cape-file))

      ;; ====================

 '';

 };
 };
}
