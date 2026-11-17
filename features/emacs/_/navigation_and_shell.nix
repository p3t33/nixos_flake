{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      vterm
      vterm-toggle
      eshell-syntax-highlighting
    ];

     extraConfig = lib.mkOrder 500 ''
      ;;=======================
      ;; shell settings
      ;;=======================
      (use-package eshell-syntax-highlighting
          :ensure nil
          :after esh-mode
          :config
          (eshell-syntax-highlighting-global-mode +1))

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
      ;;=======================


      ;;=======================
      ;; vterm settings
      ;;=======================
      (use-package vterm
          :ensure nil
          :init
          (setq shell-file-name "zsh"
           vterm-max-scrollback 5000))

      (use-package vterm-toggle
          :ensure nil
          :after vterm
          :init
          (setq vterm-toggle-fullscreen-p nil)
          (setq vterm-toggle-scope 'project)
          :config
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
     '';
    };
};
}
