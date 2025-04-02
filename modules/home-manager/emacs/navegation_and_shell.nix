{
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      vterm
      vterm-toggle
      eshell-syntax-highlighting
      neotree
    ];

     extraConfig = ''
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

      ;;=======================
      ;; neotree
      ;; =====================
      (use-package neotree
          :ensure nil
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
      ;;=======================
     '';
    };

}
