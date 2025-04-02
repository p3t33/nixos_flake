{
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      magit
      git-timemachine
    ];

    extraConfig = ''
      ;; ===================
      ;; git stack
      ;; ============
      (use-package magit
       :ensure nil)

      (use-package git-timemachine ;; needs to be fixed
          :ensure nil
          :after evil
          :hook (evil-normalize-keymaps . git-timemachine-hook)
          :config
              (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
              (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
      )
      ;; ============

    '';
  };
}

