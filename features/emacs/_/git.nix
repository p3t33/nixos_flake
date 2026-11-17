{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      magit
      git-timemachine
    ];

    extraConfig = lib.mkOrder 500 ''
      ;; ===================
      ;; git stack
      ;; ============
      (use-package magit
       :ensure nil)

      (use-package git-timemachine
          :ensure nil
          :after evil
          :config
              (evil-define-minor-mode-key 'normal 'git-timemachine-mode
                (kbd "C-j") #'git-timemachine-show-previous-revision
                (kbd "C-k") #'git-timemachine-show-next-revision)
      )
      ;; ============

    '';
  };
};
}

