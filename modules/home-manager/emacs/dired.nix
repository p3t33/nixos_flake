{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs.extraPackages = epkgs: with epkgs; [
    dired-open
    peep-dired
  ];

  programs.emacs.extraConfig = ''
      ;; dired is part of emacs so no need to install it.

      (with-eval-after-load 'general
       (general-def :states 'normal
       :keymaps 'override
       :prefix "SPC"
        "d" '(:ignore t :which-key "Dired")
        "d d" '(dired :which-key "Open dired")
        "d j" '(dired-jump :which-key "Jump to current dir")
        "d n" '(neotree-dir :which-key "Neotree directory")
        "d p" '(peep-dired :which-key "Peep-dired preview")))

      ;; =====================
      ;; dired
      ;; =====================
      ;; used to open files that are not text using the desired software
      ;; this is mostly an example a I did not installed sxiv or mpv.
      (use-package dired-open
          :ensure nil
          :after dired
          :config
          (setq dired-open-extensions '(("gif" . "sxiv")
                                        ("jpg" . "sxiv")
                                        ("png" . "sxiv")
                                        ("mkv" . "mpv")
                                        ("mp4" . "mpv"))))

      ;; Used to provide a preview of a file.
      ;; looks like this plugin is archived.
      (use-package peep-dired
      :ensure nil
      :after (dired evil)
      :hook (evil-normalize-keymaps . peep-dired-hook)
      :config
          (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
          (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
          (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
          (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

  '';

};
}

