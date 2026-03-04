{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs.extraPackages = epkgs: with epkgs; [
    dired-open
    dired-preview
  ];

  programs.emacs.extraConfig = lib.mkOrder 500 ''
      ;; dired is part of emacs so no need to install it.

      (with-eval-after-load 'general
       (general-def :states 'normal
       :keymaps 'override
       :prefix "SPC"
        "d" '(:ignore t :which-key "Dired")
        "d d" '(dired :which-key "Open dired")
        "d j" '(dired-jump :which-key "Jump to current dir")
        "d p" '(dired-preview-mode :which-key "Dired preview")))

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
      (use-package dired-preview
       :ensure nil
       :after (dired evil)
       :config
       (setq dired-preview-delay 0.1)
       (setq dired-preview-max-size (expt 2 20))
       (setq dired-preview-ignored-extensions-regexp
             (concat "\\."
                     "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|wav\\|m4a\\|flac\\|"
                     "gz\\|z\\|tar\\|tgz\\|rar\\|7z\\|zip\\|jar\\|xz\\|"
                     "iso\\|epub\\|pdf\\|"
                     "jpg\\|jpeg\\|png\\|gif\\|svg\\|bmp\\|tiff\\|webp\\|heic"
                     "\\)$"))
       (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
       (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file))

  '';

};
}

