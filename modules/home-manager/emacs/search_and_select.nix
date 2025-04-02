{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ripgrep
    fd
    fzf
  ];

  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      ivy
      counsel
      ivy-rich
      ivy-prescient
      all-the-icons-ivy-rich
      prescient
    ];


    extraConfig = ''
            ;; ivy settings
      ;; ============

      (use-package prescient
       :ensure nil
       :config
       (prescient-persist-mode 1))  ; Remember usage statistics across Emacs sessions

      (use-package ivy-prescient
       :ensure nil
       :after ivy
       :init
       (ivy-prescient-mode 1)  ; Enable ivy-prescient
       :config
       (setq ivy-prescient-retain-classic-highlighting t)  ; Optional: retain Ivy's highlighting style
       ;; Set regex builder for counsel-rg to ivy--regex-plus
       ;; without this I was getting error code: 2 when using counsel-rg with ivy-prescient
       (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))

      (use-package ivy
          :ensure nil
          :bind
          ;; ivy-resume resumes the last Ivy-based completion.
          (("C-c C-r" . ivy-resume)
           ("C-x B" . ivy-switch-buffer-other-window))
          :init
          (setq ivy-use-virtual-buffers t)
          (setq ivy-count-format "(%d/%d) ")
          (setq enable-recursive-minibuffers t)
          (setq ivy-use-selectable-prompt t)
          :config
          (ivy-mode))

      (use-package counsel
       :ensure nil
       :after ivy
       :bind (("M-x" . counsel-M-x))
       :init
       ;; These were already in ivy
       (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

      (use-package all-the-icons-ivy-rich
          :ensure nil
          :init (all-the-icons-ivy-rich-mode 1))

      (use-package ivy-rich
       :after ivy
       :ensure nil
       :init
       (ivy-rich-mode 1)  ;; Enable ivy-rich-mode
       :config
       ;; Recommended setting
       (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
       ;; Customize path style
       (setq ivy-rich-path-style 'abbrev))

      ;; ====================

    '';
    };
}

