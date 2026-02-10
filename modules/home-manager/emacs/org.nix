{ config, lib, pkgs, ... }:

let
  cfg = config.programs.emacs;
  orgRoamDirctoryPath = "${config.home.homeDirectory}/Sync/dev_resources/roam_notes";
in
{
  config = lib.mkIf cfg.enable {
  home.packages = with pkgs; [
    # used by org-bable with PlantUML
    # -------
    plantuml
    graphviz
    jdk11
    # -------

    # support for LaTex and To export to pdf
    texlive.combined.scheme-full
  ];

  programs.emacs.extraPackages = epkgs: with epkgs; [
    org-roam
    org-download
    org-tree-slide
    ox-jira
    plantuml-mode
    websocket
    org-roam-ui
    org-transclusion
    ox-hugo
    toc-org
  ];

  programs.emacs.extraConfig = ''

      ;; ============
      ;; org
      ;; ============
      (use-package org
       :ensure nil ;; Org is built-in, no need to ensure installation
       :hook (org-mode . org-indent-mode)
       :bind (("C-c c" . org-capture))
       :init
       (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
                "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
       ;; Control default display width of inline images
       (setq org-image-actual-width nil)

       (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE")
            (sequence "QUESTION" "|" "ANSWERED")))

       (setq org-todo-keyword-faces
        '(("QUESTION" . (:foreground "red" :weight bold))
            ("ANSWERED" . (:foreground "green" :weight bold))))

      (setq org-return-follows-link  t)
      (setq org-startup-with-inline-images t)
      (setq org-hide-emphasis-markers t)
      (setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.

      :config

      ;; --- Hooks ---
      ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
      ;; Otherwise, org-tempo is broken when you try to <s TAB...
      (add-hook 'org-mode-hook (lambda ()
                                (setq-local electric-pair-inhibit-predicate
                                 `(lambda (c)
                                     (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

      ;; --- Load Sub-Features & Templates ---
      ;; provides support for "easy org mode templates".
      ;; this is what makes <s (among other things) to work.
      (require 'org-tempo)
      ;; Add custom <e> template expansion for Org structure templates
      (add-to-list 'org-structure-template-alist '("e" . "example"))

      )
      ;; ============

      ;; =================================
      ;; plantuml setitngs
      ;; =================================
      (use-package plantuml-mode
       :ensure nil
       :mode ("\\.plantuml\\'" . plantuml-mode)
       :config
       ;; Use executable for .plantuml file editing
       (setq plantuml-executable-path "${lib.getExe pkgs.plantuml}")
       (setq plantuml-default-exec-mode 'executable)

       ;; Only add the preview-on-save hook in plantuml-mode buffers
       (defun plantuml-preview-on-save ()
        (when (eq major-mode 'plantuml-mode)
         (plantuml-preview-buffer 4)))

       (add-hook 'plantuml-mode-hook
        (lambda ()
         (add-hook 'after-save-hook #'plantuml-preview-on-save nil t)))

       ;; Org-mode integration (must use jar method)
       (with-eval-after-load 'org
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((plantuml . t)))
        (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
        (setq org-plantuml-exec-mode 'jar)))
      ;; =================================

      ;; ===================
      ;; org-tree-slide slide show.
      ;; ===================
      (use-package org-tree-slide
       :ensure nil
       :after org ;; Ensure org is loaded first
       :commands (org-tree-slide-mode org-tree-slide-move-previous-tree org-tree-slide-move-next-tree)
       :config
         (define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-mode)
         (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
         (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
         (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
         (global-set-key (kbd "<f9>") 'org-tree-slide-move-previous-tree)
         (global-set-key (kbd "<f10>") 'org-tree-slide-move-next-tree)
         ;; Optional: Start with a simple profile
         ;; Uncomment the following line if you want to use the simple profile by default
         ;; (org-tree-slide-simple-profile)
      )

      ;; ===================


      ;; ============
      ;; org exporters
      ;; ============
      (use-package ox-hugo
       :ensure nil
       :after ox)

      ;; no need to install already part of org.
      (use-package ox-md
      :ensure nil
      :after ox)

      (use-package ox-jira
       :ensure nil
       :after ox)
      ;; ============


      ;; ============
      ;; toc-org: table of Contents
      ;; ============
      (use-package toc-org
          :ensure nil
          :after org
          :commands toc-org-enable
          :init
          (add-hook 'org-mode-hook 'toc-org-enable)
          :config
          (setq toc-org-max-depth 6)) ;; Set the default TOC depth
      ;; ============

      ;; org-transclusion
      ;; =================================
      (use-package org-transclusion
       :ensure nil
       :config
       :after org
       :hook (org-mode . org-transclusion-mode))
      ;; =================================


      ;; ============
      ;; org-download
      ;; ============
      (use-package org-download
       :ensure nil
       :after org
       :config
       ;; Set the method for handling downloaded images
       ;; 'attach integrates with Org's attachment system
       (setq org-download-method 'directory
        org-download-image-org-width 600)

       ;; where to save files?
       ;; when  org-download-method is set to "attach"
       ;; then org-roam will handle where to store the
       ;; file and creat place for it.
       ;; if you want to specify the directory then
       ;; org-download-method needs to be chagned to directory
       ;; and you will need to set org-download-image-dir, E.g:
       ;; Set the default directory where images will be downloaded
       ;; ONLY WORKS WITH org-download-method set to directory

       ;; Dynamically set org-download-image-dir to match the Org file timestamp
       ;; This is done so when org-roam node is deleted the referenced images in it delete
       ;; as well.
       (defun my/org-download-set-image-dir ()
        "Set `org-download-image-dir` based on the timestamp in the Org filename, but do NOT create the directory."
        (when buffer-file-name
         (let* ((org-base-name (file-name-base buffer-file-name))
                ;; Extract timestamp (part before first dash)
                (timestamp (car (split-string org-base-name "-")))
                (target-dir (expand-file-name
                             (concat "images/" timestamp)
                             (file-truename "${orgRoamDirctoryPath}"))))
          (setq-local org-download-image-dir target-dir))))

       ;; Hook it to org-mode
       (add-hook 'org-mode-hook #'my/org-download-set-image-dir))
      ;; ============


      ;;(with-eval-after-load 'org
      ;;    (setq org-agenda-files '("${orgRoamDirctoryPath}")))


      ;; =====================
      ;; org-roam and its ui
      ;; =====================
      (use-package org-roam
       :ensure nil
       :after org
       :custom
       (org-roam-directory (file-truename "${orgRoamDirctoryPath}"))
       :bind (("C-c n l" . org-roam-buffer-toggle)
              ("C-c n f" . org-roam-node-find)
              ("C-c n g" . org-roam-graph)
              ("C-c n i" . org-roam-node-insert)
              ("C-c n c" . org-roam-capture)
              ;; Dailies
              ("C-c n j" . org-roam-dailies-capture-today))
       :config

       ;; Define the costum function to delete nodes and sync database.
       (defun custom-org-roam-delete-node-and-sync ()
        "Delete the current Org-roam node file with confirmation,
        delete the associated image folder (based on timestamp), then sync the Org-roam database."
        (interactive)
        (when (and (buffer-file-name)
               (y-or-n-p "Are you sure you want to delete this node and its associated image directory?"))
         (let* ((file-to-delete (buffer-file-name))
                (file-base (file-name-base file-to-delete))
                ;; Extract the timestamp (part before the first dash)
                (timestamp (car (split-string file-base "-")))
                (image-dir (expand-file-name
                            (concat "images/" timestamp)
                            (file-truename "${orgRoamDirctoryPath}"))))
          ;; Delete the org file
          (delete-file file-to-delete)
          (message "Deleted file %s" file-to-delete)
          ;; Delete the image folder, if it exists
          (when (file-directory-p image-dir)
           (delete-directory image-dir t)
           (message "Deleted image directory %s" image-dir))
          ;; Kill buffer and sync
          (kill-buffer)
          (org-roam-db-sync))))

       ;; Define the costum function to rename existing node and sync database.
       ;; This function will preserve timestamp, update the name of the file and the name of the title in it.
       (defun custom-org-roam-rename-node ()
        "Rename the current org-roam file and update the #+title."
        (interactive)
        (let* ((old-file (buffer-file-name))
               (old-title (org-roam-node-title (org-roam-node-at-point)))
               (file-dir (file-name-directory old-file))
               (file-base (file-name-base old-file))
               (file-ext (file-name-extension old-file))
               ;; Extract the timestamp or number at the beginning
               (timestamp (car (split-string file-base "-")))
               ;; Get the current title part
               (current-title (mapconcat 'identity (cdr (split-string file-base "-")) " "))
               ;; Prompt for new title
               (new-title (read-string "New title: " current-title))
               ;; Construct the new file name
               (new-file (concat file-dir timestamp "-" new-title "." file-ext)))
         (when (and new-title (not (string= new-title "")))
          ;; Rename the file
          (rename-file old-file new-file)
          ;; Update buffer name
          (set-visited-file-name new-file)
          ;; Update #+title
          (goto-char (point-min))
          (when (re-search-forward "^#\\+title:.*$" nil t)
           (replace-match (concat "#+title: " new-title)))
          (save-buffer)
          ;; Update org-roam cache
          (org-roam-db-sync)
          (message "Renamed to %s and updated #+title." new-title))))

       ;; puting the remplate setitngs under :costum did not work for me.
       (setq org-roam-capture-templates
        '(
            ("d" "default" plain "* Table of Contents :toc:noexport:\n\n%?"
             :target (file+head "%<%d%m%Y%H%M%S>-''${slug}.org"
                 "#+date: [%<%d-%m-%Y %a %H:%M>]\n#+category: ''${title}\n#+filetags:\n#+title: ''${title}\n") :unnarrowed t)
            ("s" "story" plain "* Table of Contents :toc:noexport:\n\n* Resources\n- [%?[][jira]]\n\n* Overview\n\n* AC\n\n* Testing\n\n"
             :target (file+head "class/%<%d%m%Y%H%M%S>-''${slug}.org"
                 "#+date: [%<%d-%m-%Y %a %H:%M>]\n#+category: ''${title}\n#+filetags:\n#+title: ''${title}\n") :unnarrowed t)
            )
            )
       ;; Daily notes (journals) templates
       (setq org-roam-dailies-capture-templates
        '(("j" "journal" entry "* %<%I:%M %p> - %?"
                :target (file+head "%<%d-%m-%Y>.org"
                    "#+title: %<%d-%m-%Y>\n"))))

       ;; If you're using a vertical completion framework, you might want a more informative completion interface
       (setq org-roam-node-display-template (concat "''${title:*} " (propertize "''${tags:10}" 'face 'org-tag)))
       (org-roam-db-autosync-mode)
       ;; If using org-roam-protocol
       (require 'org-roam-protocol))


       ;; for now unly used by org-roam-ui so I did not put it inot core.nix
       (use-package websocket
           :ensure nil
           :after org-roam)

       (use-package org-roam-ui
        :ensure nil
        :after org-roam ;; or :after org
        ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
        ;;         a hookable mode anymore, you're advised to pick something yourself
        ;;         if you don't care about startup time, use
        ;;  :hook (after-init . org-roam-ui-mode)
        :config
        (setq org-roam-ui-sync-theme t
         org-roam-ui-follow t
         org-roam-ui-update-on-save t
         org-roam-ui-open-on-start t))
      ;; =====================

  '';
};
}
