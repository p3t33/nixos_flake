{ config, lib, pkgs, ... }:

let
  cfg = config.programs.emacs;
  orgRoamDirectoryPath = "${config.home.homeDirectory}/Sync/dev_resources/roam_notes";
  documentsDir = config.xdg.userDirs.documents;
in
{
  config = lib.mkIf cfg.enable {
  home.packages = with pkgs; [
    # used by org-bable with PlantUML
    # -------
    plantuml
    graphviz
    # Review this explicit version choice on every NixOS stable upgrade.
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
    org-appear
  ];

  programs.emacs.extraConfig = lib.mkOrder 500 ''

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
      (setq org-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.

      ;; Export settings
      (setq org-export-in-background nil)
      (advice-remove 'org-export-output-file-name #'my/org-export-output-file-name)
      (defvar my/org-portable-export-routing nil)
      (defun my/org-portable-export-command (orig-fun &rest args)
        (let ((my/org-portable-export-routing t))
          (apply orig-fun args)))
      (defun my/org-portable-export-validate-bundle (directory)
        (dolist (entry (directory-files directory t directory-files-no-dot-files-regexp))
          (let ((attributes (file-attributes entry 'integer)))
            (when (file-symlink-p entry)
              (user-error "Unsafe symlink in export bundle: %s" entry))
            (if (file-directory-p entry)
                (my/org-portable-export-validate-bundle entry)
              (when (and attributes (null (file-attribute-type attributes))
                         (> (file-attribute-link-number attributes) 1))
                (user-error "Unsafe multiply-linked file in export bundle: %s" entry))))))
      (defun my/org-portable-export-file (orig-fun backend file &optional async subtreep visible-only body-only ext-plist post-process)
        (if (not (and my/org-portable-export-routing
                      (memq backend '(md latex))))
            (funcall orig-fun backend file async subtreep visible-only body-only ext-plist post-process)
          (let* ((source (buffer-file-name (buffer-base-buffer)))
                 (source (and source (expand-file-name source))))
            (unless (and source (not (file-remote-p source)) (file-readable-p source))
              (user-error "Portable Org exports require a readable local source file"))
            (let* ((identity (file-truename source))
                   (short-id (substring (secure-hash 'sha256 identity) 0 12))
                   (source-name (file-name-base source))
                   (source-name (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" source-name))
                   (source-name (substring source-name 0 (min 60 (length source-name))))
                   (root (expand-file-name "org_exports" "${documentsDir}"))
                   (bundle (expand-file-name (format "%s-%s" source-name short-id) root))
                   (output-name (file-name-nondirectory file))
                   (output (expand-file-name output-name bundle))
                   (ext-plist (org-combine-plists ext-plist
                                                  (list :my-org-export-bundle bundle
                                                        :my-org-export-source source))))
              (when (file-symlink-p bundle)
                (user-error "Export bundle directory is a symlink: %s" bundle))
              (make-directory bundle t)
              (my/org-portable-export-validate-bundle bundle)
              (when (or (file-symlink-p output)
                        (and (file-exists-p output)
                             (or (file-equal-p output source)
                                 (> (file-attribute-link-number (file-attributes output)) 1))))
                (user-error "Unsafe export output path: %s" output))
              (funcall orig-fun backend output async subtreep visible-only body-only
                       ext-plist post-process)))))
      (defun my/org-portable-export-images (tree backend info)
        (let ((bundle (plist-get info :my-org-export-bundle))
              (source (plist-get info :my-org-export-source)))
          (when (and bundle source (memq backend '(md latex)))
            (let* ((assets (expand-file-name "assets" bundle))
                   (rules (plist-get info (if (eq backend 'md)
                                              :html-inline-image-rules
                                            :latex-inline-image-rules)))
                   (copied (make-hash-table :test 'equal)))
              (when (file-symlink-p assets)
                (user-error "Export assets directory is a symlink: %s" assets))
              (make-directory assets t)
              (org-element-map tree 'link
                (lambda (link)
                  (let* ((path (org-element-property :path link))
                         (rule (cdr (assoc "file" rules))))
                    (when (and (equal (org-element-property :type link) "file")
                               (stringp path) rule (string-match-p rule path))
                      (let* ((absolute (expand-file-name path (file-name-directory source)))
                             (resolved (and (not (file-remote-p absolute))
                                            (file-exists-p absolute)
                                            (file-truename absolute))))
                        (unless (and resolved (not (file-remote-p resolved))
                                     (file-readable-p resolved) (not (file-directory-p resolved)))
                          (user-error "Missing or unreadable exported image: %s" absolute))
                        (let* ((extension (or (file-name-extension path t) ""))
                               (name (concat (substring (secure-hash 'sha256 resolved) 0 16) extension))
                               (destination (expand-file-name name assets)))
                          (unless (gethash destination copied)
                            (when (or (file-symlink-p destination)
                                      (and (file-exists-p destination)
                                           (or (file-equal-p destination resolved)
                                               (> (file-attribute-link-number
                                                   (file-attributes destination)) 1))))
                              (user-error "Unsafe exported image path: %s" destination))
                            (let ((temporary (make-temp-file
                                              (expand-file-name ".org-export-" assets))))
                              (unwind-protect
                                  (progn
                                    (copy-file resolved temporary t)
                                    (rename-file temporary destination t))
                                (when (file-exists-p temporary) (delete-file temporary))))
                            (puthash destination t copied))
                          (org-element-put-property link :path (concat "assets/" name))
                          (org-element-put-property link :raw-link (concat "file:assets/" name))))))))))
          tree))
      (add-hook 'org-export-filter-parse-tree-functions #'my/org-portable-export-images)
      (advice-add 'org-export-to-file :around #'my/org-portable-export-file)
      (with-eval-after-load 'ox-md
        (advice-add 'org-md-export-to-markdown :around #'my/org-portable-export-command))
      (with-eval-after-load 'ox-latex
        (advice-add 'org-latex-export-to-latex :around #'my/org-portable-export-command)
        (advice-add 'org-latex-export-to-pdf :around #'my/org-portable-export-command))

      :config

      ;; --- Hooks ---
      ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
      ;; Otherwise, org-tempo is broken when you try to <s TAB...
      (add-hook 'org-mode-hook (lambda ()
                                (setq-local electric-pair-inhibit-predicate
                                 `(lambda (c)
                                     (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

      ;; scroll-margin 8 causes jumps near tall inline images (one buffer line
      ;; can be 30+ visual lines), so reduce it locally in org-mode.
      (add-hook 'org-mode-hook (lambda ()
        (setq-local scroll-margin 2)))

      ;; --- Load Sub-Features & Templates ---
      ;; provides support for "easy org mode templates".
      ;; this is what makes <s (among other things) to work.
      (require 'org-tempo)
      ;; Add custom <e> template expansion for Org structure templates
      (add-to-list 'org-structure-template-alist '("e" . "example"))

      )
      ;; ============

      ;; =================================
      ;; plantuml settings
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
         (add-hook 'after-save-hook #'plantuml-preview-on-save nil t))))

      ;; Enable Babel independently of opening a .plantuml file.
      (with-eval-after-load 'org
       (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar"
             org-plantuml-exec-mode 'jar)
       (org-babel-do-load-languages
        'org-babel-load-languages
        (cons '(plantuml . t) (assq-delete-all 'plantuml org-babel-load-languages))))
      ;; =================================

      ;; ===================
      ;; org-tree-slide slide show.
      ;; ===================
      (use-package org-tree-slide
       :ensure nil
       :after org ;; Ensure org is loaded first
       :bind (("<f8>" . org-tree-slide-mode)
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree))
       :config
         (define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-mode)
         (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
         (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
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

      ;; org-appear: reveal emphasis markers when cursor is on them
      ;; =================================
      (use-package org-appear
       :ensure nil
       :hook (org-mode . org-appear-mode)
       :custom
       (org-appear-autolinks t)
       (org-appear-autosubmarkers t)
       (org-appear-autoentities t))
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
        org-download-image-org-width 600
        org-download-annotate-function (lambda (_link) ""))

       (defun my/org-download-journal-date (file)
        "Return the ISO date of a DD-MM-YYYY journal FILE, or nil for other names."
        (let ((base (file-name-base file)))
          (when (string-match "\\`\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{4\\}\\)\\'" base)
            (let ((day (string-to-number (match-string 1 base)))
                  (month (string-to-number (match-string 2 base)))
                  (year (string-to-number (match-string 3 base))))
              (require 'calendar)
              (unless (calendar-date-is-valid-p (list month day year))
                (user-error "Invalid journal date in filename: %s" file))
              (format "%04d-%02d-%02d" year month day)))))

       (defun my/org-download-image-dir-for-file (file)
        "Return FILE's image directory without creating it or moving existing images."
        (let* ((date (my/org-download-journal-date file))
               (key (if date (concat "journal-" date)
                      (car (split-string (file-name-base file) "-")))))
          (expand-file-name (concat "images/" key)
                            (file-truename "${orgRoamDirectoryPath}"))))

       (defun my/org-download-set-image-dir (&rest _)
        "Refresh the current note's image directory without creating it."
        (when (and buffer-file-name (derived-mode-p 'org-mode))
          (setq-local org-download-image-dir
                      (my/org-download-image-dir-for-file buffer-file-name))))

       (add-hook 'org-mode-hook #'my/org-download-set-image-dir)
       ;; Refresh at import time as well, including existing buffers and base64 drops.
       (advice-add 'org-download--dir :before #'my/org-download-set-image-dir))
      ;; ============


      ;;(with-eval-after-load 'org
      ;;    (setq org-agenda-files '("${orgRoamDirectoryPath}")))


      ;; =====================
      ;; org-roam and its ui
      ;; =====================
      ;; No :after org — load eagerly so org-roam-db-autosync-mode runs at
      ;; startup and the database is ready before the first keybinding call.
      ;; Without this, the first org-roam command (e.g. SPC f n) stalls while
      ;; the database is loaded.
      (use-package org-roam
       :ensure nil
       :demand t
       :custom
       (org-roam-directory (file-truename "${orgRoamDirectoryPath}"))
       :bind (("C-c n l" . org-roam-buffer-toggle)
              ("C-c n f" . org-roam-node-find)
              ("C-c n g" . org-roam-graph)
              ("C-c n i" . org-roam-node-insert)
              ("C-c n c" . org-roam-capture)
              ;; Dailies
              ("C-c n j" . org-roam-dailies-capture-today))
       :config

       ;; Existing notes need a manual reindex to show transclusions as backlinks.
       (setq org-roam-db-extra-links-exclude-keys
             (mapcar (lambda (entry)
                       (if (eq (car entry) 'keyword)
                           (cons 'keyword
                                 (seq-remove (lambda (key) (string-equal (downcase key) "transclude"))
                                             (cdr entry)))
                         entry))
                     org-roam-db-extra-links-exclude-keys))

       (defun custom-org-roam--delete-read-file (file)
        "Read owned nodes and dependencies from saved FILE, without preview hooks."
        (require 'org-id)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((org-inhibit-startup t))
            (delay-mode-hooks (org-mode)))
          (let* ((tree (org-element-parse-buffer))
                 (title (or (org-element-map tree 'keyword
                              (lambda (keyword)
                                (when (and (equal (org-element-property :key keyword) "TITLE")
                                           (not (org-element-lineage keyword '(headline))))
                                  (org-element-property :value keyword))) nil t)
                            (file-name-base file)))
                 nodes refs)
            (org-element-map tree 'node-property
              (lambda (property)
                (when (equal (org-element-property :key property) "ID")
                  (let ((heading (org-element-lineage property '(headline))))
                    (push (cons (org-element-property :value property)
                                (if heading (org-element-property :raw-value heading) title)) nodes)))))
            (org-element-map tree 'link
              (lambda (link)
                (when (member (org-element-property :type link) '("id" "file" "file+emacs" "file+sys"))
                  (push (list :type (org-element-property :type link)
                              :path (org-element-property :path link)
                              :kind "link" :line (line-number-at-pos (org-element-property :begin link)))
                        refs))))
            (org-element-map tree 'keyword
              (lambda (keyword)
                (when (equal (org-element-property :key keyword) "TRANSCLUDE")
                  (let ((value (org-element-property :value keyword)) links)
                    (with-temp-buffer
                      (insert value)
                      (delay-mode-hooks (org-mode))
                      (setq links (org-element-map (org-element-parse-buffer) 'link #'identity)))
                    (unless (and (= (length links) 1)
                                 (member (org-element-property :type (car links)) '("id" "file")))
                      (user-error "Cannot check transclusion in %s: %s" file value))
                    (push (list :type (org-element-property :type (car links))
                                :path (org-element-property :path (car links))
                                :kind "transclusion"
                                :line (line-number-at-pos (org-element-property :begin keyword))) refs)))))
            (list :title title :nodes (delete-dups (nreverse nodes)) :refs (nreverse refs)))))

       (defun custom-org-roam--delete-note-files (root)
        "Return canonical eligible local note files, failing on unreadable sources."
        (let (files)
          (dolist (file (org-roam-list-files))
            (when (file-remote-p file)
              (user-error "Cannot check remote Org-roam source: %s" file))
            (let ((path (file-truename file)))
              (unless (and (file-in-directory-p path root) (file-regular-p path)
                           (file-readable-p path))
                (user-error "Cannot completely check Org-roam source: %s" file))
              (push path files)))
          (sort (delete-dups files) #'string<)))

       (defun custom-org-roam--delete-check-snapshot (root snapshots)
        "Abort if eligible note files or their saved/unsaved content have changed."
        (unless (equal (mapcar #'car snapshots) (custom-org-roam--delete-note-files root))
          (user-error "Org-roam file list changed; run deletion again"))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and buffer-file-name (not (file-remote-p buffer-file-name))
                       (assoc (file-truename buffer-file-name) snapshots))
              (when (buffer-modified-p)
                (user-error "Save or discard edits in %s before checking deletion" buffer-file-name))
              (when (fboundp 'org-transclusion-within-live-sync-p)
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (while (< (point) (point-max))
                      (when (org-transclusion-within-live-sync-p)
                        (user-error "Exit transclusion live-sync and save %s before checking deletion"
                                    buffer-file-name))
                      (goto-char (next-char-property-change (point) (point-max))))))))))
        (dolist (snapshot snapshots)
          (unless (equal (cdr snapshot) (custom-org-roam--file-byte-hash (car snapshot)))
            (user-error "Org-roam source changed; run deletion again: %s" (car snapshot)))))

       (defun custom-org-roam--delete-summary (target root)
        "Return TARGET's owned nodes, incoming references and saved-file snapshots."
        (let* ((files (custom-org-roam--delete-note-files root))
               (snapshots (mapcar (lambda (file) (cons file (custom-org-roam--file-byte-hash file))) files))
               (target-data (custom-org-roam--delete-read-file target))
               (nodes (plist-get target-data :nodes)) references)
          (unless (member target files)
            (user-error "Target is not an eligible Org-roam file"))
          (unless nodes (user-error "Current file owns no Org-roam ID nodes"))
          (custom-org-roam--delete-check-snapshot root snapshots)
          (dolist (file files)
            (unless (equal file target)
              (let ((data (custom-org-roam--delete-read-file file)))
                (dolist (ref (plist-get data :refs))
                  (let* ((type (plist-get ref :type))
                         (path (org-link-unescape (plist-get ref :path)))
                         (id (car (split-string path "::")))
                         (node (and (equal type "id") (assoc id nodes)))
                         (destination (unless (equal type "id")
                                        (expand-file-name path (file-name-directory file))))
                         (file-hit (and destination (not (file-remote-p destination))
                                        (equal (file-truename destination) target))))
                    (when (or node file-hit)
                      (push (format "%s (%s:%d) -> %s [%s]"
                                    (plist-get data :title) (file-relative-name file root)
                                    (plist-get ref :line)
                                    (if node (cdr node) (file-relative-name target root))
                                    (plist-get ref :kind)) references)))))))
          (custom-org-roam--delete-check-snapshot root snapshots)
          (list :nodes nodes :references (delete-dups (nreverse references)) :snapshots snapshots)))

       (defun custom-org-roam--delete-visiting-buffers (file)
        "Return buffers visiting FILE, including indirect buffers and path aliases."
        (let (buffers)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (let ((path (buffer-file-name (or (buffer-base-buffer) buffer))))
                (when (and path (not (file-remote-p path))
                           (equal (file-truename path) file))
                  (push buffer buffers)))))
          buffers))

       (defun custom-org-roam--delete-reopen-if-needed (file)
        "Try to reopen an unchanged-on-disk note after its buffer was closed."
        (when (and (file-exists-p file)
                   (not (custom-org-roam--delete-visiting-buffers file)))
          (condition-case failure
              (find-file-noselect file)
            ((error quit)
             (message "Note remains at %s, but reopening failed: %s"
                      file (error-message-string failure))))))

       (defun custom-org-roam--delete-to-trash (file root snapshots image-dir)
        "Close FILE's buffers before trashing it; report each partially completed stage."
        (let ((org-roam-directory root))
          (condition-case failure
              (progn
                (custom-org-roam--delete-check-snapshot root snapshots)
                (dolist (buffer (custom-org-roam--delete-visiting-buffers file))
                  (when (and (buffer-live-p buffer) (not (kill-buffer buffer)))
                    (user-error "Buffer closure refused; deletion cancelled")))
                (when (custom-org-roam--delete-visiting-buffers file)
                  (user-error "A buffer still visits the note; deletion cancelled"))
                (custom-org-roam--delete-check-snapshot root snapshots))
            ((error quit)
             (custom-org-roam--delete-reopen-if-needed file)
             (user-error "No Trash operation attempted: %s" (error-message-string failure))))
          (let (note-failure image-failure db-failure
                (image-status (if image-dir "Image cleanup not attempted"
                                "Journal images retained")))
            (condition-case failure
                (move-file-to-trash file)
              ((error quit) (setq note-failure failure)))
            (when (or (file-exists-p file) (file-symlink-p file))
              (custom-org-roam--delete-reopen-if-needed file)
              (user-error "Note remains at %s; images untouched; Trash failed: %s"
                          file (if note-failure (error-message-string note-failure)
                                 "original path still exists")))
            (unless note-failure
              (when image-dir
                (if (or (file-exists-p image-dir) (file-symlink-p image-dir))
                    (progn
                      (condition-case failure
                          (progn
                            (move-file-to-trash image-dir)
                            (when (or (file-exists-p image-dir) (file-symlink-p image-dir))
                              (error "Image directory still exists at its original path")))
                        ((error quit) (setq image-failure failure)))
                      (setq image-status
                            (if image-failure
                                (format "Image Trash operation failed (%s); original directory %s: %s"
                                        (error-message-string image-failure)
                                        (if (or (file-exists-p image-dir) (file-symlink-p image-dir))
                                            "remains" "is absent; check Trash")
                                        image-dir)
                              (format "Image directory moved to Trash: %s" image-dir))))
                  (setq image-status "No associated image directory existed"))))
            (condition-case failure
                (org-roam-db-sync)
              ((error quit) (setq db-failure failure)))
            (let ((status (concat
                           (if note-failure
                               (format "Note is absent after a failed Trash attempt (%s); check Trash: %s"
                                       (error-message-string note-failure) file)
                             (format "Note moved to Trash: %s" file))
                           "; " image-status
                           (if db-failure
                               (format "; Org-roam index refresh failed: %s" (error-message-string db-failure))
                             "; Org-roam index refreshed"))))
              (if (or note-failure image-failure db-failure)
                  (user-error "%s" status)
                (message "%s" status))))))

       (defun custom-org-roam-delete-node-and-sync ()
        "Move the current whole Roam file to Trash after checking owned nodes and references."
        (interactive)
        (let ((source (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
          (when (or (not source) (file-remote-p source) (file-remote-p org-roam-directory))
            (user-error "Current buffer is not a local Org-roam note file"))
          (let* ((file (file-truename source))
                 (root (file-name-as-directory (file-truename org-roam-directory))))
            (unless (and (derived-mode-p 'org-mode) (file-regular-p file)
                         (file-in-directory-p file root) (org-roam-file-p file)
                         (verify-visited-file-modtime (current-buffer)))
              (user-error "Current buffer is not an up-to-date local Org-roam note file"))
            (let* ((journal-date (my/org-download-journal-date file))
                   (image-dir (unless journal-date (my/org-download-image-dir-for-file file)))
                   (summary (custom-org-roam--delete-summary file root))
                   (nodes (plist-get summary :nodes))
                   (references (plist-get summary :references))
                   (prompt (concat
                            (format "Move WHOLE note file to Trash:\n%s\n\nAll %d owned ID node(s) will be removed:\n- %s\n\n"
                                    file (length nodes)
                                    (mapconcat (lambda (node) (format "%s [%s]" (cdr node) (car node))) nodes "\n- "))
                            (if references
                                (concat "References from surviving notes will stop resolving:\n- "
                                        (mapconcat #'identity references "\n- "))
                              "No incoming references found in eligible Org-roam sources.")
                            "\nOnly saved eligible files within this Org-roam root are checked.\n"
                            (if journal-date
                                "Journal images will be retained, including legacy shared image directories."
                              "The associated image directory will also be moved to Trash if present.")
                            "\n\nProceed? ")))
              (when (y-or-n-p prompt)
                (custom-org-roam--delete-to-trash file root (plist-get summary :snapshots) image-dir))))))

       (defun custom-org-roam--file-byte-hash (file)
        "Return a hash of FILE's bytes, without decoding its contents."
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (secure-hash 'sha256 (current-buffer))))

       ;; Define the custom function to rename existing node and sync database.
       (defun custom-org-roam-rename-node ()
        "Safely rename the current file-level Org-roam node."
        (interactive)
        (let* ((old-file (buffer-file-name))
               (root (file-name-as-directory (file-truename org-roam-directory)))
               (node (org-roam-node-at-point))
               (old-title (and node (org-roam-node-title node)))
               (base (and old-file (file-name-nondirectory old-file)))
               (timestamp (and base (and (string-match "\\`\\([0-9]\\{14\\}\\)-.+\\.org\\'" base)
                                         (match-string 1 base))))
               (new-title nil)
               (new-file nil))
         (unless (and old-file (file-regular-p old-file)
                      (string-prefix-p root (file-truename old-file))
                      (file-in-directory-p old-file root)
                      (string-match-p "\\.org\\'" old-file)
                      node (= (or (org-roam-node-level node) 0) 0)
                      (equal (file-truename (org-roam-node-file node))
                             (file-truename old-file))
                      timestamp)
           (user-error "Current buffer is not a supported file-level Org-roam note"))
         (when (buffer-narrowed-p)
           (user-error "Widen the buffer before renaming this node"))
         (when buffer-read-only
           (user-error "Current buffer is read-only"))
         (unless (and (file-writable-p old-file) (verify-visited-file-modtime (current-buffer)))
           (user-error "Source file is not writable or was changed on disk"))
         (let ((initial-heading (org-element-map (org-element-parse-buffer) 'headline
                                  (lambda (heading) (org-element-property :begin heading)) nil t)))
           (unless (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^#\\+title:[ \t]*.*$" (or initial-heading (point-max)) t))
             (user-error "Missing #+title in file header")))
         (setq new-title (read-string "New title: " old-title))
         (when (or (= (length new-title) 0) (string-match-p "[\n\r]" new-title))
           (user-error "Title must be non-empty and on one line"))
         (let* ((new-slug (replace-regexp-in-string
                           "_+$" ""
                           (replace-regexp-in-string
                            "^_+" ""
                            (replace-regexp-in-string "[^[:alnum:]]+" "_" (downcase new-title)))))
                (existing (org-roam-node-from-title-or-alias new-title)))
           (when (= (length new-slug) 0)
             (user-error "Title must contain at least one alphanumeric character"))
           (setq new-file (expand-file-name (concat timestamp "-" new-slug ".org")
                                            (file-name-directory old-file)))
           (when (and (not (equal old-file new-file))
                      (or (file-exists-p new-file) (file-symlink-p new-file)
                          (get-file-buffer new-file)))
             (user-error "Destination already exists or is visited: %s" new-file))
           (when (and existing
                      (not (equal (org-roam-node-file existing) old-file))
                      (not (y-or-n-p (format "A node titled '%s' already exists. Rename anyway? " new-title))))
             (user-error "Rename cancelled")))
         (when (and (buffer-modified-p)
                    (not (y-or-n-p "Save existing edits before renaming? ")))
           (user-error "Rename cancelled"))
         (when (buffer-modified-p)
           (save-buffer))
         (when (buffer-modified-p)
           (unless (y-or-n-p "Save changes made by save hooks before renaming? ")
             (user-error "Rename cancelled; save-hook edits remain unsaved"))
           (save-buffer)
           (when (buffer-modified-p)
             (user-error "Save hooks modified the buffer again; rename cancelled")))
         (unless (and (file-writable-p old-file) (verify-visited-file-modtime (current-buffer)))
           (user-error "Source file is not writable or was changed on disk"))
         (let* ((first-heading (org-element-map (org-element-parse-buffer) 'headline
                                 (lambda (heading) (org-element-property :begin heading)) nil t))
                (header-end (copy-marker (or first-heading (point-max))))
                title-start category-start)
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward "^#\\+title:[ \t]*.*$" header-end t)
               (setq title-start (copy-marker (match-beginning 0))))
             (goto-char (point-min))
             (when (re-search-forward "^#\\+category:[ \t]*.*$" header-end t)
               (setq category-start (copy-marker (match-beginning 0))))
           (unless title-start (user-error "Missing #+title in file header")))
           (when (and (not (equal old-file new-file))
                      (or (file-exists-p new-file) (file-symlink-p new-file)
                          (get-file-buffer new-file)))
             (user-error "Destination already exists or is visited: %s" new-file))
           (let* ((old-text (buffer-substring (point-min) (point-max)))
                  (old-coding buffer-file-coding-system)
                  (backup (make-temp-file "org-roam-rename-backup-"))
                  (staged nil)
                  (committed nil)
                  (recovered nil)
                  (mutation-started nil))
             (unwind-protect
                 (condition-case failure
                     (progn
                       (copy-file old-file backup t)
                       (atomic-change-group
                         (goto-char title-start)
                         (unless (re-search-forward "^#\\+title:[ \t]*.*$" header-end t)
                           (error "Title header changed before replacement"))
                         (replace-match (concat "#+title: " new-title) t t)
                         (when category-start
                           (goto-char category-start)
                           (unless (re-search-forward "^#\\+category:[ \t]*.*$" header-end t)
                             (error "Category header changed before replacement"))
                           (replace-match (concat "#+category: " new-title) t t)))
                       (setq staged (make-temp-file "org-roam-rename-staged-"))
                       (let ((coding-system-for-write buffer-file-coding-system))
                         (write-region (point-min) (point-max) staged nil 'silent))
                       (setq mutation-started t)
                       (unless (equal old-file new-file)
                         (rename-visited-file new-file))
                       (save-buffer)
                       (setq committed t))
                   ((error quit)
                    (let ()
                      (condition-case recovery
                          (progn
                            (when (and (not (equal old-file new-file))
                                       (not (file-exists-p old-file))
                                       (file-exists-p new-file))
                              (unless (or (equal (custom-org-roam--file-byte-hash new-file)
                                                 (custom-org-roam--file-byte-hash backup))
                                          (and staged
                                               (equal (custom-org-roam--file-byte-hash new-file)
                                                      (custom-org-roam--file-byte-hash staged))))
                                (error "Destination changed concurrently; refusing rollback"))
                              (when (equal buffer-file-name old-file)
                                (set-visited-file-name new-file))
                              (rename-file new-file old-file)
                              (set-visited-file-name old-file))
                            (unless (or (file-exists-p old-file) (file-symlink-p old-file))
                              (when (and (or (equal old-file new-file)
                                             (not (file-exists-p new-file)))
                                         (not (file-symlink-p old-file)))
                                (copy-file backup old-file nil)
                                (unless (equal (custom-org-roam--file-byte-hash old-file)
                                               (custom-org-roam--file-byte-hash backup))
                                  (error "Restored source does not match original backup"))
                                (unless (equal buffer-file-name old-file)
                                  (set-visited-file-name old-file))))
                            (when (or (file-exists-p old-file) (file-symlink-p old-file))
                              (unless (equal buffer-file-name old-file)
                                (error "Original path is occupied; refusing to overwrite it"))
                              (unless (equal (custom-org-roam--file-byte-hash old-file)
                                             (custom-org-roam--file-byte-hash backup))
                                (unless (and staged
                                             (equal (custom-org-roam--file-byte-hash old-file)
                                                    (custom-org-roam--file-byte-hash staged)))
                                  (error "Original path changed concurrently; refusing to overwrite it"))
                                (copy-file backup old-file t)))
                            (unless (and (file-regular-p old-file)
                                         (equal (custom-org-roam--file-byte-hash old-file)
                                                (custom-org-roam--file-byte-hash backup)))
                              (error "Original source could not be restored and verified"))
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert old-text))
                            (set-buffer-file-coding-system old-coding t)
                            (set-visited-file-modtime)
                            (set-buffer-modified-p nil)
                            (setq recovered t))
                        ((error quit)
                         (set-buffer-modified-p t)
                         (message "Rename recovery failed (%s); recoverable text remains in %s; original backup: %s"
                                  (error-message-string recovery)
                                  (or buffer-file-name "an unnamed buffer") backup)))
                      (unless recovered
                        (message "Rename recovery incomplete; current buffer location: %s; original backup: %s"
                                 (or buffer-file-name "unnamed buffer") backup))
                      (signal (car failure) (cdr failure)))))
               (when (and staged (file-exists-p staged)) (delete-file staged))
               (when (and (file-exists-p backup)
                          (or committed recovered (not mutation-started)))
                 (delete-file backup)))
             (when (and (not committed) (not recovered) mutation-started)
               (message "Preserved original recovery backup at %s" backup))
             (when committed
               (condition-case db-failure
                   (org-roam-db-sync)
                 ((error quit)
                  (user-error "Rename succeeded at %s, but Org-roam index refresh failed: %s"
                              buffer-file-name (error-message-string db-failure))))
               (message "Renamed to %s and updated #+title and #+category." new-title))))))

       ;; putting the template settings under :custom did not work for me.
       (setq org-roam-capture-templates
        '(
            ("d" "default" plain "* Table of Contents :toc:noexport:\n\n%?"
             :target (file+head "%<%d%m%Y%H%M%S>-''${slug}.org"
                 "#+date: [%<%d-%m-%Y %a %H:%M>]\n#+category: ''${title}\n#+filetags:\n#+title: ''${title}\n") :unnarrowed t)
            ("s" "story" plain "* Table of Contents :toc:noexport:\n\n* Resources\n- [%?[][jira]]\n\n* Context\n\n* Scope\n\n* Implementation\n\n* AC\n\n* Testing\n\n"
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
       (require 'org-roam-protocol)

       ;; Register org-roam jumps in evil's jump list so C-o / C-i work
       (with-eval-after-load 'evil
         (advice-add 'org-roam-node-find  :before (lambda (&rest _) (evil-set-jump)))
         (advice-add 'org-roam-node-visit :before (lambda (&rest _) (evil-set-jump)))
         (advice-add 'org-mark-ring-goto  :before (lambda (&rest _) (evil-set-jump)))
         (advice-add 'org-open-at-point   :before (lambda (&rest _) (evil-set-jump)))))


       ;; for now only used by org-roam-ui so I did not put it into core.nix
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
