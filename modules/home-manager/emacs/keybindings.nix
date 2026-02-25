{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{
  config = lib.mkIf cfg.enable {
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      general
    ];

    extraConfig = lib.mkOrder 400 ''
      (defun insert-active-timestamp ()
       "Inserts an active timestamp with the current date and time up to minutes."
       (interactive)
       (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))

      (defun insert-inactive-timestamp ()
       "Inserts an inactive timestamp with the current date and time up to minutes."
       (interactive)
       (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))

      ;; Org mode Meta keybinding
      ;; ==============================

      (with-eval-after-load 'org
       ;; Ensure "M-h" is unbound in org-mode specifically
       (define-key org-mode-map (kbd "M-h") nil)
       (define-key org-mode-map (kbd "M-h") 'org-promote-subtree)

       ;; Set "M-l" for demoting subtree globally
       (define-key org-mode-map (kbd "M-l") 'org-demote-subtree)

       ;; Bind keys for moving headings up and down in Org mode
       (define-key org-mode-map (kbd "M-j") 'org-move-subtree-down)
       (define-key org-mode-map (kbd "M-k") 'org-move-subtree-up)



       ;; Custom function to prevent M-RET from splitting list items
       (defun my/org-meta-return-dwim ()
        "Insert a new list item at the end of the current item without splitting the line.
        If in Normal mode, switch to Insert mode automatically and place the cursor after the new bullet."
        (interactive)
        (if (org-at-item-p)
         (progn
          (end-of-line)
          (org-meta-return)
          (when (evil-normal-state-p)
           (evil-append-line 1))) ;; Enters insert mode at end of new line
         (org-meta-return)))

       ;; Override M-RET behavior in Org Mode
       (define-key org-mode-map (kbd "M-RET") 'my/org-meta-return-dwim))
      ;; ==============================


      ;; Buffer Movement Helpers (Windmove-based) require (require 'windmove) which I put into core.nix
      ;; ========================================
      ;;;###autoload
      (defun buf-move-up ()
          "Swap the current buffer and the buffer above the split.
          If there is no split, ie now window above the current one, an
          error is signaled."
       ;;  "Switches between the current buffer, and the buffer above the
       ;;  split, if possible."
          (interactive)
          (let* ((other-win (windmove-find-other-window 'up))
             (buf-this-buf (window-buffer (selected-window))))
          (if (null other-win)
              (error "No window above this one")
              ;; swap top with this one
              (set-window-buffer (selected-window) (window-buffer other-win))
              ;; move this one to top
              (set-window-buffer other-win buf-this-buf)
              (select-window other-win))))

      ;;;###autoload
      (defun buf-move-down ()
       "Swap the current buffer and the buffer under the split.
       If there is no split, ie now window under the current one, an
       error is signaled."
       (interactive)
       (let* ((other-win (windmove-find-other-window 'down))
              (buf-this-buf (window-buffer (selected-window))))
        (if (or (null other-win)
             (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
         (error "No window under this one")
         ;; swap top with this one
         (set-window-buffer (selected-window) (window-buffer other-win))
         ;; move this one to top
         (set-window-buffer other-win buf-this-buf)
         (select-window other-win))))

      ;;;###autoload
      (defun buf-move-left ()
       "Swap the current buffer and the buffer on the left of the split.
       If there is no split, ie now window on the left of the current
       one, an error is signaled."
       (interactive)
       (let* ((other-win (windmove-find-other-window 'left))
              (buf-this-buf (window-buffer (selected-window))))
        (if (null other-win)
         (error "No left split")
         ;; swap top with this one
         (set-window-buffer (selected-window) (window-buffer other-win))
         ;; move this one to top
         (set-window-buffer other-win buf-this-buf)
         (select-window other-win))))

      ;;;###autoload
      (defun buf-move-right ()
       "Swap the current buffer and the buffer on the right of the split.
       If there is no split, ie now window on the right of the current
       one, an error is signaled."
       (interactive)
       (let* ((other-win (windmove-find-other-window 'right))
              (buf-this-buf (window-buffer (selected-window))))
        (if (null other-win)
         (error "No right split")
         ;; swap top with this one
         (set-window-buffer (selected-window) (window-buffer other-win))
         ;; move this one to top
         (set-window-buffer other-win buf-this-buf)
         (select-window other-win))))
    ;; ===================================




      (use-package general
           :ensure nil
           :config
           (general-evil-setup)

           ;; I treid to use general-create-definer and had some issues
           ;; so for now general-def will do.
           (general-def :states '(normal insert visual emacs)
            :keymaps 'override
            :prefix "SPC"
            :global-prefix "M-SPC"
            ;;
            ;; general
            ;; -------
            "SPC" '(counsel-M-x :wk "Counsel M-x")
            "." '(find-file :wk "Find file")
            "f r" '(counsel-recentf :wk "Find recent files")
            "f f" '(counsel-fzf :wk "Find files")
            "f n" '(org-roam-node-find :which-key "find org-roam node")
            "f s" '(counsel-rg :wk "Find string")
            "f b" '(counsel-switch-buffer :wk "Find buffer")
            "f h" '(counsel-org-goto :wk "Find org file header")
            "TAB TAB" '(comment-line :wk "Comment lines")
            ;;
            ;; sudo on files
            ;; -------------
            "fu" '(sudo-edit-find-file :wk "Sudo find file")
            "fU" '(sudo-edit :wk "Sudo edit file")




            ;; Buffer/bookmarks
            ;; -----
            "b" '(:ignore t :wk "Bookmarks/Buffers")
            "b b" '(counsel-switch-buffer :wk "Switch buffer")
            "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
            "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
            "b d" '(bookmark-delete :wk "Delete bookmark")
            "b i" '(ibuffer :wk "Ibuffer")
            "b k" '(kill-this-buffer :wk "Kill this buffer")
            "b K" '(kill-some-buffers :wk "Kill multiple buffers")
            "b l" '(list-bookmarks :wk "List bookmarks")
            "b m" '(bookmark-set :wk "Set bookmark")
            "b n" '(next-buffer :wk "Next buffer")
            "b p" '(previous-buffer :wk "Previous buffer")
            "b r" '(revert-buffer :wk "Reload buffer")
            "b R" '(rename-buffer :wk "Rename buffer")
            "b s" '(basic-save-buffer :wk "Save buffer")
            "b S" '(save-some-buffers :wk "Save multiple buffers")
            "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
            "b y" '(yank-buffer :wk "Yank buffer")

            ;; git
            ;; ---------
            "g" '(:ignore t :wk "Git")
            "g /" '(magit-dispatch :wk "Magit dispatch")
            "g ." '(magit-file-dispatch :wk "Magit file dispatch")
            "g b" '(magit-branch-checkout :wk "Switch branch")
            "g c" '(:ignore t :wk "Create")
            "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
            "g c c" '(magit-commit-create :wk "Create commit")
            "g c f" '(magit-commit-fixup :wk "Create fixup commit")
            "g C" '(magit-clone :wk "Clone repo")
            "g f" '(:ignore t :wk "Find")
            "g f c" '(magit-show-commit :wk "Show commit")
            "g f f" '(magit-find-file :wk "Magit find file")
            "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
            "g F" '(magit-fetch :wk "Git fetch")
            "g g" '(magit-status :wk "Magit status")
            "g i" '(magit-init :wk "Initialize git repo")
            "g l" '(magit-log-buffer-file :wk "Magit buffer log")
            "g r" '(vc-revert :wk "Git revert file")
            "g s" '(magit-stage-file :wk "Git stage file")
            "g t" '(git-timemachine :wk "Git time machine")
            "g u" '(magit-stage-file :wk "Git unstage file")

            ;; emacs help
            ;; ----------
            "h" '(:ignore t :wk "Help")
            "h a" '(counsel-apropos :wk "Apropos")
            "h b" '(describe-bindings :wk "Describe bindings")
            "h c" '(describe-char :wk "Describe character under cursor")
            "h d" '(:ignore t :wk "Emacs documentation")
            "h d a" '(about-emacs :wk "About Emacs")
            "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
            "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
            "h d m" '(info-emacs-manual :wk "The Emacs manual")
            "h d n" '(view-emacs-news :wk "View Emacs news")
            "h d o" '(describe-distribution :wk "How to obtain Emacs")
            "h d p" '(view-emacs-problems :wk "View Emacs problems")
            "h d t" '(view-emacs-todo :wk "View Emacs todo")
            "h d w" '(describe-no-warranty :wk "Describe no warranty")
            "h e" '(view-echo-area-messages :wk "View echo area messages")
            "h f" '(describe-function :wk "Describe function")
            "h F" '(describe-face :wk "Describe face")
            "h g" '(describe-gnu-project :wk "Describe GNU Project")
            "h i" '(info :wk "Info")
            "h I" '(describe-input-method :wk "Describe input method")
            "h k" '(describe-key :wk "Describe key")
            "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
            "h L" '(describe-language-environment :wk "Describe language environment")
            "h m" '(describe-mode :wk "Describe mode")
            "h r" '(:ignore t :wk "Reload")
            "h t" '(load-theme :wk "Load theme")
            "h v" '(describe-variable :wk "Describe variable")
            "h w" '(where-is :wk "Prints keybinding for command if set")
            "h x" '(describe-command :wk "Display full documentation for command")

            ;;
            ;; Toggle
            ;; ------
            "t" '(:ignore t :wk "Toggle")
            "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
            "t t" '(visual-line-mode :wk "Toggle truncated lines")
            "t v" '(vterm-toggle :wk "Toggle vterm")
            ;;
            ;; Search
            ;; ------
            "s" '(:ignore t :wk "Search")
            "s d" '(dictionary-search :wk "Search dictionary")
            "s m" '(man :wk "Man pages")
            "s t" '(tldr :wk "Lookup TLDR docs for a command")
            "s w" '(woman :wk "Similar to man but doesn't require man")

            ;;
            ;; Windows
            ;; -------
            "w" '(:ignore t :wk "Windows")
            ;; Window splits
            "w c" '(evil-window-delete :wk "Close window")
            "w n" '(evil-window-new :wk "New window")
            "w s" '(evil-window-split :wk "Horizontal split window")
            "w v" '(evil-window-vsplit :wk "Vertical split window")
            ;; Window motions
            "w h" '(evil-window-left :wk "Window left")
            "w j" '(evil-window-down :wk "Window down")
            "w k" '(evil-window-up :wk "Window up")
            "w l" '(evil-window-right :wk "Window right")
            "w w" '(evil-window-next :wk "Goto next window")
            ;; Move Windows
            "w H" '(buf-move-left :wk "Buffer move left")
            "w J" '(buf-move-down :wk "Buffer move down")
            "w K" '(buf-move-up :wk "Buffer move up")
            "w L" '(buf-move-right :wk "Buffer move right")
            ;;
            ;; org-mode keybindings
            "m" '(:ignore t :wk "Org")
            "m a" '(org-agenda :wk "Org agenda")
            "m e" '(org-export-dispatch :wk "Org export dispatch")
            "m l" '(org-toggle-item :wk "Org toggle list item")
            "m u" '(org-roam-ui-mode :wk "toggle org-roam-ui-mode")
            "m t" '(org-todo :wk "Org todo")
            "m q" '(lambda () (interactive) (if (org-entry-is-todo-p) (org-todo) (org-todo "QUESTION")))
            "m B" '(org-babel-tangle :wk "Org babel tangle")
            "m T" '(org-todo-list :wk "Org todo list")
            "m b" '(:ignore t :wk "Tables")
            "m b -" '(org-table-insert-hline :wk "Insert hline in table")
            "m d" '(:ignore t :wk "Date/deadline")
            "m d t" '(insert-inactive-timestamp :which-key "Insert inactive timestamp")
            "m d a" '(insert-active-timestamp :which-key "Insert active timestamp")
            "m h" '(org-toggle-heading :wk "Org toggle heading")
            "m i" '(:ignore t :wk "Org insert")
            "m i l" '(org-insert-link :wk "Insert Org link")
            "m i n" '(org-roam-node-insert :which-key "org-roam node insert")
            "m i i" '(org-id-get-create :which-key "assign Org-roam ID to heading")
            "m i c" '(org-download-clipboard :which-key "insert clipboard")



            ;; Org-roam
            ;; find is bound under find
            "m r" '(org-roam-buffer-toggle :which-key "list node refernces")
            "m g" '(org-roam-graph :which-key "org-roam graph")
            "m c" '(org-roam-capture :which-key "org-roam capture")
            "m j" '(org-roam-dailies-capture-today :which-key "org-roam dailies capture today")
            "m n" '(:ignore t :wk "org-roam node")
            "m n d" '(custom-org-roam-delete-node-and-sync :wk "Delete and sync Org-roam node")
            "m n r" '(custom-org-roam-rename-node  :wk "Rename node and sync Org-roam node")

            ;; Open
            ;; ----
            "o" '(:ignore t :wk "Open")
            "o d" '(dashboard-open :wk "Dashboard")
            "o e" '(elfeed :wk "Elfeed RSS")
            "o f" '(make-frame :wk "Open buffer in new frame")
            "o F" '(select-frame-by-name :wk "Select frame by name")

            ;; Evaluate
            ;; -------
            "e" '(:ignore t :wk "Eshell/Evaluate")
            "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
            "ec" '((lambda () (interactive) (load-file user-init-file)) :wk "Evaluate config")
            "ed" '(eval-defun :wk "Evaluate defun containing or after point")
            "ee" '(eval-expression :wk "Evaluate and elisp expression")
            "eh" '(counsel-esh-history :which-key "Eshell history")
            "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
            "er" '(eval-region :wk "Evaluate elisp in region")
            "es" '(eshell :which-key "Eshell"))
      )

    '';
  };
  };
}
