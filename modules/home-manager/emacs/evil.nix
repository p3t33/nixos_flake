{ pkgs, ... }:
{
  programs.emacs.extraPackages = epkgs: with epkgs; [
    evil
    evil-collection
  ];
  programs.emacs.extraConfig = ''
      ;; =================================
      ;; Configure and initialize evil-mode
      ;; =================================
      (use-package evil
       :ensure nil
       :init
       ;; evil-collection assumes evil-want-keybinding is set to nil and
       ;; evil-want-integration is set to t before loading evil and evil-collection.
       ;; Note some other packages may load evil (e.g. evil-leader) so bear that
       ;; in mind when determining when to set the variables.
       (setq evil-want-integration t) ;; This is optional since it's already set to t by default
       (setq evil-want-keybinding nil)

       (setq evil-vsplit-window-right t)
       (setq evil-split-window-below t)
       :config
       (evil-mode 1)

       (defun save-and-switch-to-normal-mode ()
        "Save the current buffer and switch to normal mode, preserving cursor position."
        (interactive)
        ;; Save the current point as a marker so it moves with buffer changes
        (let ((pos (point-marker)))
         ;; Save the buffer
         (save-buffer)
         ;; Restore the cursor position
         (goto-char pos)
         ;; Clean up the marker to avoid a potential resource leak
         (set-marker pos nil))
        ;; Switch to normal mode
        (evil-normal-state))

        ;; Set the key bindings for normal and insert modes
        (define-key evil-normal-state-map (kbd "C-s") 'save-and-switch-to-normal-mode)
        (define-key evil-insert-state-map (kbd "C-s") 'save-and-switch-to-normal-mode))

        ;; recenter org around search string
        ;; same as:
        ;;vim.keymap.set('n', 'n', "nzzzv", opts)
        ;; --------------------------------------
        (defadvice evil-search-next
         (after advice-for-evil-search-next activate)
         (evil-scroll-line-to-center (line-number-at-pos)))

        (defadvice evil-search-previous
         (after advice-for-evil-search-previous activate)
         (evil-scroll-line-to-center (line-number-at-pos)))
           ;; --------------------------------------

        (use-package evil-collection
         :ensure nil
         :after evil
         :config
         ;; limites the mode list to intergrate with, might chagne this
         ;; in the future.
         (setq evil-collection-mode-list '(dashboard dired ibuffer))
         (evil-collection-init))

        ;; Using RETURN to follow links in Org/Evil
        ;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
        (with-eval-after-load 'evil-maps
         (define-key evil-motion-state-map (kbd "SPC") nil)
         (define-key evil-motion-state-map (kbd "RET") nil)
         (define-key evil-motion-state-map (kbd "TAB") nil))
        ;; Setting RETURN key in org-mode to follow links

        (with-eval-after-load 'evil-maps
         (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up))


        (evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
        (evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
        (evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
        (evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)
           ;; =================================
  '';
}
