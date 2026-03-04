{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{

  config = lib.mkIf cfg.enable {

  programs.emacs.extraPackages = epkgs: with epkgs; [
    flycheck
    flyspell-correct
  ];

  programs.emacs.extraConfig = lib.mkOrder 500 ''
      ;; ===================
      ;; setup spell checker
      ;; ===================
      (use-package flyspell
       :ensure nil ;; Built-in package
       :hook ((text-mode . flyspell-mode)         ;; Enable in text modes
           (prog-mode . flyspell-prog-mode))  ;; Enable for comments/strings in prog modes
       :init
       (setq ispell-program-name "aspell")
       ;; slow mode uses phonetic + edit-distance scoring, closest to vim's "best"
       (setq ispell-extra-args '("--sug-mode=slow"))
       :config

       ;; A more vim like keybindings for flyspell
       ;;---------------------------------------
       (defun custom-flyspell-previous-and-correct ()
        "Correct previous misspelling and leave cursor at start of corrected word."
        (interactive)
        (unless (flyspell-overlay-p (point))
         (condition-case nil (call-interactively 'evil-prev-flyspell-error) (error nil)))
        (let ((word-start (cadr (flyspell-get-word))))
         (flyspell-correct-at-point)
         (when word-start (goto-char word-start))))

       (defun custom-flyspell-next-and-correct ()
        "Correct next misspelling and leave cursor at start of corrected word."
        (interactive)
        (unless (flyspell-overlay-p (point))
         (condition-case nil (call-interactively 'evil-next-flyspell-error) (error nil)))
        (let ((word-start (cadr (flyspell-get-word))))
         (flyspell-correct-at-point)
         (when word-start (goto-char word-start))))
       ;;---------------------------------------

       ;; Keybindings using general.el (ensure general is loaded first)
       (with-eval-after-load 'general
        (general-def :states 'normal
         "]s" #'custom-flyspell-next-and-correct
         "[s" #'custom-flyspell-previous-and-correct))
       )

      (use-package flyspell-correct
       :ensure nil
       :after flyspell
       :config
       ;; Wrap the default completing-read interface to cap candidates at 5, matching vim's "best,5"
       (defun my/flyspell-correct-limited (candidates word)
        (completing-read "Correct: " (seq-take candidates 5) nil t nil nil word))
       (setq flyspell-correct-interface #'my/flyspell-correct-limited))
  '';
  };
}
