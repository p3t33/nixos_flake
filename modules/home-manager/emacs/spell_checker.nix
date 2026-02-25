{ lib, config, ... }:

let
  cfg = config.programs.emacs;
in
{

  config = lib.mkIf cfg.enable {

  programs.emacs.extraPackages = epkgs: with epkgs; [
    flycheck
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
       ;; Set backend program before flyspell loads fully
       (setq ispell-program-name "aspell")
       :config

       ;; A more vim like keybindis for flyspell
       ;;---------------------------------------
       (defun costum-flyspell-previous-and-correct ()
        "Correct previous misspelling or current if already on a misspelled word."
        (interactive)
        (unless (flyspell-overlay-p (point))
         ;; Attempt to move to previous error using Evil's command
         (condition-case nil (call-interactively 'evil-prev-flyspell-error) (error nil)))
        ;; Correct word at point (Flyspell's function)
        (call-interactively 'flyspell-correct-word-before-point))

       (defun costum-flyspell-next-and-correct ()
        "Correct next misspelling or current if already on a misspelled word."
        (interactive)
        (unless (flyspell-overlay-p (point))
         ;; Attempt to move to next error using Evil's command
         (condition-case nil (call-interactively 'evil-next-flyspell-error) (error nil)))
        ;; Correct word at point (Flyspell's function)
        (call-interactively 'flyspell-correct-word-before-point))
       ;;---------------------------------------

       ;; Keybindings using general.el (ensure general is loaded first)
       (with-eval-after-load 'general
        (general-def :states 'normal
         "]s" #'costum-flyspell-next-and-correct   ;; Use #' for functions
         "[s" #'costum-flyspell-previous-and-correct)) ;; Use #' for functions
       )
  '';
  };
}
