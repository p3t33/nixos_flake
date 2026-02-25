{ pkgs, config, lib, ... }:
let
  #xdg.dataHome used ro specify a path while xdg.dataFile used for creating files
  # in the same path.
  autoSaveDirectoryName = "emacs_autosave";
  autoSaveDirectoryPath = "${config.xdg.dataHome}/${autoSaveDirectoryName}";
  autoBackupDirectoryPatch = "${config.xdg.dataHome}/Trash/files";

in
{

  config = lib.mkIf config.programs.emacs.enable {
  # This is a fix to make the system use universal-ctags package instead
  # of the ctags package that is provided by emacs. The order in which
  # the packges are listed is important and by listing universal-ctags first
  # I am making sure that it will be the one used by the system.
  #
  # Note I am not sure if this is the right way to go about this problem
  # and I am not sure how this will effect Emacs when it will try to call
  # ctags.
  home.packages = with pkgs; [
    universal-ctags
  ];

  programs.emacs.extraPackages = epkgs: with epkgs; [
    sudo-edit
    use-package
    tldr
    projectile
  ];


  # Will create the dedined path for emacs auto save
  # an empty .keep file is a workaround to create an empty directory.
  xdg.dataFile."${autoSaveDirectoryName}/.keep".text = "";

  # enables emacs daemon
  services.emacs.enable = true;

  programs.emacs = {
    package = pkgs.emacs30;

    # programs.emcas.extraConfig is responsible to create a configuration file for
    # Emacs but this file, although interpreted as init.el(in logs) isn't located in ~/.emacs.d.
    extraConfig = lib.mkOrder 100 ''
      ;; disabling the default built int plugin manager
      ;; in context of nix these settings are essentially ensuring a
      ;; clean slate" in terms of package management behavior in Emacs
      ;; They ensure that certain built-in package manager features that
      ;; might conflict with an external management tool are turned
      ;; off or are in their most permissive settings.
      ;;
      ;; This should be always at the top of the file before any
      ;; other settings have the chance to run.
      (setq package-quickstart nil)
      (setq package-menu-async nil)
      (setq package-load-list '(all))

      ;; Set UTF-8 as the default encoding
      (prefer-coding-system 'utf-8)
      (set-default-coding-systems 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)

      ;; smoth scroll with mergin of lines
      ;; =================================
      (setq scroll-margin 8)
      (setq-default scroll-conservatively 100 scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)
      (pixel-scroll-mode 1)
      (pixel-scroll-precision-mode 1)
      ;; =================================

      ;; By default, Emacs creates automatic backups of files in their
      ;; original directories, such “file.el” and the backup “file.el~”.
      ;; This leads to a lot of clutter, so this setting defines a single
      ;; path to put all the backup into.
      (setq backup-directory-alist '((".*" . "${autoBackupDirectoryPatch}")))


      ;; By default, Emacs creates automatic saves of files in their
      ;; original directories, such “file.el” and the autosave “#file.el#”.
      ;; This leads to a lot of clutter, so this setting defines a single
      ;; path to put all the autosave files.
      (setq auto-save-file-name-transforms '((".*" "${autoSaveDirectoryPath}" t)))


      ;; Load windmove for directional window navigation
      ;; Used by buf-move-* functions in keybindings.nix
      (require 'windmove)

      ;; Ensure the use-package macro is available
      ;; This needs to be the first plugin related settings as
      ;; every pluging that uses this macro in order to use it
      ;; for its own configuration needs to be loaded only after
      ;; this macro has been loaded.
      (require 'use-package)

      (use-package sudo-edit
          :ensure nil)

      (use-package tldr
       :ensure nil)

      (use-package projectile
       :ensure nil
       :config
       (projectile-mode 1))

      ;; Sane defaults
      ;;=======================
      (delete-selection-mode 1)    ;; You can select text and delete it by typing.
      (electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
      (electric-pair-mode 1)       ;; Turns on automatic parens pairing
      (global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
      (global-display-line-numbers-mode 1) ;; Display line numbers
      (global-visual-line-mode t)  ;; Enable truncated lines
      (menu-bar-mode -1)           ;; Disable the menu bar
      (scroll-bar-mode -1)         ;; Disable the scroll bar
      (tool-bar-mode -1)           ;; Disable the tool bar
      ;;=======================


      ;; Line spacing and line number style
      ;;=======================
      ;; Uncomment the following line if line spacing needs adjusting.
      (setq-default line-spacing 0.12)

      (setq display-line-numbers-type 'relative)
      ;;=======================

      ;; simple keys to zoom in and out
      ;; ==============================
      (global-set-key (kbd "C-=") 'text-scale-increase)
      (global-set-key (kbd "C--") 'text-scale-decrease)
      (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
      (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
      ;; =============================

      ;; mini buffer escape
      (global-set-key [escape] 'keyboard-escape-quit)
'';
};
};
}

