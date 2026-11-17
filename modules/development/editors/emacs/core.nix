# NOTE: I mostly use emacs for org-roam so the rest of the config were not fully
# tested(E.g git) when I started refactoring.
#
# programs.emacs.extraConfig is responsible to create a configuration file for
# Emacs but this file, although interpreted as init.el(in logs) isn't located in ~/.emacs.d.
# To find the generated default.el, follow:
#   $(which emacs) → bin/.emacs-wrapped → emacs-packages-deps → share/emacs/site-lisp/default.el

# The discovered components stitch together programs.emacs.extraConfig.
# Outer priorities preserve component composition, package order, and tied Lisp order;
# inner priorities still control the ordering of sections within the same option:
#
#   100 = core, 200 = evil, 300 = ui, 400 = keybindings,
#   500 = other sections, 600 = completion.

{ lib, ... }:
{
  flake.modules = {
    nixos.emacs = { pkgs, ... }:
{
  # NOTICE: This module is not used and not maintained at the moment.
  #
  # The init.el that is created by the extraConfig in for Emacs by home-manager
  # does not gets picked up by the service, instead you need to make sure
  # that ~/.emacs.d/init.el exist(I used home.file to create one instead of
  # using extraConfig).
  #
  # The systemd unit that is mentioned in the Emacs documentation has a setting for the
  # ssh agent socket(Environment=SSH_AUTH_SOCK=%t/keyring/ssh) looking(systemctl --user cat emacs.service) at the
  # unit that is created by enabling this daemon I do not see any definition for it and so
  # I am not sure if remote editing via ssh will work or I will need to create my own systemd
  # unit to include this definition.
  services.emacs = {
    enable = true;

    # I am not sure why but packages need to be explicitly stated as part of the daemon
    # configuration for them being picked up by the daemon. Without doing this
    # I was getting a lot of errors.
    #
    # When emacs executed as server from cli and not inside systemd unit
    # it is able to find the packages without the need to explicitly set
    # them here as well.
    # Review this explicit Emacs version choice on every NixOS stable upgrade.
    package =
      with pkgs;
      ((emacsPackagesFor emacs30).emacsWithPackages (epkgs: [
        epkgs.use-package
        epkgs.evil
        epkgs.evil-collection
        epkgs.general
        epkgs.toc-org
        epkgs.org-bullets
        epkgs.which-key
        epkgs.sudo-edit
        epkgs.counsel
        epkgs.ivy
        epkgs.all-the-icons-ivy-rich
        epkgs.ivy-rich
        epkgs.counsel
        epkgs.prescient
        epkgs.ivy-prescient
        epkgs.all-the-icons
        epkgs.all-the-icons-dired
        epkgs.dired-open
        epkgs.peep-dired
        epkgs.neotree
        epkgs.eshell-syntax-highlighting
        epkgs.vterm
        epkgs.vterm-toggle
        epkgs.rainbow-mode
        epkgs.rainbow-delimiters
        epkgs.dashboard
        epkgs.projectile
        epkgs.diminish
        epkgs.lua-mode
        epkgs.flycheck
        epkgs.lsp-mode
        epkgs.company
        epkgs.company-box
        epkgs.nord-theme
        epkgs.vscode-dark-plus-theme
        epkgs.doom-themes
        epkgs.doom-modeline
        epkgs.elfeed
        epkgs.elfeed-goodies
        epkgs.git-timemachine
        epkgs.magit
        epkgs.hl-todo
        epkgs.tldr
        epkgs.org-roam
        epkgs.org-download
        epkgs.org-tree-slide
        epkgs.undo-tree
        epkgs.ox-jira
        epkgs.plantuml-mode
        epkgs.websocket
        epkgs.org-roam-ui
        epkgs.org-transclusion
      ]));
  };
};
    homeManager.emacs = lib.mkOrder 100 ({ pkgs, config, lib, ... }:
let
  #xdg.dataHome used ro specify a path while xdg.dataFile used for creating files
  # in the same path.
  autoSaveDirectoryName = "emacs_autosave";
  autoSaveDirectoryPath = "${config.xdg.dataHome}/${autoSaveDirectoryName}";
  autoBackupDirectoryPath = "${config.xdg.dataHome}/Trash/files";

in
{

  config = lib.mkIf config.programs.emacs.enable {
  # This is a fix to make the system use universal-ctags package instead
  # of the ctags package that is provided by emacs. The order in which
  # the packages are listed is important and by listing universal-ctags first
  # I am making sure that it will be the one used by the system.
  #
  # Note I am not sure if this is the right way to go about this problem
  # and I am not sure how this will effect Emacs when it will try to call
  # ctags.
  home.packages = lib.mkOrder 400 (with pkgs; [
    universal-ctags
  ]);

  programs.emacs.extraPackages = epkgs: with epkgs; [
    sudo-edit
    tldr
  ];


  # Will create the defined path for emacs auto save
  # an empty .keep file is a workaround to create an empty directory.
  xdg.dataFile."${autoSaveDirectoryName}/.keep".text = "";

  # enables emacs daemon
  services.emacs = {
    enable = true;
    startWithUserSession = lib.mkIf config.wayland.windowManager.sway.enable "graphical";
  };

  # Only affects the generated emacsclient desktop file used by launchers like rofi.
  # It does not change how emacsclient behaves when called directly, e.g. by git.
  services.emacs.client = {
    enable = true;
    arguments = [ "--alternate-editor=" "--create-frame" "--no-wait" ];
  };

  programs.emacs = {
    # Review this explicit version choice on every NixOS stable upgrade.
    package = pkgs.emacs30-pgtk;

    # programs.emacs.extraConfig is responsible to create a configuration file for
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

      ;; Separate Emacs internal clipboard from the system clipboard
      ;; This allows 'y' to yank only into Emacs, and requires explicit
      ;; use of the '+' register (or a custom keybind) to yank to the OS clipboard.
      (setq select-enable-clipboard nil)
      (setq select-enable-primary nil)

      (let ((wayland-clipboard-bin "${pkgs.wl-clipboard}/bin"))
        (add-to-list 'exec-path wayland-clipboard-bin)
        (setenv "PATH"
                (concat wayland-clipboard-bin ":" (or (getenv "PATH") ""))))

      ;; smooth scroll with margin of lines
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
      (setq backup-directory-alist '((".*" . "${autoBackupDirectoryPath}")))


      ;; By default, Emacs creates automatic saves of files in their
      ;; original directories, such “file.el” and the autosave “#file.el#”.
      ;; This leads to a lot of clutter, so this setting defines a single
      ;; path to put all the autosave files.
      (setq auto-save-file-name-transforms '((".*" "${autoSaveDirectoryPath}" t)))


      ;; Load windmove for directional window navigation
      ;; Used by buf-move-* functions in keybindings.nix
      (require 'windmove)

      (use-package sudo-edit
          :ensure nil)

      (use-package tldr
       :ensure nil)

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
});
  };
}
