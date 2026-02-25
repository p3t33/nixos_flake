{ pkgs, ... }:
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
}
