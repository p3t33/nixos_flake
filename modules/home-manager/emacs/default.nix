{

# NOTE: I mosly use emacs for org-roam so the reset of the config were not fully
# tested(E.g git) when I started refactoring.
#
# programs.emacs.extraConfig is responsible to create a configuration file for
# Emacs but this file, although interpreted as init.el(in logs) isn't located in ~/.emacs.d.
# To find the generated default.el, follow:
#   $(which emacs) → bin/.emacs-wrapped → emacs-packages-deps → share/emacs/site-lisp/default.el

# The imports will create programs.emcas.extraConfig that creates the configuration files
# that will be evaluated by emacs. extraConfig is "stitched" from multiple files.
# Each module uses lib.mkOrder to control evaluation order:
#   100 = core, 200 = evil, 300 = ui, 400 = keybindings, 500 = everything else.

imports = [
  # all moudles use config.programs.emacs.enable to be activate,
  # with Core being the base of configuration.
  ./core.nix
  ./keybindings.nix
  ./ui.nix
  ./spell_checker.nix
  ./evil.nix
  ./org.nix
  ./search_and_select.nix
  ./dired.nix
  ./navigation_and_shell.nix
  ./rss.nix
  ./git.nix
  ./completion.nix
];
}
