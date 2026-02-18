{

# NOTE: I mosly use emacs for org-roam so the reset of the config were not fully
# tested(E.g git) when I started refactoring.
#
# programs.emcas.extraConfig is responsible to create a configuration file for
# Emacs but this file, although interpreted as init.el(in logs) isn't located in ~/.emacs.d.

# The imports will create programs.emcas.extraConfig that creates the configuration files
# that will be evaluated by emacs. extraConfig is "stitched" from multiple files and
# the order in which they are imported is important.
#
# I did use use-package with :after to control how the packages are evaluated but I can't
# be sure it is covering 100% of all the edge cases, so the order should be kept as is.

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
