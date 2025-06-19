{ lib, config, ... }:

let
  cfg = config.customOptions.enableModule.emacs;
in
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
  # Core is the one to define the customOptions.enableModule.emacs and this
  # module is the one to enable cofnig.programs.emacs.enable
  #
  # All other modules use config.program.emacs.enable as the logic to to enable them.
  # As I broke my emacs config into fragments so it will be easier to maintain and develop
  # It they all should depend on a single switch to be enabled.
  #
  # it is possible to create customOptions.enableModule.emacsUI to get more control
  # over the individual fragments but for now I don"t see the need.
  ./core.nix
  ./keybindings.nix
  ./ui.nix
  ./spell_checker.nix
  ./evil.nix
  ./org.nix
  ./search_and_select.nix
  ./dired.nix
  ./navegation_and_shell.nix
  ./rss.nix
  ./git.nix
  ./completion.nix
];
}
