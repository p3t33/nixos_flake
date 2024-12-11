# This file can only be included if home-manger is imported to nixos
# as flake input.
{
  inputs,
  machineName,
  config,
  ...
}:
{
  home-manager = {
    useGlobalPkgs = true;

    # in case of conflict between existing file in ~/ and one managed by home-manager,
    # a .backup fle will be created automatically.
    backupFileExtension = "backup";

    useUserPackages = true;
    extraSpecialArgs = {
      inherit inputs;
      inherit machineName;
    };
    users.${config.userDefinedGlobalVariables.primeUsername} = import config.userDefinedGlobalVariables.home_manger_import_path;
    # will be available to all users managed by home manager
    sharedModules = [
      # allows to use sops-nix subset of NixOS module.
      # This configuration is done in addition to sops-nix that is used system wide
      # as part of the OS(inputs.sops-nix.nixosModules.sops). And is intended for handling
      # secrets in user home directory.
      #
      # Before using sops-nix as home manger module, I used the system wide module to
      # for secrets that are part of the home directory and it worked fine up to the
      # point where I installed the configuration on a new machine and noticed that
      # when using nixos-anywhere path in which I put the file(E.g ~/.config/git/my_extra_confg,
      # ~/.ssh)
      # the ownership of the top level directory where the secret got generated got messed up
      # and was set as root which caused partial installation of the configuration, with
      # home-manger failing to switch.
      inputs.sops-nix.homeManagerModules.sops
    ];
  };
}
