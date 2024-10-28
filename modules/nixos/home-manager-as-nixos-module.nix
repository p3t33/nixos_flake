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
      # as I am using this config file as part of NixOS and using home
      # manager as NixOS module I don't really need this input as
      # I can just use can just use the NixOS module that is more powerfull.
      #
      # The reason I decided to include this input is because I have
      # stand alone home manger settings(as part of this flake) that I am
      # using with a generic Linux such as Ubuntu and I might be able
      # to share configurations between the stand alone home manger and the
      # one that is in this file and is used as NixOS module.
      inputs.sops-nix.homeManagerModules.sops
    ];
  };
}
