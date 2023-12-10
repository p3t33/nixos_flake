{ inputs, config, ... }:
{
    home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = {inherit inputs;};
        users.${config.userDefinedGlobalVariables.username} = import config.userDefinedGlobalVariables.home_manger_import_path;
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
