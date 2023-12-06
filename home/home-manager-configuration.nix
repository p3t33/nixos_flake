{ inputs, config, ... }:
{
    home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = {inherit inputs;};
        users.${config.userDefinedGlobalVariables.username} = import config.userDefinedGlobalVariables.home_manger_import_path;
    };
}
