{ inputs, pkgs, ... }:
{
    home.file = {
# Put the pre-generated nix-index database in place,
# used for command-not-found.
        ".cache/nix-index/files".source =
            inputs.nix-index-database.legacyPackages.${pkgs.system}.database;
    };

    programs.nix-index =
    {
        enable = true;
        enableZshIntegration = true;
    };
}


