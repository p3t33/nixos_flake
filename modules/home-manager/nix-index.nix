{ inputs, pkgs, ... }:
{

    # For this to work you will need to execute nix-index from the cli.
    # In the past I was using:

    # home.file = {
    # Put the pre-generated nix-index database in place,
    # used for command-not-found.
    #    ".cache/nix-index/files".source =
    #        inputs.nix-index-database.legacyPackages.${pkgs.system}.database;
    #};


    # but database is no longer a valid variable and I need some time to go
    # over the changes in nix-index input to see how to fix it, if it even possible.
    programs.nix-index =
    {
        enable = true;
        enableZshIntegration = true;
    };
}


