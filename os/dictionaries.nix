{ pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
        # Added both hunspell and aspell they should not conplict.
        # Added both because some applicatons might only use one of them.
        hunspell
        hunspellDicts.en-us
        aspell
        aspellDicts.en
    ];
}

