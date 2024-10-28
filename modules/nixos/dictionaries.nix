{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Added both hunspell and aspell spell checkers they should not conplict.
    # Added both because some applicators might only use one of them.
    hunspell
    hunspellDicts.en-us
    aspell
    aspellDicts.en
  ];
}
