{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "drone";
  home.homeDirectory = "/home/drone";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  imports = [
    ./home/vm_home
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    SUDO_EDITOR = "nvim";
  };

  home.packages = with pkgs; [
    zsh-syntax-highlighting
    zsh-autosuggestions
    thefuck
  ];

    programs.zsh = {
       history.ignorePatterns = [ "ls" "cd *" "pwd" "reboot" "history" ];
       shellAliases = {
         update = "sudo nixos-rebuild switch --flake /home/drone/system#kvm-nixos";
         upgrade = "sudo nix flake update && update";
         list-generations = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
         clean-generations = "sudo nix-collect-garbage --delete-older-than 2d";
         rollback = "sudo nixos-rebuild switch --rollback";
         ls = "exa --icons --color=always --group-directories-first";
         ll = "exa -l --icons --color=always --group-directories-first";
         lt = "exa -aT --icons --color=always --group-directories-first";
         gc89 = "gcc -ansi -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
         gc89d = "gcc -ansi -pedantic-errors -Wall -Wextra -g";
         gc99 = "gcc -std=c99 -pedantic-errors -Wall -Wextra -g  -DNDEBUG -O3";
         gc99d = "gcc -std=c99 -pedantic-errors -Wall -Wextra -g";
         gpp17 = "g++ -std=c++17 -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
         gpp17d = "g++ -std=c++17 -pedantic-errors -Wall -Wextra -g";
         clpp17 = "clang++ -std=c++17 -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
         clpp17d = "clang++ -std=c++17 -pedantic-errors -Wall -Wextra -g";
         vlg = "valgrind --leak-check=yes --track-origins=yes";
         grind = "rm callgrind.out.* && valgrind --tool=callgrind ./a.out &&  callgrind.out.*";

       };
       # there are two types of plugins.
       # the one that are part of the shell(like the two bellow),
       # and then one that are part of oh-my-zsh.
       enableAutosuggestions = true;
       enableSyntaxHighlighting = true;

      enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = [
          "git"
          "command-not-found"
          "thefuck"
          "history-substring-search"
          "colored-man-pages"
          "last-working-dir"
          "sudo"
          "web-search"
          "z"
        ];
        # Not used because I am using starship prompt.
        #theme = "agnoster";
        #theme = "avit";
      };
    };
}
