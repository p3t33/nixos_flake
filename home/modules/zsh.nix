{ config, pkgs, ... }:

{
  home.packages = with pkgs; [

  # This are plugins that are installed directly with no
  # plugin manager.
  zsh-syntax-highlighting
  zsh-autosuggestions
  zsh-fzf-tab
  ];

  programs.zsh = {

    enable = true;

    history.ignorePatterns = [ "ls" "cd *" "pwd" "reboot" "history" ];

    shellAliases = {
      get_flake_repository_if_does_not_exit = "if [[ ! -d ${config.userDefinedGlobalVariables.pathToFlakeDirectory} ]]; then git clone ${config.userDefinedGlobalVariables.flakeRepositoryUrl} ${config.userDefinedGlobalVariables.pathToFlakeDirectory}; fi;";
      update = "get_flake_repository_if_does_not_exit; sudo nixos-rebuild switch --flake ${config.userDefinedGlobalVariables.pathToFlakeDirectory}#${config.userDefinedGlobalVariables.hostTag}";
      upgrade = "get_flake_repository_if_does_not_exit; sudo nix flake update ${config.userDefinedGlobalVariables.pathToFlakeDirectory} && update";
      list-generations = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
      cleanup = "sudo nix-collect-garbage --delete-older-than 2d";
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

    initExtra = ''
        export LANG="en_US.UTF-8";
    '';
    # There are two types of plugins.
    # the one that are part of the shell(like the once bellow),
    # and then one that are part of oh-my-zsh.
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    # This is also a non oh-my-zsh pluging but it doesn't have
    # a clean option(E.g enableFzfTab = true;)
    plugins = [ { name = "fzf-tab"; src = "${pkgs.zsh-fzf-tab}/share/fzf-tab"; } ];

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
        "vi-mode"
      ];
      # Not used because I am using starship prompt.
      #theme = "agnoster";
      #theme = "avit";
   };
  };

}

