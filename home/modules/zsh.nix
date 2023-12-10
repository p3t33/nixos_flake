{ config, pkgs, ... }:

{
  home.packages = with pkgs; [

  # This are plugins that are installed directly with no
  # plugin manager.
  zsh-syntax-highlighting
  zsh-autosuggestions
  zsh-fzf-tab
  thefuck
  ];

  programs.zsh = {

    enable = true;

    history.ignorePatterns = [ "ls" "cd *" "pwd" "reboot" "history" ];

    shellAliases = {
      get_flake_repository_if_does_not_exit = "if [[ ! -d ${config.userDefinedGlobalVariables.pathToFlakeDirectory} ]]; then ${pkgs.git}/bin/git clone ${config.userDefinedGlobalVariables.flakeRepositoryUrl} ${config.userDefinedGlobalVariables.pathToFlakeDirectory}; fi;";
      update = "get_flake_repository_if_does_not_exit; sudo nixos-rebuild switch --flake ${config.userDefinedGlobalVariables.pathToFlakeDirectory}#${config.userDefinedGlobalVariables.hostTag}";
      upgrade = "get_flake_repository_if_does_not_exit; sudo nix flake update ${config.userDefinedGlobalVariables.pathToFlakeDirectory} && update";
      list-generations = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
      cleanup = "sudo nix-collect-garbage --delete-older-than 2d";
      rollback = "sudo nixos-rebuild switch --rollback";
      ls = "${pkgs.eza}/bin/eza --icons --color=always --group-directories-first";
      ll = "${pkgs.eza}/bin/eza -l --icons --color=always --group-directories-first";
      lt = "${pkgs.eza}/bin/eza -aT --icons --color=always --group-directories-first";
      gc89 = "${pkgs.gcc}/bin/gcc -ansi -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
      gc89d = "${pkgs.gcc}/bin/gcc -ansi -pedantic-errors -Wall -Wextra -g";
      gc99 = "${pkgs.gcc}/bin/gcc -std=c99 -pedantic-errors -Wall -Wextra -g  -DNDEBUG -O3";
      gc99d = "${pkgs.gcc}/bin/gcc -std=c99 -pedantic-errors -Wall -Wextra -g";
      gpp17 = "${pkgs.gcc}/bin/g++ -std=c++17 -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
      gpp17d = "${pkgs.gcc}/bin/g++ -std=c++17 -pedantic-errors -Wall -Wextra -g";
      clpp17 = "${pkgs.clang}/bin/clang++ -std=c++17 -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
      clpp17d = "${pkgs.clang}/bin/clang++ -std=c++17 -pedantic-errors -Wall -Wextra -g";
      vlg = "${pkgs.valgrind}/bin/valgrind --leak-check=yes --track-origins=yes";
      grind = "rm callgrind.out.* && ${pkgs.valgrind}/bin/valgrind --tool=callgrind ./a.out &&  callgrind.out.*";
      vi = "nvim";
     };

    initExtra = ''
        export LANG="en_US.UTF-8";
        function tmux-sesssion {
        BUFFER='tmux-sessionizer'
            zle accept-line
        }
        zle -N tmux-sesssion
        bindkey '^f' tmux-sesssion
    '';
    # There are two types of plugins.
    # the one that are part of the shell(like the once bellow),
    # and then one that are part of oh-my-zsh.
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    # This is also a non oh-my-zsh pluging but it doesn't have
    # a clean option(E.g enableFzfTab = true;)
    plugins = [
        {
            name = "fzf-tab";
            src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
        }
    ];

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
        "vi-mode"
      ];
      # Not used because I am using starship prompt.
      #theme = "agnoster";
      #theme = "avit";
   };
  };

}

