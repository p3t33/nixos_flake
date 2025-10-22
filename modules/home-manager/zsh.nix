{ config, pkgs, lib, hostSpecific, ... }:

let
  flakeRepositoryUrl = "https://github.com/${config.customGlobal.githubFlakeRepositoryName}.git";
  pathToFlakeDirectory = "${config.home.homeDirectory}/projects/nixos_flake";
in
{
  config = lib.mkIf config.programs.zsh.enable {
    home.packages = with pkgs; [

      # This are plugins that are installed directly with no
      # plugin manager.
      zsh-syntax-highlighting
      zsh-autosuggestions
      zsh-fzf-tab
      thefuck
      eza
    ];

    programs.zsh = {
      history.ignorePatterns = [
        "ls"
        "cd *"
        "pwd"
        "reboot"
        "history"
      ];

      shellAliases = {
        get_flake_repository_if_does_not_exit = "if [[ ! -d ${pathToFlakeDirectory} ]]; then ${pkgs.git}/bin/git clone ${flakeRepositoryUrl} ${pathToFlakeDirectory}; fi;";
        update = "get_flake_repository_if_does_not_exit; sudo nixos-rebuild switch --flake ${pathToFlakeDirectory}#${hostSpecific.hostName}";
        upgrade = "get_flake_repository_if_does_not_exit; sudo nix flake update --flake ${pathToFlakeDirectory} && update";
        format-nix = "find ${pathToFlakeDirectory} -type f -name \"*.nix\" -print0 | xargs -0 -n1 ${pkgs.nixfmt-rfc-style}";
        list-generations = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
        cleanup = "sudo nix-collect-garbage --delete-older-than 2d";
        rollback = "sudo nixos-rebuild switch --rollback";
        ls = "${pkgs.eza}/bin/eza --icons --color=always --group-directories-first";
        ll = "${pkgs.eza}/bin/eza -l --group --icons --color=always --group-directories-first";
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
        f = "fuck";
        t = "tmux-sessionizer";
      };

      initContent = ''
        function tmux-sesssion {
        BUFFER='tmux-sessionizer'
            zle accept-line
        }
        zle -N tmux-sesssion
        bindkey '^f' tmux-sesssion

        # The default behavior of postponing the initialization of the zsh-vi-mode
        # plugin until the first command line prompt appears is designed to minimize conflicts
        # with other plugins(can be disabled using ZVM_INIT_MODE=sourcing). that might
        # alter keybindings or other settings that zsh-vi-mode relies on. This approach
        # helps ensure that zsh-vi-mode can apply its configurations without being overridden
        # by the subsequent loading of other plugins or scripts. this is also refered as
        # lazy binding.
        #
        # THis means that this plugis initialization is done on first use after all of .zshrc
        # had done loading.
        #
        # This was confilction with my use of atuin for history on C-r.
        # The function bellow makes sure to rebind this key(that vim uses to redo chagge)
        # back to the use of atuin.
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
        function zvm_custom_bindings() {
            bindkey '^R' _atuin_search_widget
        }

        # Ensure custom bindings are applied after zsh-vi-mode initializes
        zvm_after_init_commands+=(zvm_custom_bindings)

        # --- Navi widget setup ---
        autoload -Uz add-zle-hook-widget

        _navi_setup_widget() {
          eval "$(navi widget zsh)"
        }

        if [[ $options[zle] = on ]]; then
          _navi_setup_widget
        else
          add-zle-hook-widget line-init _navi_setup_widget
        fi

        # Ensure Navi keeps Ctrl-G binding after zsh-vi-mode initializes
        function _navi_rebind_ctrl_g() {
          zle -N _navi_widget
          bindkey '^G' _navi_widget
        }
        zvm_after_init_commands+=(_navi_rebind_ctrl_g)
        # --- End Navi setup ---
      '';

      # There are four types of plugins in this config
      # ==============================================
      # And the one that I installed with initExtra, although it can be
      # installled using oh-my-zsh but the documentation suggested using initExtra
      # for nix installtion.
      #
      # the one that are part of the zsh shell and home-manger has "clean"
      # configurations(like the once bellow), for them:
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      #
      # plugins that are installed without  plugin manager:
      # without a "clean" option(E.g enableFzfTab = true;)
      plugins = [
        {
          name = "fzf-tab";
          src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
        }
      ];

      # the one that are part of oh-my-zsh plugin manager:
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
        ];
      };
    };
  };
}
