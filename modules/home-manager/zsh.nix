{ config, pkgs, lib, hostSpecific, ... }:

let
  flakeRepositoryUrl = "https://github.com/${config.custom.shared.githubFlakeRepositoryName}.git";
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
      eza
    ];

    programs.zsh = {
      history.share = true;
      history.ignorePatterns = [
        "ls"
        "cd *"
        "pwd"
        "reboot"
        "history"
      ];

      shellAliases = {
        get_flake_repository_if_does_not_exit = "if [[ ! -d ${pathToFlakeDirectory} ]]; then ${lib.getExe pkgs.git} clone ${flakeRepositoryUrl} ${pathToFlakeDirectory}; fi;";
        update = "get_flake_repository_if_does_not_exit; sudo nixos-rebuild switch --flake ${pathToFlakeDirectory}#${hostSpecific.hostName}";
        upgrade = "get_flake_repository_if_does_not_exit; sudo nix flake update --flake ${pathToFlakeDirectory} && update";
        format-nix = "find ${pathToFlakeDirectory} -type f -name \"*.nix\" -print0 | xargs -0 -n1 ${pkgs.nixfmt}";
        list-generations = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
        cleanup = "sudo nix-collect-garbage --delete-older-than 2d";
        rollback = "sudo nixos-rebuild switch --rollback";
        ls = "${lib.getExe pkgs.eza} --icons --color=always --group-directories-first";
        ll = "${lib.getExe pkgs.eza} -l --group --icons --color=always --group-directories-first";
        lt = "${lib.getExe pkgs.eza} -aT --icons --color=always --group-directories-first";
        gc89 = "${lib.getExe pkgs.gcc} -ansi -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
        gc89d = "${lib.getExe pkgs.gcc} -ansi -pedantic-errors -Wall -Wextra -g";
        gc99 = "${lib.getExe pkgs.gcc} -std=c99 -pedantic-errors -Wall -Wextra -g  -DNDEBUG -O3";
        gc99d = "${lib.getExe pkgs.gcc} -std=c99 -pedantic-errors -Wall -Wextra -g";
        gpp17 = "${lib.getExe' pkgs.gcc "g++"} -std=c++17 -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
        gpp17d = "${lib.getExe' pkgs.gcc "g++"} -std=c++17 -pedantic-errors -Wall -Wextra -g";
        clpp17 = "${lib.getExe' pkgs.clang "clang++"} -std=c++17 -pedantic-errors -Wall -Wextra -DNDEBUG -O3";
        clpp17d = "${lib.getExe' pkgs.clang "clang++"} -std=c++17 -pedantic-errors -Wall -Wextra -g";
        vlg = "${lib.getExe' pkgs.valgrind "valgrind"} --leak-check=yes --track-origins=yes";
        grind = "rm callgrind.out.* && ${lib.getExe' pkgs.valgrind "valgrind"} --tool=callgrind ./a.out &&  callgrind.out.*";
        vi = "nvim";
        t = "tmux-sessionizer";
      };

      initContent = ''
        # Disable Ctrl+S terminal freeze (vestigial serial flow control)
        stty -ixon

        function tmux-sessionizer-widget {
            BUFFER='tmux-sessionizer'
            zle accept-line
        }
        zle -N tmux-sessionizer-widget

        # zsh-vi-mode uses lazy initialization (runs after .zshrc finishes loading)
        # which causes it to overwrite any keybindings set before it. The function
        # below re-applies our custom bindings after zsh-vi-mode initializes:
        #  - ^R: atuin history search (vi-mode rebinds this to redo)
        #  - ^S: tmux-sessionizer (keyboard tmux layer misfire sends Ctrl+S)
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
        function zvm_custom_bindings() {
            bindkey '^R' _atuin_search_widget
            bindkey '^S' tmux-sessionizer-widget
            # Atuin's init prepends itself to ZSH_AUTOSUGGEST_STRATEGY. Reset it here
            # so autosuggestions use zsh's native history (local machine only) instead
            # of Atuin's cross-machine synced database.
            ZSH_AUTOSUGGEST_STRATEGY=(history)
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
