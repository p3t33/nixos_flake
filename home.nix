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
  home.sessionVariables = {
    EDITOR = "nvim";
    SUDO_EDITOR = "nvim";
  };

  home.packages = with pkgs; [
    zsh-syntax-highlighting
    zsh-autosuggestions
    thefuck
  ];

  programs.ssh = {
    enable = true;
  };

  programs.starship = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      settings = {

        add_newline = false;
        username = {
          style_user = "green bold";
          style_root = "red bold";
          format = "[$user]($style)";
          disabled = false;
          show_always = true;
        };

        hostname = {
          ssh_only = false;
          format = "@[$hostname](bold yellow):";
          trim_at = ".";
          disabled = false;
        };
        character = {
          success_symbol = "[❯](bold green)";
          error_symbol = "[✗](bold red)";
        };

        directory = {
          read_only = " ";
          truncation_length = 10;
          truncate_to_repo = true;
          style = "bold italic blue";
        };

        cmd_duration = {
          min_time = 2;
          show_milliseconds = false;
          disabled = false;
          style = "bold italic red";
        };

        aws = {
          symbol = "  ";
        };

        git_branch = {
          symbol = " ";
        };

        golang = {
          symbol = " ";
        };

        rust = {
          symbol = " ";
        };

        c = {
          symbol = " ";
        };

        nix_shell = {
          symbol = " ";
        };

        package = {
          symbol = " ";

        };

        python = {
          symbol = " ";
        };


      };
    };

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

    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;

      # CTRL-T
      fileWidgetCommand = "fd --hidden";

      # ALT-C
      changeDirWidgetCommand = "fd --hidden --type d";

      # Default command that is executed for fzf
      defaultCommand = "fd --hidden --type f";
    };

    programs.git = {
      enable = true;
      delta = {
        enable = true;
        options = {
          navigate = true;
          line-numbers = true;
          side-by-side = true;
          theme = "Nord";
        };
      };

      userName = "Kobi Medrish";
      userEmail = "kobi.medrish@motorolasolutions.com";
    };

        programs.neovim = {
      enable = true;
      vimAlias = true;
      extraConfig = "
        colorscheme nord
        set number
        set relativenumber

        set shiftwidth=4
        set mouse=a

        nnoremap <C-n> :NERDTree<CR>
        nnoremap <C-t> :NERDTreeToggle<CR>

        nnoremap <C-p> :Files<Cr>
        nnoremap <C-f> :Rg<Cr>
        nnoremap <C-b> :Buffers<Cr>Cr

        nmap<F8> :TagbarToggle<CR>

        noremap <Up> <Nop>
        noremap <Down> <Nop>
        noremap <Left> <Nop>
        noremap <Right> <Nop>

        nnoremap <C-h> <C-w>h
        nnoremap <C-j> <C-w>j
        nnoremap <C-k> <C-w>k
        nnoremap <C-l> <C-w>l

        nnoremap <C-u> <C-u>zz
        nnoremap <C-d> <C-d>zz

      ";
      plugins = with pkgs.vimPlugins; [
        vim-nix
        #gruvbox
        nord-vim
        vim-airline
        fzf-vim
        nerdtree
        tagbar
        vim-devicons
        #pear-tree
        delimitMate
      ];
    };


}
