{ pkgs, ... }:
{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = rainbow-delimiters-nvim;
      type = "lua";
      config = ''

        vim.g.rainbow_delimiters = {
            highlight = {
                'RainbowDelimiterYellow',
                'RainbowDelimiterRed',
                'RainbowDelimiterBlue',
                'RainbowDelimiterOrange',
                'RainbowDelimiterGreen',
                'RainbowDelimiterViolet',
                'RainbowDelimiterCyan',
            },
        }
      '';
    }

    # Theme
    vim-code-dark

    # Adds actual color to hex value.
    colorizer

    # Icons
    # -----
    vim-devicons
    nvim-web-devicons

    # Status/tabline
    # --------------
    {
      plugin = lualine-nvim;
      type = "lua";
      config = ''
        require('lualine').setup {
            options = {
                icons_enabled = true,
                theme = 'nord',
                component_separators = { left = '', right = ''},
                section_separators = { left = '', right = ''},
                disabled_filetypes = {
                    statusline = {},
                    winbar = {},
                },
                ignore_focus = {},
                always_divide_middle = true,
                globalstatus = false,
                refresh = {
                    statusline = 1000,
                    tabline = 1000,
                    winbar = 1000,
                }
            },
            sections = {
                lualine_a = {'mode'},
                lualine_b = {'branch', 'diff', 'diagnostics'},
                lualine_c = {'filename'},
                lualine_x = {'filetype'},
                lualine_y = {'progress'},
                lualine_z = {'location'}
            },
            inactive_sections = {
                lualine_a = {},
                lualine_b = {},
                lualine_c = {'filename'},
                lualine_x = {'location'},
                lualine_y = {},
                lualine_z = {}
            },
            tabline = {},
            winbar = {},
            inactive_winbar = {},
            extensions = {}
        }
      '';
    }

    # creates indent lines
    {
      plugin = indent-blankline-nvim;
      type = "lua";
      config = ''
        require("ibl").setup()
      '';
    }
    {
      # Manages the start screen for vim and also sessions
      plugin = vim-startify;
      config = ''
        let g:startify_session_persistence = 1
        let g:startify_session_autoload = 1
        let g:startify_custom_header = [
        \' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
        \' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
        \' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
        \' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
        \' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
        \' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
        \]

        let g:startify_lists = [
           \ { 'type': 'sessions',  'header': ['   Sessions']       },
           \ { 'type': 'files',     'header': ['   Recent files']            },
           \ { 'type': 'dir',       'header': ['   CWD '. getcwd()] },
           \]
      '';
    }
  ];
}
