{ config, lib, pkgs, ... }:

let
  cfg = config.programs.neovim;
  palette = {
    dark_background = "#13141c";
    bright_foreground = "#c0caf5";
    accent = "#7aa2f7";
    foreground = "#a9b1d6";
    green = "#9ece6a";
    magenta = "#ad8ee6";
    red = "#f7768e";
    yellow = "#e0af68";
  } // (config.custom.shared.omarchyColors or { });
  statusBackground = "#303030";
  statusSectionBackground = "#3a3a3a";
in
{
  config = lib.mkIf cfg.enable {
  programs.neovim.plugins = lib.mkOrder 300 (with pkgs.vimPlugins; [
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
    {
      plugin = nvim-colorizer-lua;
      type = "lua";
      config = ''
        require('colorizer').setup()
      '';
    }

    # Icons
    # -----
    nvim-web-devicons
    {
      plugin = render-markdown-nvim;
      type = "lua";
      config = ''
        require("render-markdown").setup({
          -- Optional: Customize the plugin's settings
          file_types = { "markdown", "vimwiki" },
        })

      '';
    }

    # Status/tabline
    # --------------
    {
      plugin = lualine-nvim;
      type = "lua";
      config = ''
        require('lualine').setup {
            options = {
                icons_enabled = true,
                theme = {
                    normal = {
                        a = { fg = '${palette.dark_background}', bg = '${palette.accent}', gui = 'bold' },
                        b = { fg = '${palette.bright_foreground}', bg = '${statusSectionBackground}' },
                        c = { fg = '${palette.bright_foreground}', bg = '${statusBackground}' },
                    },
                    insert = { a = { fg = '${palette.dark_background}', bg = '${palette.green}', gui = 'bold' } },
                    visual = { a = { fg = '${palette.dark_background}', bg = '${palette.magenta}', gui = 'bold' } },
                    replace = { a = { fg = '${palette.dark_background}', bg = '${palette.red}', gui = 'bold' } },
                    command = { a = { fg = '${palette.dark_background}', bg = '${palette.yellow}', gui = 'bold' } },
                    inactive = {
                        a = { fg = '${palette.foreground}', bg = '${statusBackground}' },
                        b = { fg = '${palette.foreground}', bg = '${statusBackground}' },
                        c = { fg = '${palette.foreground}', bg = '${statusBackground}' },
                    },
                },
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
        require("ibl").setup {
          exclude = {
            filetypes = { "dashboard" },
          },
        }
      '';
    }
    {
      # Modern, fast Lua dashboard
      plugin = dashboard-nvim;
      type = "lua";
      config = ''
        local logo = {
        [[ ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗]],
        [[ ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║]],
        [[ ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║]],
        [[ ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║]],
        [[ ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║]],
        [[ ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝]],
        [[]],
        [[]],
        }

        require('dashboard').setup {
          theme = 'doom',
          config = {
            header = logo,
            center = {
              { action = 'FzfLua files',                 desc = ' Find file',       icon = ' ', key = 'f' },
              { action = 'FzfLua oldfiles',              desc = ' Recent files',    icon = ' ', key = 'r' },
              { action = 'AutoSession search',           desc = ' Sessions',        icon = ' ', key = 's' },
              { action = 'FzfLua live_grep',             desc = ' Find text',       icon = ' ', key = 'g' },
              { action = 'e $MYVIMRC',                   desc = ' Config',          icon = ' ', key = 'c' },
              { action = 'qa',                           desc = ' Quit',            icon = ' ', key = 'q' },
            },
            footer = {}
          }
        }
      '';
    }
    {
      plugin = mini-nvim;
    }
  ]);
  };
}
