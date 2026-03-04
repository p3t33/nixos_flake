{ config, lib, pkgs, ... }:

let
  cfg = config.programs.neovim;
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
    {
      plugin = markdown-nvim;
      type = "lua";
      config = ''
        require('markdown').setup({})

      '';

    }
  ]);
  };
}
