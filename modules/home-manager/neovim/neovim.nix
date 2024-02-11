{ pkgs, config, ... }:

# Understanding how the configuration gets generated.

# All vim script from plugins settings and from extraConfig is generated into a single
# file which get called at the top of init.lua that is generated from all plugins settings
# that is written in lua and from extraLuaConfig. The order in which the plugins defined
# is also reflected in both lua and vim script configuration files.

{
  imports = [
    ./spelling.nix
    ./ui.nix
    ./completion.nix
  ];

  programs.neovim = {
    enable = true;
    vimAlias = true;

    # Dependency management
    extraPackages = with pkgs; [
        # required by neovim/vim for copy/paste
        # to work with system clipboard on x11.
        xclip
    ];

    plugins = with pkgs.vimPlugins; [

      # Utils
      # =====
      {
        plugin = gitsigns-nvim;
        type = "lua";
        config = ''
            require('gitsigns').setup {
                on_attach = function(bufnr)
                local gs = package.loaded.gitsigns
                    vim.keymap.set('n', "<leader>gd", gs.reset_hunk)
                    vim.keymap.set('v', "<leader>gd", function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
                    vim.keymap.set('n', "<leader>gb", gs.toggle_current_line_blame)
                    vim.keymap.set('n', "]g", function()
                            if vim.wo.diff then return ']c' end
                            vim.schedule(function() gs.next_hunk() end)
                            return '<Ignore>'
                            end, {expr=true})
                    vim.keymap.set('n', "[g", function()
                            if vim.wo.diff then return '[c' end
                            vim.schedule(function() gs.prev_hunk() end)
                            return '<Ignore>'
                            end, {expr=true})
            end
            }
        '';
      }
      {

         plugin = fzf-lua;
         type = "lua";
         config = ''
             require('fzf-lua').setup({
                'default', -- the profile to be used.
                winopts = {
                    height = 0.90,  -- Adjust as needed
                    width = 0.90,   -- Adjust as needed
                    row = 0.50,     -- Center vertically
                    col = 0.50,     -- Center horizontally

                    preview = {
                        layout = "vertical",
                        vertical = "up:70%",
                        default="builtin"
                      },
                },

                fzf_opts = {
                ['--layout'] = 'reverse-list',
                },
             })

             vim.keymap.set('n', '<leader>ff', ':FzfLua files<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fb', ':FzfLua buffers<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fs', ':FzfLua live_grep<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fr', ':FzfLua resume<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fw', ':FzfLua grep_cword<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fW', ':FzfLua grep_cWORD<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fg', ':FzfLua git_bcommits<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fv', ':FzfLua grep_visual<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fc', ':FzfLua git_status<CR>', { noremap = true, silent = true })
             vim.keymap.set('n', '<leader>fl', ':FzfLua lsp_finder<CR>', { noremap = true, silent = true })
         '';
      }
      {
          plugin = nvim-autopairs;
          type = "lua";
          config = ''
          require("nvim-autopairs").setup {}
          '';
      }
      {
          plugin = comment-nvim;
          type = "lua";
          config = ''
            require('Comment').setup ({
                toggler = {
                    line = '<leader><tab><tab>'
                },

                opleader = {
                    line = '<leader><tab><tab>'
                },

                extra = {
                    ---Add comment on the line above
                        above = '<leader><tab>O',
                    ---Add comment on the line below
                        below = '<leader><tab>o',
                    ---Add comment at the end of line
                        eol = '<leader><tab>A',
                },
            })
          '';
      }
      {
          # vim-tmux-navigator plugin has a dual function(although name implies only
          # vim integration )
          #
          # This plugin adds smart movements between vim windows and tmux panes, By using
          # Ctrl+h/j/k/l you will be able to move across tmux pane into pane with
          # vim inside it and then move inside the vim windows and back seamlessly
          # For this integration to work counterpart plugin needs to be added to tmux.
          #
          # If the counterpart isn't installed the only functionality that will be added
          # is the ability to move between windows using Ctr+h/j/k/l in vim.
          plugin = vim-tmux-navigator;
      }

      {
          # A tree file explore.

          # Without the require 'nvim-tree'.setup {} this plugin doesn't work
          # the commands won't be recognized.
          plugin = nvim-tree-lua;
          type = "lua";
          config = ''
          require'nvim-tree'.setup {}
          '';
      }


      # This is a vim session manager that I currently do not use because I am
      # using startify
      vim-obsession

      # This is a dependence for many other plugins and could be considered as
      # a "library"
      plenary-nvim

      {
          # a vim bookmarks manager
          plugin = vim-bookmarks;
          config = ''
            let g:bookmark_save_per_working_dir = 1
            let g:bookmark_no_default_key_mappings = 1

            "I need to define leader key here because the order in which "
            "nix generates the config"
            let mapleader = " "
            nnoremap <leader>mm :BookmarkToggle<CR>
            nnoremap <leader>mi :BookmarkAnnotate<CR>
            nnoremap <leader>ma :BookmarkShowAll<CR>
            nnoremap <leader>md :BookmarkClearAll<CR>
            nnoremap <leader>mn :BookmarkNext<CR>
            nnoremap <leader>mp :BookmarkPrev<CR>
          '';
      }

      {
        # A git wrapper for vim
           plugin = vim-fugitive;
           type = "lua";
           config = ''
               vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
          '';
      }


      # Code completion and LSP
      # =======================

      # Automatic closing of quotes, parenthesis, brackets...
      delimitMate

      # A pretty list for showing diagnostics, references and fixes.
      trouble-nvim
      telescope-vim-bookmarks-nvim
      telescope-ui-select-nvim

      {
        plugin = telescope-nvim;
        type = "lua";
        config = ''
            local actions = require("telescope.actions")
            local trouble = require("trouble.providers.telescope")
            require('telescope').setup {
                defaults = {
                    mappings = {
                        i = {
                            ['<C-u>'] = false,
                            ['<C-d>'] = false,
                            ["<c-t>"] = trouble.open_with_trouble,
                        },
                        n = {
                            ["<c-t>"] = trouble.open_with_trouble,
                        },
                    },
                    dynamic_preview_title = true,
                    wrap_results = true,

                    layout_config = {
                        prompt_position = "top",
                        horizontal = {
                            preview_width = 0.65,
                            width = 0.75,
                        },
                        vertical = {
                            width = 0.9,  -- Adjusts the width of the Telescope window as a percentage of the total screen width
                            height = 0.95,  -- Adjusts the height as a percentage of the total screen height
                            mirror = false,  -- If true, flips the layout vertically (preview at bottom)
                            preview_height = 0.6,
                            preview_cutoff = 20,
                            prompt_position = "bottom",
                        },
                        center = {
                            mirror = false,
                            height = 0.40,  -- Height of the Telescope window as a percentage of the total screen height
                            width = 0.50,  -- Width of the Telescope window as a percentage of the total screen width
                            preview_cutoff = 40,  -- Minimum width of the Telescope window to show the preview pane
                        },
                        cursor = {
                            width = 0.80,  -- Width of the Telescope window as a percentage of the total screen width
                            height = 0.40,  -- Height of the Telescope window as a percentage of the total screen height
                            preview_cutoff = 40,  -- Minimum width of the Telescope window to show the preview pane
                        },
                    },

                },
                pickers = {
                    find_files = {
                        layout_strategy = 'vertical',
                    },
                    live_grep = {
                        layout_strategy = 'vertical',
                    },
                    grep_string = {
                        layout_strategy = 'vertical',
                    },
                    buffers = {
                        layout_strategy = 'vertical',
                    },
                    help_tags = {
                        layout_strategy = 'vertical',
                    },
                    git_status = {
                        layout_strategy = 'vertical',
                    },
                },
                extensions = {
                    -- telescope-fzf-native-nvim pluginreplaces native lua search
                    fzf = {
                        fuzzy = true,                    -- false will only do exact matching
                        override_generic_sorter = true,  -- override the generic sorter
                        override_file_sorter = true,     -- override the file sorter
                        case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                    },
                    ["ui-select"] = {
                        require("telescope.themes").get_cursor(),
                    },
                }
            }

            -- for the use of telescope-fzf-native.nvim
            require('telescope').load_extension('fzf')
            -- for the use of telescope-vim-bookmarks-nvim
            require('telescope').load_extension('vim_bookmarks')
            require("telescope").load_extension("ui-select")

            local builtin = require('telescope.builtin')
            vim.keymap.set('n', '<leader>xf', builtin.find_files, {})
            vim.keymap.set('n', '<leader>xs', builtin.live_grep, {})
            vim.keymap.set('n', '<leader>xw', builtin.grep_string, {})
            vim.keymap.set('n', '<leader>xb', builtin.buffers, {})
            vim.keymap.set('n', '<leader>xh', builtin.help_tags, {})
            vim.keymap.set('n', '<leader>xc', builtin.git_status, {})
            vim.keymap.set('n', '<leader>xr', builtin.resume, {})
            vim.keymap.set('n', '<leader>xm', function()
                require('telescope').extensions.vim_bookmarks.all()
            end, {desc = 'Telescope: All Vim Bookmarks'})

            vim.keymap.set('n', '<leader>ka', function()
                    require("telescope.builtin").find_files({
                        results_title = "Config Files Results",
                        layout_strategy = "horizontal",
                        layout_config = {
                        preview_width = 0.65,
                        width = 0.75,
                        },
                    })
            end, {desc = "Find Config Files"})
        '';
      }
      {
        plugin = telescope-fzf-native-nvim;
        type = "lua";
        config = ''
        '';
      }

      # LSP clients
      # -----------


      # linter
      # ------

      # List all the tag objects in a file
      # Depends on universal-ctags
      tagbar

      # Tree-sitter is a parsing library for programming languages that
      # can be used to analyze, edit, and transform code.
      # This setting replaced:
      #
      # "ensure_installed = { "c", "cpp"..}" that was part
      # of the config = ''... and made vim install treesitter
      # packages dynamically instaed of using the declarative
      # nature of nix.
      #
      # By using withAllGrammars I am installing all of the packages
      # that can are avaliable to treesitter. This single setting
      # replaced.
      # plugin = (nvim-treesitter.withPlugins (p: [
      #     p.tree-sitter-nix
      #     p.tree-sitter-bash
      #     ....
      #     ]));

      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = ''
          require('nvim-treesitter.configs').setup {
          -- depending on nvim-treesitter-textobjects pluging
          textobjects = {
              lsp_interop = {
                  enable = true,
                  border = 'none',
                  floating_preview_opts = {},
                  peek_definition_code = {
                      ["<leader>df"] = "@function.outer",
                      ["<leader>dF"] = "@class.outer",
                  },
              },

              select = {
                  enable = true,

                  -- Automatically jump forward to textobj, similar to targets.vim
                      lookahead = true,

                  keymaps = {
                      ['aa'] = '@parameter.outer',
                      ['ia'] = '@parameter.inner',
                      ['af'] = '@function.outer',
                      ['if'] = '@function.inner',
                      ['ac'] = '@class.outer',
                      ['ic'] = '@class.inner',
                      ['ii'] = '@conditional.inner',
                      ['ai'] = '@conditional.outer',
                      ['il'] = '@loop.inner',
                      ['al'] = '@loop.outer',
                      ['at'] = '@comment.outer',
                  },
                  -- You can choose the select mode (default is charwise 'v')
                      --
                      -- Can also be a function which gets passed a table with the keys
                      -- * query_string: eg '@function.inner'
                      -- * method: eg 'v' or 'o'
                      -- and should return the mode ('v', 'V', or '<c-v>') or a table
                      -- mapping query_strings to modes.
                      selection_modes = {
                          ['@parameter.outer'] = 'v', -- charwise
                              ['@function.outer'] = 'V', -- linewise
                                  ['@class.outer'] = '<c-v>', -- blockwise
                      },
                  -- If you set this to `true` (default is `false`) then any textobject is
                      -- extended to include preceding or succeeding whitespace. Succeeding
                      -- whitespace has priority in order to act similarly to eg the built-in
                      -- `ap`.
                      --
                      -- Can also be a function which gets passed a table with the keys
                      -- * query_string: eg '@function.inner'
                      -- * selection_mode: eg 'v'
                      -- and should return true of false
                      include_surrounding_whitespace = true,
              },


          },

           highlight = {
             enable = true,
             additional_vim_regex_highlighting = {'org'},
           },
          }
        '';
      }

      # Depended on nvim-treesitter
      # and provides syntax aware text-objects, select, move, swap, and peek support.
      nvim-treesitter-textobjects

      # org-mode support
      {
          plugin = orgmode;
          type = "lua";
          config = ''
              -- Load custom treesitter grammar for org filetype
              require('orgmode').setup_ts_grammar()

              -- Orgmode setup
              require('orgmode').setup({
                      org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
                      org_default_notes_file = '~/Dropbox/org/refile.org',
                      });
          '';
      }

      # Look and feel
      # ======================

      # Navigation
      # ==========
      {
        # Undotree visualizes the undo history and makes it easy to
        # browse and switch between different undo branches.
        plugin = undotree;
        config = ''
            "I need to define leader key here because the order in which "
            "nix generates the config"
            let mapleader = " "
            nnoremap <leader>u :UndotreeToggle<CR>

            if has("persistent_undo")
                " Let's save undo info!
                if !isdirectory($HOME."/.vim")
                    call mkdir($HOME."/.vim", "", 0770)
                endif

                set undodir=~/.vim/undo-dir
                set undofile
            endif
            "---------------------------"
        '';
      }

      {
        # specify marks on the file and then manage them.
        plugin = harpoon;
        type = "lua";
        config = ''
          local mark = require("harpoon.mark")
          local ui = require("harpoon.ui")

          vim.keymap.set("n", "<leader>a", mark.add_file)
          vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu)

          vim.keymap.set("n", "<leader>1", function() ui.nav_file(1) end)
          vim.keymap.set("n", "<leader>2", function() ui.nav_file(2) end)
          vim.keymap.set("n", "<leader>3", function() ui.nav_file(3) end)
          vim.keymap.set("n", "<leader>4", function() ui.nav_file(4) end)
        '';
      }
    ];

    extraLuaConfig = ''
        -- ============
        -- Editor setup
        -- ============
        vim.g.mapleader = " "

        -- No timeout for leader key
        vim.o.timeout = false
        vim.o.ttimeout = false

        -- Colorscheme
        vim.cmd("colorscheme codedark")

        -- Line numbers and relative lines
        vim.wo.number = true
        vim.wo.relativenumber = true

        -- Indentation settings of 4 spaces
        vim.o.tabstop = 4
        vim.o.softtabstop = 4
        vim.o.shiftwidth = 4
        vim.o.expandtab = true

        -- Use file type based indentation
        vim.cmd("filetype plugin indent on")

        -- Use cindent for C/C++
        vim.api.nvim_create_autocmd("FileType", {
                pattern = {"c", "cpp"},
                callback = function()
                vim.bo.cindent = true
                end,
                })

        -- Set cursorline
        vim.wo.cursorline = true

        -- Disable wrap of text
        vim.wo.wrap = false

        -- incremental search, tarts searching as you type, immediately jumps to the closest match to the text you've entered so far
        vim.o.incsearch = true

        -- Enable true color support in the terminal
        vim.o.termguicolors = true

        -- set the number of lines to keep above and below the cursor when scrolling through a document
        vim.o.scrolloff = 8

        -- A column that is always displayed
        -- A plugin is used to populate it with data
        -- E.g erros, git information
        vim.o.signcolumn = "yes"

        -- Set update time for various features like diagnostics to appear
        vim.o.updatetime = 50

        -- Creates a ruler at 80 characters width
        vim.wo.colorcolumn = "80"

        -- Enable mouse support in all modes
        vim.o.mouse = "a"

        -- ------swap/backup file disable---
        -- As I am using undotree I don't need this file
        vim.o.swapfile = false
        vim.o.backup = false
        -- ---------------------------------

       -- ================================
       -- Costum functions
       -- ================================

       -- ----------------------------------------------------------
       -- Remove white spaces at the end of the line on buffer write
       -- ----------------------------------------------------------
       -- Define the Lua function to trim whitespace
       local function trim_whitespace()
            local save = vim.fn.winsaveview()
            vim.api.nvim_exec('%s/\\s\\+$//e', false)
            vim.fn.winrestview(save)
       end

       -- Set up an autocmd to call the Lua function before buffer write
       vim.api.nvim_create_autocmd("BufWritePre", {
               pattern = "*",
               callback = trim_whitespace,
       })

       -- ----------------------------------------------------------
       -- spelling and costum dictrionary use
       -- ----------------------------------------------------------
       -- Define the custom command for nixen spell file compilation
       vim.api.nvim_create_user_command('MakeNixSpell', function()
               vim.cmd('mkspell! ~/.config/nvim/spell/nixen.utf-8.spl ~/.config/nvim/spell/nixen.utf-8.add')
       end, {})

       local function compile_nix_spell_file_if_needed()
           local spellfile = vim.fn.expand("~/.config/nvim/spell/nixen.utf-8.spl")
           local addfile = vim.fn.expand("~/.config/nvim/spell/nixen.utf-8.add")

           if not vim.fn.filereadable(addfile) then
               return
           end

           local spell_timestamp = vim.fn.system('stat -c %Y ' .. vim.fn.shellescape(spellfile))
           local add_timestamp = vim.fn.system('stat -c %Y ' .. vim.fn.shellescape(addfile))

           if not vim.fn.filereadable(spellfile) or add_timestamp > spell_timestamp then
               vim.cmd('silent! MakeNixSpell')
           end
       end

       -- Autocommand to call the function at Vim start
       vim.api.nvim_create_autocmd("VimEnter", {
            pattern = "*",
            callback = compile_nix_spell_file_if_needed,
       })

       -- Spell checker key mapping and autocommands
       vim.api.nvim_set_keymap('n', '<F5>', ":setlocal spell! spellsuggest=best,5 spelllang=en_us,nixen<CR>", { noremap = true, silent = true })

       vim.api.nvim_create_autocmd("FileType", {
            pattern = "gitcommit,markdown",
            command = "setlocal spell spellsuggest=best,5 spelllang=en_us,nixen",
       })
       -- ------------------------------------

       -- ================================

       -- ================================
       -- Key Mappings
       -- ================================
       -- General options for mappings
       local opts = { noremap = true, silent = true }

      -- Drag up and down selected lines in visual line mode
      vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", opts)
      vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", opts)

      -- Centered movements
      -- ------------------
      vim.keymap.set('n', '<C-u>', "<C-u>zz", opts)
      vim.keymap.set('n', '<C-d>', "<C-d>zz", opts)
      vim.keymap.set('n', 'n', "nzzzv", opts)
      vim.keymap.set('n', 'N', "Nzzzv", opts)
      vim.keymap.set('n', '{', '{zz', opts)
      vim.keymap.set('n', '}', '}zz', opts)
      vim.keymap.set('n', '<C-i>', '<C-i>zz', opts)
      vim.keymap.set('n', '<C-o>', '<C-o>zz', opts)
      vim.keymap.set('n', '%', '%zz', opts)
      vim.keymap.set('n', '*', '*zz', opts)
      vim.keymap.set('n', '#', '#zz', opts)

     -- Paste over and the deleted text will go into void register
     vim.keymap.set('x', '<leader>p', '"_dP', opts)

     -- Deleting into void register
     vim.keymap.set('n', '<leader>d', '"_d', opts)
     vim.keymap.set('v', '<leader>d', '"_d', opts)

     -- Yanking into clipboard
     vim.keymap.set('n', '<leader>y', '"+y', opts)
     vim.keymap.set('v', '<leader>y', '"+y', opts)
     vim.keymap.set('n', '<leader>Y', '"+Y', opts)

     -- Column mode edit exit
     vim.keymap.set('i', '<C-c>', '<Esc>', opts)

     -- Replace the word you are on
     vim.keymap.set('n', '<leader>s', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>", opts)

     -- Make file executable
     vim.keymap.set('n', '<leader>x', "<cmd>!chmod +x %<CR>", opts)

     -- NvimTree toggle
     vim.keymap.set('n', '<leader>.', ":NvimTreeToggle<Cr>", opts)


     -- Save current buffer
     vim.keymap.set('n', '<C-s>', ":w<CR>", opts)
     vim.keymap.set('i', '<C-s>', "<Esc>:w<CR>", opts)

     -- Save and exit file
     vim.keymap.set('n', '<C-x>', ":wq<CR>", opts)
     vim.keymap.set('i', '<C-x>', "<Esc>:wq<CR>", opts)

     -- Generate ctags
     vim.keymap.set('n', '<leader>gt', ":!ctags -R --exclude=.git<Cr><Cr>", opts)

     -- Bookmarks (Add your bookmark functions in a similar way)

     -- Buffer control
     vim.keymap.set('n', '<leader>bn', ":bn<Cr>", opts)
     vim.keymap.set('n', '<leader>bp', ":bp<Cr>", opts)
     vim.keymap.set('n', '<leader>bd', ":bd<Cr>", opts)
     vim.keymap.set('n', '<leader>bs', "<C-w>s", opts)
     vim.keymap.set('n', '<leader>bv', "<C-w>v", opts)

     -- Tagbar toggle
     vim.keymap.set('n', '<F8>', ":TagbarToggle<CR>", opts)

     -- Trouble toggle
     vim.keymap.set('n', '<leader>xd', ":TroubleToggle<CR>", opts)

     -- Disable arrow keys
     vim.keymap.set('n', '<Up>', "<Nop>", opts)
     vim.keymap.set('n', '<Down>', "<Nop>", opts)
     vim.keymap.set('n', '<Left>', "<Nop>", opts)
     vim.keymap.set('n', '<Right>', "<Nop>", opts)

     -- Show current buffer absolute path and copy it
     vim.keymap.set('n', '<F6>', function() vim.fn.setreg('+', vim.fn.expand('%:p')) print(vim.fn.getreg('+')) end, opts)

     -- Apply clang-format
     vim.keymap.set('n', '<leader>cf', ":%!clang-format<CR>", opts)

     -- Jump to the next misspelled word and activate fix mode
     vim.keymap.set('n', ']s', ']sz=', opts)

     -- Jump to the previous misspelled word and activate fix mode
     vim.keymap.set('n', '[s', '[sz=', opts)
    '';

    # this is generated into vim script and called at the top of init.lua
    extraConfig = ''
      cmap w!! w !sudo tee > /dev/null %


      '';
    };
}
