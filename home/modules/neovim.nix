{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [

      # Utils
      # =====
      {
        plugin = gitsigns-nvim;
        type = "lua";
        config = ''
            require('gitsigns').setup()
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
          plugin = indent-blankline-nvim;
      }
      {
          # vim-tmux-navigator plugin has a dual function(although name implies only
          # vim integration )
          #
          # This plugin adds smart movments between vim windows and tmux panes, By using
          # Ctrl+h/j/k/l you will be able to move across tmux pane into pane with
          # vim inside it and then move inside the vim windows and back seamlessly
          # For this integration to work counterpart plugin needs to be added to tmux.
          #
          # If the conterpart isn't installed the only functionality that will be added
          # is the aability to move between windows using Ctr+h/j/k/l in vim.
          plugin = vim-tmux-navigator;
      }

      {
          # A tree file explore.

          # Without the require'nvim-tree'.setup {} this plugin doesn't work
          # the commands won't be recognised.
          plugin = nvim-tree-lua;
          type = "lua";
          config = ''
          require'nvim-tree'.setup {}
          '';
      }

      # Adds actual color to hex value for that represtn colors.
      colorizer

      # This is a vim session manager that I currently do not use because I am
      # using X
      vim-obsession

      # This is a dependencie for many other plugins and could be considered as
      # a "library"
      plenary-nvim

      {
          # a vim bookmarks manager
          plugin = vim-bookmarks;
          config = ''
            let g:bookmark_save_per_working_dir = 1
            let g:bookmark_no_default_key_mappings = 1
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

      # LSP completion related capabilities
      # -----------------------------------
      # This plugin is responsible for code completion,
      # code snippets(for function), and errors with explanations.
      # This pluging depends on nvim-lspconfig plugin to have minimal
      # functionality. It needs bunch of other plugins(added right below)
      # to function properly.
      {
        plugin = nvim-cmp;
        type = "lua";
        config = ''
        vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

        require('luasnip.loaders.from_vscode').lazy_load()

        local cmp = require('cmp')
        local luasnip = require('luasnip')

        local select_opts = {behavior = cmp.SelectBehavior.Select}

        cmp.setup({
          snippet = {
            expand = function(args)
            luasnip.lsp_expand(args.body)
            end
          },
          sources = {
            {name = 'path'},
            {name = 'nvim_lsp', keyword_length = 3},
            {name = 'buffer', keyword_length = 3},
            {name = 'luasnip', keyword_length = 2},
          },
          window = {
            documentation = cmp.config.window.bordered()
          },
          formatting = {
            fields = {'menu', 'abbr', 'kind'},
            format = function(entry, item)
            local menu_icon = {
              nvim_lsp = 'λ',
              luasnip = '⋗',
              buffer = 'Ω',
              path = '🖫',
            }

            item.menu = menu_icon[entry.source.name]
            return item
            end,
          },
          mapping = {
            ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
            ['<Down>'] = cmp.mapping.select_next_item(select_opts),

            ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
            ['<C-n>'] = cmp.mapping.select_next_item(select_opts),

            ['<C-u>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),

            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({select = false}),

            ['<C-d>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(1) then
            luasnip.jump(1)
            else
            fallback()
            end
            end, {'i', 's'}),

            ['<C-b>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
            luasnip.jump(-1)
            else
            fallback()
            end
            end, {'i', 's'}),

            ['<Tab>'] = cmp.mapping(function(fallback)
            local col = vim.fn.col('.') - 1

            if cmp.visible() then
            cmp.select_next_item(select_opts)
            elseif col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
            fallback()
            else
            cmp.complete()
            end
            end, {'i', 's'}),

            ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
            cmp.select_prev_item(select_opts)
            else
            fallback()
            end
            end, {'i', 's'}),
          },
        })
        '';
      }

      # nvim-cmp dependencies
      # ---------------------
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline

      # users of luasnip
      luasnip
      cmp_luasnip

      # This one wasn't on the list of nvim-cmp but in a reference
      # and is used in require('luasnip.loaders.from_vscode').lazy_load().
      friendly-snippets

      # LSP clients
      # -----------

      {
        # This plugin is the base for all of the LSP functionality.
        # It can work just fine wihtout nvim-cmp but not the
        # other way around. But if nvim-cmp isn't installed and
        # set the amount of default information will be very
        # minimal.
        plugin = nvim-lspconfig;
        type = "lua";
        config = ''
            local opts = { noremap=true, silent=true }

            -- Use an on_attach function to only map the following keys
            -- after the language server attaches to the current buffer
            local on_attach = function(client, bufnr)

            -- Enable completion triggered by <c-x><c-o>
            vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

            -- Mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            local bufopts = { noremap=true, silent=true, buffer=bufnr }

            -- go to definition.
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
            vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, bufopts)
            vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float, bufopts)
            vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
            vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
            -- clang-tidy code actions
            vim.keymap.set("n", "<leader>vca", vim.lsp.buf.code_action, bufopts)
            vim.keymap.set("n", "<leader>vrr", vim.lsp.buf.references, bufopts)
            vim.keymap.set("n", "<leader>vrn", vim.lsp.buf.rename, bufopts)
            vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, bufopts)

            -- Can be used to replace plugin trouble-nvim(and dependencie nvim-web-devicons)
            vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)

            -- clang format.
            vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
            end

            local lsp_flags = {
                -- This is the default in Nvim 0.7+
                    debounce_text_changes = 150,
            }
          require('lspconfig').rust_analyzer.setup{}
          require('lspconfig').rnix.setup{}
          require('lspconfig').lua_ls.setup{}
          require('lspconfig').clangd.setup{
              on_attach = on_attach,
              flags = lsp_flags,
          }

          require('lspconfig').pyright.setup{
              on_attach = on_attach,
              flags = lsp_flags,
          }
        '';
      }

      # linter
      # ------

      # List all the tag objects in a file
      # Depends on universal-ctags
      tagbar

      # Tree-sitter is a parsing library for programming languages that
      # can be used to analyze, edit, and transform code.
      {
        plugin =  nvim-treesitter;
        type = "lua";
        config = ''
          require('nvim-treesitter.configs').setup {
          ensure_installed = { "c", "lua", "cpp", "nix", "python", "yaml", "cmake", "go", "json", "make", "markdown" },
          parser_install_dir = "~/.config/nvim/treesitters",

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
                      -- You can use the capture groups defined in textobjects.scm
                          ["af"] = "@function.outer",
                      ["if"] = "@function.inner",
                      ["ac"] = "@class.outer",
                      -- You can optionally set descriptions to the mappings (used in the desc parameter of
                              -- nvim_buf_set_keymap) which plugins like which-key display
                          ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
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
             additional_vim_regex_highlighting = false,
           },
          }
        '';
      }

      # Dependeed on nvim-treesitter
      nvim-treesitter-textobjects

      # Look and feel
      # ======================

      # Themes
      # ------
      vim-code-dark
      dracula-vim
      nord-vim
      gruvbox

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

      # Status/tabline
      # --------------
      {
          plugin = vim-airline;
          config = ''

              " Show just the filename
              let g:airline#extensions#tabline#fnamemod = ':t'
          '';
      }

      # Icons
      # -----
      vim-devicons
      # Required by trouble-nvim
      nvim-web-devicons



      # Navigation
      # ==========
      {
          # fuzzy finder for files/string/open buffers...
          plugin = fzf-vim;
          config = ''

              "FZF Buffer Delete

              function! s:list_buffers()
              redir => list
              silent ls
              redir END
              return split(list, "\n")
              endfunction

              function! s:delete_buffers(lines)
              execute 'bwipeout' join(map(a:lines, {_, line -> split(line)[0]}))
              endfunction

              command! BD call fzf#run(fzf#wrap({
                          \ 'source': s:list_buffers(),
                          \ 'sink*': { lines -> s:delete_buffers(lines) },
                          \ 'options': '--multi --reverse --bind ctrl-a:select-all+accept'
                          \ }))
          '';
      }

      {
        # Undotree visualizes the undo history and makes it easy to
        # browse and switch between different undo branches.
        plugin = undotree;
        type = "lua";
        config = ''
          vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
        '';
      }

      {
        # specify marks on the file and then manage them.
        plugin = harpoon;
        type = "lua";
        config = ''
          vim.g.mapleader = " "
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

    extraConfig = ''
      " path for treesitter"
      " -------------------"
      lua << EOF
      local parser_install_dir = vim.fn.stdpath("cache") .. "~/treesitters"
      vim.fn.mkdir(parser_install_dir, "p")
      vim.opt.runtimepath:append("~/.config/nvim/treesitters")
      EOF

      " Editor setup"
      " ============"
      let mapleader = " "

      "No timeout for leader key"
      set notimeout nottimeout


      colorscheme codedark

      "line numbers and relative lines"
      set number
      set relativenumber

      "Indentation settings of 4 spaces"
      set tabstop=4
      set softtabstop=4
      set shiftwidth=4
      set expandtab

      " Use file type based indentation
      filetype plugin indent on

      " Use cindent for C/C++
      autocmd FileType c,cc,cpp setlocal cindent

      :set cursorline

      " Disable wrap of text"
      " --------------------"
      set nowrap

      "Setting up backup of files"
      set noswapfile
      set nobackup
      " Let's save undo info!
      if !isdirectory($HOME."/.vim")
          call mkdir($HOME."/.vim", "", 0770)
      endif
      if !isdirectory($HOME."/.vim/undo-dir")
          call mkdir($HOME."/.vim/undo-dir", "", 0700)
      endif

      set undodir=~/.vim/undo-dir
      set undofile

      "Search related"
      set incsearch

      "set better colorscheme"
      set termguicolors

      "Scroll"
      set scrolloff=8

      "A column that is always displayed"
      "A plugin is used to populate it with data"
      " E.g erros, git information"
      set signcolumn=yes

      set updatetime=50

      "Creates a ruler with 80 character wide"
      set colorcolumn =80

      set mouse=a


      "Remove white spaces at the end of the line on buffer write"
      fun! TrimWhitespace()
          let l:save = winsaveview()
          keeppatterns %s/\s\+$//e
          call winrestview(l:save)
      endfun
      autocmd BufWritePre * call TrimWhitespace()

      "Mappings"
      "========"

      "drag up and down selected lines in visual line mode"
      vnoremap J :m '>+1<CR>gv=gv
      vnoremap K :m '<-2<CR>gv=gv

      "up and down the page and stay in the middle"
      nnoremap <C-u> <C-u>zz
      nnoremap <C-d> <C-d>zz

      "while in search screen stays in the middle"
      nnoremap n nzzzv
      nnoremap N Nzzzv

      "pase over and the deleted text will go into void regester"
      xnoremap <leader>p "_dP

      "deleting into void regester"
      nnoremap <leader>d "_d
      vnoremap <leader>d "_d

      "yanking into clipboard"
      nnoremap <leader>y "+y
      vnoremap <leader>y "+y
      nnoremap <leader>Y "+Y

      "column mode edit exis"
      inoremap <C-c> <Esc>

      "replace the word you are on"
      nnoremap <leader>s :%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>

      "make file executalbe"
      nnoremap <silent> <leader>x <cmd>!chmod +x %<CR>

      nnoremap <leader>. :NvimTreeToggle<Cr>

      "fzf"
      nnoremap <leader>ff :Files<Cr>
      nnoremap <leader>fs :Rg<Cr>
      nnoremap <leader>fb :Buffers<Cr>
      nnoremap <leader>fd :BD<Cr>

      "use of Ctrl-q to save current buffer in normal and insert mode."
      nnoremap <c-s> :w<CR>
      inoremap <c-s> <Esc>:w<CR>

      "used for saving and exiting file - useful for git commits"
      nnoremap <c-x> :wq<CR>
      inoremap <c-x> <Esc>:wq<CR>


      "Generate ctags"
      nnoremap <leader>gt :!ctags -R --exclude=.git<Cr><Cr>

      "bookmarks"
      nnoremap <leader>mm :BookmarkToggle<Cr>
      nnoremap <leader>mi :BookmarkAnnotate<Cr>
      nnoremap <leader>mn :BookmarkNext<Cr>
      nnoremap <leader>mp :BookmarkPrev<Cr>

      nnoremap <leader>ma :BookmarkShowAll<Cr>

      nnoremap <leader>mc :BookmarkClear<Cr>
      nnoremap <leader>mx :BookmarkClearAll<Cr>


      "make file executalbe"
      nnoremap <leader>fx :!chmod +x %<Cr><Cr>

      "boffer control"
      nnoremap <leader>bn :bn<Cr>
      nnoremap <leader>bp :bp<Cr>
      nnoremap <leader>bd :bd<Cr>
      nnoremap <leader>bs <C-w>s
      nnoremap <leader>bv <C-w>v

      nmap<F8> :TagbarToggle<CR>

      "spell checker"
      "------------"
      map <F5> :setlocal spell! spellsuggest=best,5 spelllang=en_us<CR>
      "will activate spell checker automatically once trying to create a new git commit"
      autocmd FileType gitcommit,markdown setlocal spell spellsuggest=best,5 spelllang=en_us

      noremap <leader>xd :TroubleToggle<CR>

      noremap <Up> <Nop>
      noremap <Down> <Nop>
      noremap <Left> <Nop>
      noremap <Right> <Nop>
      '';
    };

}
