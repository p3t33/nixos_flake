{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Utils
      # =====
      vim-obsession

      # All the lua functions I don't want to write
      # twice.
      plenary-nvim

      # Code completion
      # ===============



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
      # This is the base for all of the LSP functionality.
      # It can work just fine wihtout nvim-cmp but not the
      # other way around. But if nvim-cmp isn't installed and
      # set the amount of default information will be very
      # minimal.
      {
        plugin = nvim-lspconfig;
        type = "lua";
        config = ''
          require('lspconfig').rust_analyzer.setup{}
          require('lspconfig').rnix.setup{}
          require('lspconfig').sumneko_lua.setup{}
          require('lspconfig').clangd.setup{}
        '';
      }
      
      # linter
      # ------
      tagbar # depends on universal-ctags

      {
        plugin =  nvim-treesitter;
        type = "lua";
        config = ''
          require('nvim-treesitter.configs').setup {
          ensure_installed = { "c", "lua", "cpp" },
          parser_install_dir = "~/.config/nvim/treesitters",

           highlight = {
             enable = true,
             additional_vim_regex_highlighting = false,
           },
          }
        '';
      }
      
      # Look and feel
      # ====================== 

      # Themes
      # ------
      {
          plugin = rose-pine;
          type = "lua";
          config = ''
              require('rose-pine').setup({
                      disable_background = true
                      })

          function ColorMyPencils(color) 
              color = color or "rose-pine"
              vim.cmd.colorscheme(color)

              vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
              vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

              end

              ColorMyPencils()
          '';
      }
      dracula-vim
      nord-vim
      gruvbox
      {
        plugin = vim-fugitive;
        type = "lua";
        config = ''
          vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
        '';
      }

      {
        plugin = vim-startify;
        config = ''
          let g:startify_session_persistence = 1 
          let g:startify_custom_header = [
          \' ____     ___   ___   __ __  ____  ___ ___ ',
          \'|    \   /  _] /   \ |  |  ||    ||   |   |',
          \'|  _  | /  [_ |     ||  |  | |  | | _   _ |',
          \'|  |  ||    _]|  O  ||  |  | |  | |  \_/  |',
          \'|  |  ||   [_ |     ||  :  | |  | |   |   |',
          \'|  |  ||     ||     | \   /  |  | |   |   |',
          \'|__|__||_____| \___/   \_/  |____||___|___|',
          \] 
          


          let g:startify_lists = [
             \ { 'type': 'sessions',  'header': ['   Sessions']       },
             \ { 'type': 'files',     'header': ['   Recent files']            },
             \ { 'type': 'dir',       'header': ['   CWD '. getcwd()] },
             \]

        '';
      }

      # status/tabline 
      # --------------
      vim-airline

      # Icons
      # -----
      vim-devicons

      #pear-tree
      delimitMate

      # Navigation
      # ==========
      fzf-vim
      nerdtree

      {
        plugin = undotree;
        type = "lua";
        config = ''
          vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
        '';
      }

      {
        plugin = harpoon;
        type = "lua";
        config = ''
          vim.g.mapleader = " "
          local mark = require("harpoon.mark")
          local ui = require("harpoon.ui")

          vim.keymap.set("n", "<leader>a", mark.add_file)
          vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu)

          vim.keymap.set("n", "<C-h>", function() ui.nav_file(1) end)
          vim.keymap.set("n", "<C-t>", function() ui.nav_file(2) end)
          vim.keymap.set("n", "<C-n>", function() ui.nav_file(3) end)
          vim.keymap.set("n", "<C-s>", function() ui.nav_file(4) end)
        '';
      }
    ];

    extraConfig = ''
      "path for treesitter"
      "-------------------"
      lua << EOF
      local parser_install_dir = vim.fn.stdpath("cache") .. "~/treesitters"
      vim.fn.mkdir(parser_install_dir, "p")
      vim.opt.runtimepath:append("~/.config/nvim/treesitters")
      EOF

      " Editor setup"
      " ============"
      let mapleader = " "

      colorscheme rose-pine
      
      "line numbers and relative lines"
      set number
      set relativenumber

      "Indentation settings of 4 spaces"
      set tabstop=4
      set softtabstop=4
      set shiftwidth=4
      set expandtab

      set smartindent

      "Disable wrap of text"
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

      nnoremap <leader>t :NERDTree<CR>
      nnoremap <C-n> :NERDTree<CR>
      nnoremap <C-t> :NERDTreeToggle<CR>

      nnoremap <C-p> :Files<Cr>
      nnoremap <C-f> :Rg<Cr>
      nnoremap <C-b> :Buffers<Cr>Cr

      nmap<F8> :TagbarToggle<CR>
      map <F5> :setlocal spell! spellsuggest=best,5 spelllang=en_us<CR>

      noremap <Up> <Nop>
      noremap <Down> <Nop>
      noremap <Left> <Nop>
      noremap <Right> <Nop>

      nnoremap <C-h> <C-w>h
      nnoremap <C-j> <C-w>j
      nnoremap <C-k> <C-w>k
      nnoremap <C-l> <C-w>l


      nmap <silent> <c-k> :wincmd k<CR>
      nmap <silent> <c-j> :wincmd j<CR>
      nmap <silent> <c-h> :wincmd h<CR>
      nmap <silent> <c-l> :wincmd l<CR>
      '';
    };

}
