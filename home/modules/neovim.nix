{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;
    vimAlias = true;

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

      nmap <silent> <c-k> :wincmd k<CR>
      nmap <silent> <c-j> :wincmd j<CR>
      nmap <silent> <c-h> :wincmd h<CR>
      nmap <silent> <c-l> :wincmd l<CR>
      ";
    };

}
