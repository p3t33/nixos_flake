{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # editors
    neovim
    # required by neovim/vim for copy/paste
    # to work with system clipboard on x11.
    xclip

    emacs
    vscode

    # UML
    plantuml
    graphviz
    jdk11

    #version control
    git
    git-crypt
    delta


    # lua
    sumneko-lua-language-server
    lua


    # C/C++
    valgrind
    gcc
    ccls # LSP for C/CPP
    clang-tools # has clangd as part of it
    clang
    cmake
    gnumake

    # nix devlepoment
    rnix-lsp

    # python devlepoment 
    # ------------------
    python39
    nodePackages.pyright # language server.

    # networking
    wireshark

    # shell script static checker
    shellcheck

    # serial
    picocom

    #general
    universal-ctags






  ];
}
