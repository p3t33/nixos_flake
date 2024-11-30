{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # editors
    neovim
    # required by neovim/vim for copy/paste
    # to work with system clipboard on x11.
    xclip

    vscode

    # UML
    plantuml
    graphviz
    jdk11

    #version control
    git
    git-crypt
    delta
    lazygit

    # lua
    sumneko-lua-language-server
    lua

    # C/C++
    valgrind
    libsForQt5.kcachegrind # front end for callgrind and cachegrind.
    linuxPackages.perf # profiling tool.
    cppcheck # static analysis tool for c/c++
    gcc
    ccls # LSP for C/CPP
    clang-tools # has clangd as part of it, and clang-format.
    clang
    cmake
    gnumake
    pkg-config # Used to get compile/linking flags(E.g gcc myprogram.c $(pkg-config --cflags --libs libpng) -o myprogram)

    # debug
    gdb
    cgdb
    lldb
    rr # record and replay to be used with gdb.

    # nix devlepoment
    #rnix-lsp

    # python devlepoment
    # ------------------
    #
    # For packages to be able to find each other they must be delared toghter
    # I installed there the bare minimum minimum to be able to use python
    # venv scheme.
    (python3.withPackages (
      p: with p; [
        python3Packages.pip
        python3Packages.virtualenv
      ]
    ))
    pyright # language server.

    # networking
    wireshark

    # shell script static checker
    shellcheck

    # serial
    picocom

    #general
    universal-ctags

    # encryption
    openssl

    # message bus system
    dbus
    hugo

    # nix formator
    nixfmt-rfc-style
  ];
}
