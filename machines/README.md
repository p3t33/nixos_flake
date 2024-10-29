This directory includes all the machines that are declared in the
flake.nix file. Each such machines is a high level configuration that
imports into itself all the fractured configuration files that
are shared with other machines. Each such high level configuration
have some machine specific settings as well(E.g Nvidia drivers).
