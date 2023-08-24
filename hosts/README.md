This directory includes all the hosts that are declared in the
flake.nix file. Each such host is a high level configuration that
imports into itself all the fractured configuration files that
are shared with other hosts. Each such high level configuration
have some host specific settings as well(E.g Nvidia drivers).
