The meta.nix was created in order to encapsulate variables with same values
that are being set across multiple files. Some of the values are
evaluated "dynamically" via an if statement based on the value that host
sets for its hostname.
