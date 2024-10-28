{ ... }:
{
  # limit the resources that are used during the build to prevent
  # from becoming unresponsive.

  # Dictates how many separate derivations will be built at the same time.
  nix.settings.max-jobs = 2;
  # Suggests how many cores each derivation should use. Similar to make -j
  nix.settings.cores = 2;

}
