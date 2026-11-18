{ ... }:
{
  flake.modules.nixos.hermes-agent = { config, hostSpecific, inputs, lib, pkgs-unstable, ... }:
{
  options.custom.services.hermes-agent.enable =
    lib.mkEnableOption "Hermes Agent gateway service";

  config = lib.mkIf config.custom.services.hermes-agent.enable {
    services.hermes-agent = {
      enable = true;
      # TODO: Remove this package override once upstream Hermes fixes its
      # patchelf handling and the default package builds with our nixpkgs pin.
      # Hermes patches patchelf 0.15.2 with an incompatible newer fix.
      # patchelfUnstable already includes that fix, so replace the build input.
      package = inputs.hermes-agent.packages.${pkgs-unstable.stdenv.hostPlatform.system}.default.override {
        pmPackages = lib.mapAttrs
          (_: tool: tool.overrideAttrs (old: {
            nativeBuildInputs = map
              (input:
                if lib.getName input == "patchelf"
                then pkgs-unstable.patchelfUnstable
                else input)
              (old.nativeBuildInputs or [ ]);
          }))
          (lib.filterAttrs (_: lib.isDerivation)
            (pkgs-unstable.callPackage (inputs.hermes-agent + "/nix/pm-packages.nix") { }));
      };
      user = hostSpecific.primeUsername;
      group = config.users.users.${hostSpecific.primeUsername}.group;
      createUser = false;

      # OAuth login required per-host: hermes auth add openai-codex
      settings = {
        model = {
          provider = "openai-codex";
          default = config.home-manager.users.${hostSpecific.primeUsername}.custom.programs.pi.defaultModel;
        };
      };

      extraDependencyGroups = [ "messaging" "voice" ];

      environmentFiles = [
        config.sops.secrets."hermes-agent/env".path
      ];

      addToSystemPackages = true;
    };

    sops.secrets."hermes-agent/env" = { };
  };
};
}
