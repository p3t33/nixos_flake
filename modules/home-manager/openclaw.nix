{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
{
  options.custom.programs.openclaw.enable = lib.mkEnableOption "OpenClaw Home Manager integration";

  config = lib.mkIf config.custom.programs.openclaw.enable {
    sops.secrets."openclaw/telegram_bot_token" = { };
    sops.secrets."openclaw/env_vars" = { };

    home.packages = [ pkgs.whisper-cpp ];

    programs.openclaw = {
      enable = true;

      config = {
        agents.defaults.model.primary = "openai-codex/gpt-5.4";

        agents.list = [
          {
            id = "main";
            default = true;
            workspace = "~/.openclaw/workspace";
          }
        ];

        bindings = [
          {
            agentId = "main";
            match = {
              channel = "telegram";
              accountId = "default";
            };
          }
          {
            agentId = "main";
            match.channel = "whatsapp";
          }
        ];

        gateway = {
          mode = "local";
          # explicitly binds to 127.0.0.1
          bind = "loopback";
          auth = {
            mode = "token";
            token = "\${OPENCLAW_GATEWAY_TOKEN}";
          };
        };

        auth = {
          profiles."openai-codex:default" = {
            provider = "openai-codex";
            mode = "oauth";
          };

          order."openai-codex" = [ "openai-codex:default" ];
        };

        channels.telegram = {
          # OpenClaw startup auto-enables configured Telegram channels even in Nix mode.
          # Keep this explicit so it does not rewrite ~/.openclaw/openclaw.json on restart.
          # Clue source: /tmp/openclaw/openclaw-gateway.log contains
          # "Telegram configured, enabled automatically."
          enabled = true;
          defaultAccount = "default";
          allowFrom = [ "\${TELEGRAM_ALLOW_FROM_1}" ];
          groups = {
            "*" = {
              requireMention = true;
              allowFrom = [ "\${TELEGRAM_ALLOW_FROM_1}" ];
            };
          };
          accounts = {
            default = {
              tokenFile = config.sops.secrets."openclaw/telegram_bot_token".path;
            };
          };
        };

        channels.whatsapp = {
          enabled = true;
          accounts = {
            default = {
              enabled = true;
            };
          };
        };

        tools.media.audio = {
          enabled = true;
          maxBytes = 20971520;
          models = [
            # The manual way to manually download the model(like base) with:
            # whisper-cpp-download-ggml-model base
            # and then provide pull path to it.
            {
              type = "cli";
              command = "${pkgs.whisper-cpp}/bin/whisper-cli";
              args = [
                "-m"
                "${inputs.whisper-model-base.outPath}"
                "-f"
                "{{MediaPath}}"
              ];
              timeoutSeconds = 45;
            }
          ];
        };

        tools.profile = "coding";
        session.dmScope = "per-channel-peer";

        # Keep the default memory backend explicit so bootstrap behavior does not
        # depend on upstream defaults.
        #
        # OpenClaw startup auto-enables the bundled openai plugin for openai-codex models.
        # Keep this explicit so it does not rewrite ~/.openclaw/openclaw.json on restart.
        # Clue source: /tmp/openclaw/openclaw-gateway.log contains
        # "openai-codex/gpt-5.4 model configured, enabled automatically."
        plugins = {
          slots.memory = "memory-core";
          entries.openai.enabled = true;
        };

        hooks = {
          internal = {
            enabled = true;
            entries = {
              "session-memory".enabled = true;
              "command-logger".enabled = true;
            };
          };
        };

        # Leave these off until there is a concrete need:
        # - bootstrap-extra-files: only useful for layered bootstrap files such as
        #   per-project AGENTS.md / TOOLS.md under the workspace.
        # - boot-md: only useful when we intentionally maintain a recurring
        #   BOOT.md startup routine.
      };
    };

    systemd.user.services.openclaw-gateway = {
      # sops-nix.service is the service responsible for user space secrets for
      # home-manager. Without adding it to the list openclaw-gateway.service will
      # fail sometimes on boot due to the secrets that are provided as environment
      # file are not ready yet.
      Unit = {
        Wants = [
          "network-online.target"
          "sops-nix.service"
        ];
        After = [
          "network-online.target"
          # Home Manager secrets are materialized by the user-level sops-nix.service.
          # Order OpenClaw after it so EnvironmentFile exists on boot.
          "sops-nix.service"
        ];
      };

      # openclaw-gateway.service was not starting on system boot
      Install.WantedBy = [ "default.target" ];

      # openssl rand -hex 32
      # note that home-manger syntax for "Service" portion of the
      # systemd unit is a bit different from nixos which uses
      # serviceConfig, E,g: systemd.services.n8n.serviceConfig.
      Service.EnvironmentFile = config.sops.secrets."openclaw/env_vars".path;
    };
  };
}
