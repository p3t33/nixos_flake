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
        # Keep workspace default explicit so OpenClaw does not rewrite openclaw.json
        # on startup when it auto-detects and sets this value.
        agents.defaults.workspace = "${config.custom.shared.primeUserHomeDirectory}/.openclaw/workspace";

        agents.list = [
          {
            id = "main";
            default = true;
            workspace = "${config.custom.shared.primeUserHomeDirectory}/.openclaw/workspace";
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

        # memory-core selects the memory backend, but semantic indexing/search only
        # becomes operational once an embedding provider is configured. We use
        # Ollama for that on this host fleet.
        #
        # Useful checks:
        # - `openclaw memory status --deep` shows whether the embedding provider,
        #   vector index, and indexed chunk counts are actually healthy.
        # - `openclaw memory index --force` forces a full reindex after changing
        #   providers/models or when the memory index looks stale/dirty.
        #
        # memorySearch controls how the vector index is queried. It does NOT affect:
        #   - what gets stored (that is the session-memory hook's job)
        #   - dreaming (which reads short-term-recall.json directly, before the index)
        #   - what gets promoted to MEMORY.md
        #
        # It affects retrieval only — specifically what active-memory gets back when
        # it runs a semantic search before each reply.
        #
        # Hybrid search combines two signals:
        #   - vector similarity (semantic meaning via nomic-embed-text) weighted at 0.7
        #   - BM25 keyword matching weighted at 0.3
        # BM25 catches what embeddings miss: exact names, version numbers, config keys.
        #
        # MMR (Maximal Marginal Relevance):
        #   Without it: if 5 daily notes mention the same topic, you get 5 near-identical
        #   results back. With it: results are re-ranked for diversity so active-memory
        #   gets broader coverage. lambda controls the trade-off (0=max diversity,
        #   1=max relevance). Does not affect dreaming or storage.
        #
        # Temporal decay:
        #   Without it: a note from 6 months ago scores the same as yesterday's if the
        #   semantic match is equal. With it: older entries are penalised — score halves
        #   every halfLifeDays days. Evergreen files like MEMORY.md are not affected,
        #   only timestamped daily notes. Does not affect dreaming or storage.
        agents.defaults.memorySearch = {
          provider = "ollama";
          query.hybrid = {
            mmr = {
              enabled = true;
              # lambda: 0 = max diversity, 1 = max relevance. Default mid-point is fine
              # to start — revisit if active-memory results feel too samey or too scattered.
            };
            temporalDecay = {
              enabled = true;
              # halfLifeDays: score halves every N days. 30 is the default.
              # Reduce if you want recent notes weighted much more heavily.
              halfLifeDays = 30;
            };
          };
        };

        tools.profile = "coding";
        session.dmScope = "per-channel-peer";

        # Keep the default memory backend explicit so bootstrap behavior does not
        # depend on upstream defaults. This alone does not enable semantic memory
        # search; that also needs an embedding provider via agents.defaults.memorySearch.
        #
        # OpenClaw startup auto-enables the bundled openai plugin for openai-codex models.
        # Keep this explicit so it does not rewrite ~/.openclaw/openclaw.json on restart.
        # Clue source: /tmp/openclaw/openclaw-gateway.log contains
        # "openai-codex/gpt-5.4 model configured, enabled automatically."
        plugins = {
          slots.memory = "memory-core";
          entries.openai.enabled = true;

          # Dreaming runs a scheduled background sweep that scores short-term signals
          # and promotes qualified items into MEMORY.md. Runs daily at 3am by default.
          # Internal scoring phases (light/deep/REM) and thresholds are not configurable.
          entries.memory-core.config.dreaming = {
            enabled = true;
            # Default cron is "0 3 * * *" — keeping it explicit.
            frequency = "0 3 * * *";
          };

          # active-memory runs a blocking sub-agent before each reply to surface
          # relevant memories proactively. Without it, memory-core stores and indexes
          # memories but never injects them into responses unless explicitly asked.
          #
          # Activation requires all of:
          #   - plugin enabled
          #   - current agent listed in `agents`
          #   - session type listed in `allowedChatTypes`
          #   - interactive persistent chat (not headless/one-shot API calls)
          #
          # queryMode options:
          #   "message"  - last user message only; fastest, good for preference recall
          #   "recent"   - last message + short tail; balanced, good for follow-ups
          #   "full"     - full conversation history; best recall, slowest
          #
          # promptStyle options (controls eagerness when deciding to surface memories):
          #   "strict"          - least eager, minimizes unwanted context bleed
          #   "balanced"        - general-purpose default
          #   "contextual"      - prioritizes conversation continuity
          #   "recall-heavy"    - surfaces softer/weaker matches
          #   "precision-heavy" - returns nothing unless match is obvious
          #   "preference-only" - tuned for habits, favorites, recurring personal facts
          #
          # timeoutMs: hard deadline for the sub-agent. Docs recommend 15000ms for
          # "recent" mode. Max is 120000ms. Agent reply is blocked until this resolves
          # or times out, so keep this conservative.
          entries."active-memory" = {
            enabled = true;
            config = {
              agents = [ "main" ];
              # Start with direct only. Add "group" later if useful in Telegram groups.
              allowedChatTypes = [ "direct" ];
              queryMode = "recent";
              promptStyle = "balanced";
              timeoutMs = 15000;
              # Keep logging on initially to observe behavior via openclaw logs.
              # Disable once active-memory feels stable.
              logging = true;
              # persistTranscripts: off by default. Enable only if you want to inspect
              # what the sub-agent saw and decided on disk.
            };
          };
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
