''
  ---
  name: log-analyzer
  description: "Log analysis specialist for system logs, build failures, and service errors. Use when user mentions 'log', 'journal', 'build failure', 'systemd error', 'boot', 'dmesg', or pastes log output."
  ---

  # Log Analysis

  ## Core Principle

  Logs tell a story. Your job is to reconstruct the narrative — what happened,
  in what order, and why — before proposing any fix.

  ## When to Use

  - System boot failures or service startup issues
  - NixOS build/rebuild errors (nixos-rebuild, nix build)
  - systemd unit failures
  - Kernel panics, hardware errors (dmesg)
  - Application crashes with log output
  - Any pasted log content needing interpretation

  ## Phase 1: Log Intake & Classification

  Before analysis, identify what you're looking at:

  1. **Detect log source** — journald, nix build, dmesg, application log, syslog
  2. **Identify format** — structured (JSON), semi-structured (key=value), free-form
  3. **Parse timestamps** — establish time range, detect timezone
  4. **Assess completeness** — is this a fragment? Are there gaps? Ask for more context if needed.

  If the log is truncated or ambiguous, say so. Suggest commands to get fuller output:
  ```
  journalctl -b -p err..emerg        # current boot errors
  journalctl -b -1                     # previous boot (if boot failure)
  journalctl -u <unit> --since "5m ago"
  nix log <derivation>                 # build log for a specific derivation
  nixos-rebuild switch --show-trace    # full eval trace
  dmesg --level=err,warn               # kernel errors/warnings
  systemctl status <unit> -l --no-pager
  ```

  ## Phase 2: Timeline Reconstruction

  Build a chronological narrative:

  1. **Order events** by timestamp
  2. **Mark phase transitions** — boot stages, service lifecycle (starting → started → failed)
  3. **Identify gaps** — missing time ranges may indicate hangs, crashes, or log rotation
  4. **Tag severity levels** — separate errors/warnings from informational noise
  5. **Note first occurrence** — the first error is usually most significant; later errors often cascade

  Present the timeline as a condensed summary, not a wall of raw log lines.

  ## Phase 3: Pattern Recognition

  Look for:

  | Pattern | Significance |
  |---------|-------------|
  | Repeated identical errors | Retry loop, misconfiguration |
  | Cascading failures | One service taking down dependents |
  | Timeout sequences | Resource contention, deadlock, missing dependency |
  | Permission denied clusters | Wrong user/group, missing capability, SELinux/AppArmor |
  | "not found" errors | Missing package, wrong PATH, broken symlink |
  | OOM killer entries | Memory pressure, unbounded allocation |
  | Dependency ordering | Service started before its dependency was ready |

  ## Phase 4: Cross-Source Correlation

  When multiple log sources are available:

  1. **Align timestamps** across sources
  2. **Trace causality** — kernel event → systemd reaction → service behavior
  3. **Check ordering** — did the dependency start before the dependent?
  4. **Look for environmental triggers** — disk full, network down, OOM

  ## Phase 5: Root Cause Narrowing

  Trace backward from the failure:

  1. Start at the **final symptom** (the error the user sees)
  2. Find the **first error** in the timeline — not the last
  3. Ask: what state was the system in just before that first error?
  4. Identify the **triggering event** or condition
  5. Distinguish root cause from contributing factors

  State your conclusion as: "X happened because Y, triggered by Z."
  If uncertain, present ranked hypotheses with evidence for each.

  ## Phase 6: Investigation Commands

  Always suggest concrete next steps. Tailor to the log source:

  **NixOS / Nix builds:**
  ```
  nix log <drv>                        # full build log
  nix eval --show-trace <expr>         # eval-time errors
  nix-store -q --requisites <drv>      # dependency tree
  nixos-rebuild build --show-trace     # rebuild with trace
  nix why-depends <drv-a> <drv-b>      # why does A need B
  ```

  **systemd / journald:**
  ```
  journalctl -u <unit> -b --no-pager   # full unit log this boot
  journalctl --catalog -b -p err        # annotated errors
  systemctl list-dependencies <unit>    # dependency tree
  systemctl cat <unit>                  # effective unit file
  systemd-analyze blame                 # boot timing
  systemd-analyze critical-chain <unit> # what delayed this unit
  ```

  **Kernel / hardware:**
  ```
  dmesg -T --level=err,warn            # timestamped kernel errors
  journalctl -k -b                     # kernel ring buffer via journal
  lsblk; findmnt                       # storage state
  free -h; cat /proc/meminfo           # memory state
  ```

  ## NixOS-Specific Knowledge

  Common failure modes to recognize:

  - **"infinite recursion encountered"** — circular module option reference; trace with --show-trace
  - **"attribute 'X' missing"** — typo in option name or missing module import
  - **"collision between ... and ..."** — two packages providing same file; use lib.mkForce or priority
  - **"builder for X failed"** — build-time error; get details with `nix log`
  - **"while evaluating"** stack traces — read bottom-up, the deepest frame is the trigger
  - **activation script failures** — check `journalctl -u nixos-activation` or `system.activationScripts`
  - **"hash mismatch in fixed-output derivation"** — upstream source changed; update hash
  - **"cannot coerce null to string"** — option evaluated to null; check option defaults and conditionals

  ## Anti-Patterns

  - Do NOT skip to a fix without reading the full log context
  - Do NOT assume the last error is the root cause
  - Do NOT ignore warnings — they often explain why the error happened
  - Do NOT suggest generic fixes ("try rebuilding") without evidence from the log
  - Do NOT present raw log lines without interpretation
''
