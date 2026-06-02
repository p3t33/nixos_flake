''
  import {
    CustomEditor,
    type ExtensionAPI,
    type ExtensionContext,
    type KeybindingsManager,
  } from "@mariozechner/pi-coding-agent";
  import type { Component, EditorTheme, TUI } from "@mariozechner/pi-tui";
  import { truncateToWidth, visibleWidth } from "@mariozechner/pi-tui";

  function fitLine(
    left: string,
    right: string,
    width: number,
    fill: string = " ",
  ): string {
    const gap = Math.max(1, width - visibleWidth(left) - visibleWidth(right));
    return truncateToWidth(left + fill.repeat(gap) + right, width);
  }

  function formatCwd(cwd: string): string {
    const home = process.env.HOME;
    if (home && cwd.startsWith(home)) {
      return "~" + cwd.slice(home.length);
    }
    return cwd;
  }

  function formatTokens(n: number): string {
    if (n < 1000) return String(n);
    if (n < 1000000) return (n / 1000).toFixed(1) + "K";
    return (n / 1000000).toFixed(1) + "M";
  }

  function formatDuration(ms: number): string {
    const seconds = Math.round(ms / 1000);
    if (seconds < 60) return seconds + "s";
    const minutes = Math.floor(seconds / 60);
    const remaining = seconds % 60;
    return minutes + "m" + (remaining > 0 ? remaining + "s" : "");
  }

  function buildProgressBar(percent: number, barWidth: number): string {
    const filled = Math.round((percent / 100) * barWidth);
    const empty = barWidth - filled;
    return "█".repeat(filled) + "░".repeat(empty);
  }

  export default function (pi: ExtensionAPI) {
    let isWorking = false;
    let agentStartTime: number | undefined;
    let lastTurnDuration: number | undefined;
    let elapsedSeconds = 0;
    let elapsedTimer: ReturnType<typeof setInterval> | undefined;
    let activeTui: TUI | undefined;

    const stopElapsedTimer = () => {
      if (elapsedTimer) {
        clearInterval(elapsedTimer);
        elapsedTimer = undefined;
      }
    };

    pi.on("agent_start", () => {
      isWorking = true;
      agentStartTime = Date.now();
      elapsedSeconds = 0;
      stopElapsedTimer();
      elapsedTimer = setInterval(() => {
        elapsedSeconds++;
        activeTui?.requestRender();
      }, 1000);
      activeTui?.requestRender();
    });

    pi.on("agent_end", () => {
      isWorking = false;
      if (agentStartTime) {
        lastTurnDuration = Date.now() - agentStartTime;
      }
      stopElapsedTimer();
      activeTui?.requestRender();
    });

    pi.on("session_shutdown", () => {
      stopElapsedTimer();
      activeTui = undefined;
    });

    pi.on("session_start", (_event, ctx) => {
      ctx.ui.setWorkingVisible(false);

      let branch: string | undefined;

      const refreshBranch = async () => {
        const result = await pi.exec("git", ["branch", "--show-current"], { cwd: ctx.cwd }).catch(() => undefined);
        const stdout = result?.stdout.trim();
        branch = stdout && stdout.length > 0 ? stdout : undefined;
        activeTui?.requestRender();
      };
      void refreshBranch();

      // Custom editor: add ❯ prompt and enforce paddingX
      class StatusBarEditor extends CustomEditor {
        constructor(tui: TUI, theme: EditorTheme, keybindings: KeybindingsManager) {
          super(tui, theme, keybindings, { paddingX: 2 });
          activeTui = tui;
        }

        setPaddingX(_padding: number): void {
          super.setPaddingX(2);
        }

        render(width: number): string[] {
          const lines = super.render(width);
          // Replace left padding space with ❯ prompt on first content line
          if (lines.length >= 3 && lines[1] && lines[1].startsWith(" ")) {
            lines[1] = ctx.ui.theme.fg("accent", "❯") + lines[1].slice(1);
          }
          return lines;
        }
      }

      ctx.ui.setEditorComponent((tui, theme, keybindings) => new StatusBarEditor(tui, theme, keybindings));

      // Custom footer: status bar with model, tokens, progress, timing
      ctx.ui.setFooter((tui, theme, footerData) => {
        const unsub = footerData.onBranchChange(() => {
          void refreshBranch();
        });

        return {
          dispose: unsub,
          invalidate() {},
          render(width: number): string[] {
            const thm = ctx.ui.theme;
            const segments: string[] = [];

            // Model
            const modelId = ctx.model?.id ?? "no model";
            segments.push(thm.fg("accent", "⊕ " + modelId));

            // Tokens
            const usage = ctx.getContextUsage();
            const contextWindow = usage?.contextWindow ?? ctx.model?.contextWindow;
            if (usage && contextWindow) {
              const tokensStr = formatTokens(usage.tokens) + "/" + formatTokens(contextWindow);
              segments.push(thm.fg("muted", tokensStr));
            }

            // Progress bar
            const percent = usage?.percent ?? null;
            if (percent !== null) {
              const barWidth = 8;
              const filled = Math.round((percent / 100) * barWidth);
              const filledPart = "█".repeat(filled);
              const emptyPart = "░".repeat(barWidth - filled);
              const progressStr = "[" + thm.fg("accent", filledPart) + thm.fg("dim", emptyPart) + "] " + Math.round(percent) + "%";
              segments.push(progressStr);
            }

            // Turn duration
            if (lastTurnDuration !== undefined) {
              segments.push(thm.fg("muted", formatDuration(lastTurnDuration)));
            }

            // Live elapsed
            if (isWorking) {
              segments.push(thm.fg("accent", "⊙ " + elapsedSeconds + "s"));
            }

            const left = segments.join(thm.fg("muted", " | "));

            // Right side
            const cwdStr = formatCwd(ctx.cwd);
            const branchSuffix = branch ? " (" + branch + ")" : "";
            const right = thm.fg("dim", cwdStr + branchSuffix);

            return [fitLine(left, right, width)];
          },
        };
      });
    });
  }
''
