''
  import {
    CustomEditor,
    type ExtensionAPI,
    type ExtensionContext,
    type KeybindingsManager,
  } from "@earendil-works/pi-coding-agent";
  import type { Component, EditorTheme, TUI } from "@earendil-works/pi-tui";
  import { parseColor, stripTerminalSequences, truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

  const footerForeground = parseColor("#e5e9f0");
  const footerBackground = parseColor("#4c566a");
  const modelBackground = parseColor("#3b4252");
  const meterForeground = parseColor("#81a1c1");
  const meterTrackForeground = modelBackground;

  function fitLine(
    left: string,
    right: string,
    width: number,
    fill: (width: number) => string = (width) => " ".repeat(width),
    ellipsis: string = "...",
  ): string {
    const gap = Math.max(1, width - visibleWidth(left) - visibleWidth(right));
    const line = truncateToWidth(left + fill(gap) + right, width, ellipsis);
    const padding = Math.max(0, width - visibleWidth(line));
    return padding > 0 ? line + fill(padding) : line;
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

  function getFilledWidth(percent: number, barWidth: number): number {
    const filled = Math.round((percent / 100) * barWidth);
    return Math.max(0, Math.min(barWidth, filled));
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
            const footerText = (text: string) => thm.style(text, {
              fg: footerForeground,
              bg: footerBackground,
            });
            const segments: string[] = [];

            // Model
            const modelId = ctx.model?.id ?? "no model";
            segments.push(thm.style("⊕ " + modelId, {
              fg: footerForeground,
              bg: modelBackground,
            }));

            // Tokens
            const usage = ctx.getContextUsage();
            const contextWindow = usage?.contextWindow ?? ctx.model?.contextWindow;
            if (usage && contextWindow) {
              const tokens = usage.tokens === null ? "·" : formatTokens(usage.tokens);
              const tokensStr = tokens + "/" + formatTokens(contextWindow);
              segments.push(footerText(tokensStr));
            }

            // Progress bar
            const percent = usage?.percent ?? null;
            if (percent !== null && Number.isFinite(percent)) {
              const barWidth = 8;
              const filled = getFilledWidth(percent, barWidth);
              const filledPart = "█".repeat(filled);
              const emptyPart = "░".repeat(barWidth - filled);
              const progressStr = footerText("[")
                + thm.style(filledPart, { fg: meterForeground, bg: footerBackground })
                + thm.style(emptyPart, { fg: meterTrackForeground, bg: footerBackground })
                + footerText("] " + Math.round(percent) + "%");
              segments.push(progressStr);
            }

            const quotaStatus = footerData.getExtensionStatuses().get("usage");
            if (quotaStatus) {
              segments.push(footerText(stripTerminalSequences(quotaStatus)));
            }

            // Turn duration
            if (lastTurnDuration !== undefined) {
              segments.push(footerText(formatDuration(lastTurnDuration)));
            }

            // Live elapsed
            if (isWorking) {
              segments.push(footerText("⊙ " + elapsedSeconds + "s"));
            }

            const left = segments.join(footerText(" | "));

            // Right side
            const cwdStr = formatCwd(ctx.cwd);
            const branchSuffix = branch ? " (" + branch + ")" : "";
            const right = footerText(cwdStr + branchSuffix);

            return [
              fitLine(left, right, width, (gap) => footerText(" ".repeat(gap)), footerText("...")),
              "",
            ];
          },
        };
      });
    });
  }
''
