''
  import { execFile } from "node:child_process";
  import { promisify } from "node:util";
  import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

  const execFileAsync = promisify(execFile);
  const ATTENTION_OPTION = "@pi-attention";

  export type TmuxRunner = (args: string[]) => Promise<void>;

  export function isEligible(mode: string, env: NodeJS.ProcessEnv = process.env): boolean {
    return (
      mode === "tui" &&
      Boolean(env.TMUX && env.TMUX_PANE) &&
      !env.PI_SUBAGENT_DEPTH
    );
  }

  export function createTmuxRunner(env: NodeJS.ProcessEnv = process.env): TmuxRunner {
    const socket = env.TMUX?.split(",", 1)[0];
    const pane = env.TMUX_PANE;
    return async (args) => {
      if (!socket || !pane) throw new Error("Missing tmux context");
      await execFileAsync("tmux", ["-N", "-S", socket, ...args], {
        env,
        timeout: 1500,
        maxBuffer: 16 * 1024,
      });
    };
  }

  export function registerTmuxAttention(
    pi: ExtensionAPI,
    env: NodeJS.ProcessEnv = process.env,
    runTmux: TmuxRunner = createTmuxRunner(env),
  ): void {
    let initialized = false;
    let acceptingEvents = false;
    let work = Promise.resolve();

    const enqueue = (operation: () => Promise<void>) => {
      work = work.then(operation, operation).catch(() => undefined);
    };
    const pane = env.TMUX_PANE;
    const socket = env.TMUX?.split(",", 1)[0];

    pi.on("session_start", async (_event, ctx: ExtensionContext) => {
      if (!isEligible(ctx.mode, env) || initialized) return;
      initialized = true;
      acceptingEvents = true;
      enqueue(async () => {
        await runTmux(["set-option", "-p", "-u", "-t", pane ?? "", ATTENTION_OPTION]);
      });
    });

    const signal = () => {
      if (!initialized || !acceptingEvents || !socket || !pane) return;
      enqueue(async () => {
        if (!acceptingEvents) return;
        const command = `set-option -p -t '${"$"}{pane}' ''${ATTENTION_OPTION} 1`;
        await runTmux([
          "if-shell",
          "-F",
          "-t",
          pane,
          "#{==:#{window_active_clients},0}",
          command,
        ]);
      });
    };

    pi.on("agent_settled", signal);
    const unsubscribeBlocked = pi.events.on("herdr:blocked", (event) => {
      if ((event as { active?: boolean } | undefined)?.active) signal();
    });

    pi.on("session_shutdown", async () => {
      if (!initialized) return;
      acceptingEvents = false;
      unsubscribeBlocked();
      enqueue(async () => {
        if (pane && socket) {
          await runTmux(["set-option", "-p", "-u", "-t", pane, ATTENTION_OPTION]);
        }
      });
      await work;
      initialized = false;
    });
  }

  export default function (pi: ExtensionAPI): void {
    registerTmuxAttention(pi);
  }
''
