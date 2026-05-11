{ lib, pkgs }:

''
  import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
  import { Type } from "@sinclair/typebox";

  function isRedditUrl(url: string): boolean {
    return /^https?:\/\/(www\.|old\.)?reddit\.com\//.test(url);
  }

  function formatRedditComment(comment: any, depth = 0): string {
    if (!comment || comment.kind !== "t1") return "";
    const data = comment.data;
    const indent = "  ".repeat(depth);
    const score = data.score ?? 0;
    const author = data.author ?? "[deleted]";
    let text = indent + author + " (" + score + " points):\n";
    text += data.body
      .split("\n")
      .map((line: string) => indent + "  " + line)
      .join("\n") + "\n\n";
    if (data.replies && data.replies.data && data.replies.data.children) {
      for (const reply of data.replies.data.children) {
        text += formatRedditComment(reply, depth + 1);
      }
    }
    return text;
  }

  async function fetchReddit(url: string, signal?: AbortSignal): Promise<string> {
    const jsonUrl = url.replace(/\/?$/, ".json");
    const resp = await fetch(jsonUrl, {
      signal,
      headers: { "User-Agent": "pi-fetch-url/1.0" },
    });
    if (!resp.ok) throw new Error("Reddit API returned " + resp.status);
    const json = await resp.json() as any[];
    const post = json[0]?.data?.children?.[0]?.data;
    if (!post) throw new Error("Could not parse Reddit response");
    let output = "# " + post.title + "\n\n";
    if (post.selftext) output += post.selftext + "\n\n";
    output += "---\n\n## Comments\n\n";
    const comments = json[1]?.data?.children ?? [];
    for (const comment of comments) {
      output += formatRedditComment(comment);
    }
    return output;
  }

  export default function (pi: ExtensionAPI) {
    pi.registerTool({
      name: "fetch_url",
      label: "Fetch URL",
      description: "Fetch a web page and return its readable text content. Use when the user provides a URL or asks to read/summarize a web page.",
      promptSnippet: "Fetch a URL and extract readable text content",
      parameters: Type.Object({
        url: Type.String({ description: "URL to fetch" }),
      }),
      async execute(_toolCallId, params, signal) {
        const maxLength = 50000;
        let text: string;

        if (isRedditUrl(params.url)) {
          text = await fetchReddit(params.url, signal);
        } else {
          const result = await pi.exec(
            "${lib.getExe pkgs.nodePackages.readability-cli}",
            ["--quiet", "-p", "text-content", params.url],
            { signal, timeout: 30000 }
          );
          if (result.code !== 0) {
            throw new Error(result.stderr || "Failed to fetch URL");
          }
          text = result.stdout.trim();
        }

        const truncated = text.length > maxLength
          ? text.slice(0, maxLength) + "\n\n[Truncated - content exceeded " + maxLength + " characters]"
          : text;
        return {
          content: [{ type: "text", text: truncated || "No readable content found." }],
          details: {},
        };
      },
    });
  }
''
