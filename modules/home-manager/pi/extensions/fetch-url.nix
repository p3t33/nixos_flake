{ lib, pkgs }:

''
  import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
  import { Type } from "@sinclair/typebox";
  import { existsSync, readdirSync, readFileSync, statSync, mkdirSync, openSync, readSync, closeSync } from "node:fs";
  import { join, extname, resolve as resolvePath, sep as pathSep } from "node:path";

  const GITHUB_CLONE_ROOT = "/tmp/pi-github-repos";
  const MAX_TREE_ENTRIES = 200;
  const MAX_INLINE_FILE_CHARS = 100000;
  const CONCURRENT_LIMIT = 3;
  const JINA_READER_BASE = "https://r.jina.ai/";
  const JINA_TIMEOUT_MS = 30000;
  const MIN_USEFUL_CONTENT = 200;

  const BINARY_CONTENT_TYPES = [
    "image/", "audio/", "video/",
    "application/zip", "application/gzip", "application/x-tar",
    "application/octet-stream", "application/pdf",
    "application/x-7z-compressed", "application/x-rar-compressed",
    "font/", "application/font",
  ];

  const BINARY_EXTENSIONS = new Set([
    ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".ico", ".webp", ".svg", ".tiff",
    ".mp3", ".mp4", ".avi", ".mov", ".mkv", ".wav", ".ogg", ".webm", ".flac",
    ".zip", ".tar", ".gz", ".bz2", ".xz", ".7z", ".rar", ".zst",
    ".exe", ".dll", ".so", ".dylib", ".bin", ".o", ".a",
    ".woff", ".woff2", ".ttf", ".otf",
    ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx",
    ".sqlite", ".db", ".pyc", ".class", ".jar",
    ".iso", ".img", ".dmg",
  ]);

  const NOISE_DIRS = new Set([
    "node_modules", "vendor", ".next", "dist", "build", "__pycache__",
    ".venv", "venv", ".tox", ".mypy_cache", ".pytest_cache",
    "target", ".gradle", ".idea", ".vscode",
  ]);

  const NON_CODE_SEGMENTS = new Set([
    "issues", "pull", "pulls", "discussions", "releases", "wiki",
    "actions", "settings", "security", "projects", "compare",
    "commits", "tags", "branches", "stargazers", "watchers",
    "network", "forks", "labels", "packages",
  ]);

  interface GitHubUrlInfo {
    owner: string;
    repo: string;
    ref?: string;
    path?: string;
    type: "root" | "blob" | "tree";
  }

  function parseGitHubUrl(url: string): GitHubUrlInfo | null {
    let parsed: URL;
    try { parsed = new URL(url); } catch { return null; }

    const host = parsed.hostname.toLowerCase();
    if (host !== "github.com" && host !== "www.github.com") return null;

    const segments = parsed.pathname.split("/").filter(Boolean).map(s => {
      try { return decodeURIComponent(s); } catch { return s; }
    });
    if (segments.length < 2) return null;

    const owner = segments[0];
    const repo = segments[1].replace(/\.git$/, "");

    if (segments.length >= 3 && NON_CODE_SEGMENTS.has(segments[2].toLowerCase())) return null;

    if (segments.length === 2) return { owner, repo, type: "root" };

    const action = segments[2];
    if (action !== "blob" && action !== "tree") return null;
    if (segments.length < 4) return null;

    const ref = segments[3];
    const path = segments.slice(4).join("/") || "";
    return { owner, repo, ref, path, type: action as "blob" | "tree" };
  }

  function isBinaryFile(filePath: string): boolean {
    if (BINARY_EXTENSIONS.has(extname(filePath).toLowerCase())) return true;
    let fd: number;
    try { fd = openSync(filePath, "r"); } catch { return false; }
    try {
      const buf = Buffer.alloc(512);
      const n = readSync(fd, buf, 0, 512, 0);
      for (let i = 0; i < n; i++) { if (buf[i] === 0) return true; }
      return false;
    } catch { return false; } finally { closeSync(fd); }
  }

  function formatSize(bytes: number): string {
    if (bytes < 1024) return bytes + " B";
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + " KB";
    return (bytes / (1024 * 1024)).toFixed(1) + " MB";
  }

  function resolveInRepo(root: string, rel: string): string | null {
    const resolved = resolvePath(root, rel);
    const prefix = root.endsWith(pathSep) ? root : root + pathSep;
    if (resolved !== root && !resolved.startsWith(prefix)) return null;
    return resolved;
  }

  function buildTree(root: string): string {
    const entries: string[] = [];
    function walk(dir: string, rel: string): void {
      if (entries.length >= MAX_TREE_ENTRIES) return;
      let items: string[];
      try { items = readdirSync(dir).sort(); } catch { return; }
      for (const item of items) {
        if (entries.length >= MAX_TREE_ENTRIES) return;
        if (item === ".git") continue;
        const itemRel = rel ? rel + "/" + item : item;
        const full = resolveInRepo(root, itemRel);
        if (!full) continue;
        let stat;
        try { stat = statSync(full); } catch { continue; }
        if (stat.isDirectory()) {
          if (NOISE_DIRS.has(item)) { entries.push(itemRel + "/  [skipped]"); continue; }
          entries.push(itemRel + "/");
          walk(full, itemRel);
        } else {
          entries.push(itemRel);
        }
      }
    }
    walk(root, "");
    if (entries.length >= MAX_TREE_ENTRIES) entries.push("... (truncated at " + MAX_TREE_ENTRIES + " entries)");
    return entries.join("\n");
  }

  function buildDirListing(root: string, sub: string): string {
    const target = resolveInRepo(root, sub);
    if (!target) return "(path escapes repository root)";
    let items: string[];
    try { items = readdirSync(target).sort(); } catch { return "(directory not readable)"; }
    const lines: string[] = [];
    for (const item of items) {
      if (item === ".git") continue;
      const full = resolveInRepo(root, sub ? sub + "/" + item : item);
      if (!full) { lines.push("  " + item + "  (outside repo)"); continue; }
      try {
        const stat = statSync(full);
        lines.push(stat.isDirectory() ? "  " + item + "/" : "  " + item + "  (" + formatSize(stat.size) + ")");
      } catch { lines.push("  " + item + "  (unreadable)"); }
    }
    return lines.join("\n");
  }

  function readReadme(root: string): string | null {
    for (const name of ["README.md", "readme.md", "README", "README.txt", "README.rst"]) {
      const p = join(root, name);
      if (!existsSync(p)) continue;
      try {
        const content = readFileSync(p, "utf-8");
        return content.length > 8192 ? content.slice(0, 8192) + "\n\n[README truncated at 8K chars]" : content;
      } catch { continue; }
    }
    return null;
  }

  function generateGitHubContent(localPath: string, info: GitHubUrlInfo): string {
    const lines: string[] = ["Repository cloned to: " + localPath, ""];

    if (info.type === "root") {
      lines.push("## Structure", buildTree(localPath), "");
      const readme = readReadme(localPath);
      if (readme) lines.push("## README.md", readme, "");
      lines.push("Use \`read\` and \`bash\` tools at the path above to explore further.");
      return lines.join("\n");
    }

    if (info.type === "tree") {
      const dirPath = info.path || "";
      const full = resolveInRepo(localPath, dirPath);
      if (!full || !existsSync(full)) {
        lines.push("Path \`" + dirPath + "\` not found. Showing root instead.", "", "## Structure", buildTree(localPath));
      } else {
        lines.push("## " + (dirPath || "/"), buildDirListing(localPath, dirPath));
      }
      lines.push("", "Use \`read\` and \`bash\` tools at the path above to explore further.");
      return lines.join("\n");
    }

    if (info.type === "blob") {
      const filePath = info.path || "";
      const full = resolveInRepo(localPath, filePath);
      if (!full || !existsSync(full)) {
        lines.push("Path \`" + filePath + "\` not found. Showing root instead.", "", "## Structure", buildTree(localPath));
        lines.push("", "Use \`read\` and \`bash\` tools at the path above to explore further.");
        return lines.join("\n");
      }
      let stat;
      try { stat = statSync(full); } catch (e: any) {
        lines.push("Could not inspect \`" + filePath + "\`: " + e.message);
        return lines.join("\n");
      }
      if (stat.isDirectory()) {
        lines.push("## " + filePath, buildDirListing(localPath, filePath));
        lines.push("", "Use \`read\` and \`bash\` tools at the path above to explore further.");
        return lines.join("\n");
      }
      if (isBinaryFile(full)) {
        lines.push("## " + filePath, "Binary file (" + extname(filePath).slice(1) + ", " + formatSize(stat.size) + "). Use \`read\` or \`bash\` to inspect.");
        return lines.join("\n");
      }
      try {
        const content = readFileSync(full, "utf-8");
        lines.push("## " + filePath);
        lines.push(content.length > MAX_INLINE_FILE_CHARS
          ? content.slice(0, MAX_INLINE_FILE_CHARS) + "\n\n[Truncated at 100K chars. Full file: " + full + "]"
          : content);
      } catch {
        lines.push("Could not read \`" + filePath + "\` as UTF-8.");
      }
      lines.push("", "Use \`read\` and \`bash\` tools at the path above to explore further.");
      return lines.join("\n");
    }

    return lines.join("\n");
  }

  async function cloneGitHub(pi: ExtensionAPI, info: GitHubUrlInfo, signal?: AbortSignal): Promise<string | null> {
    const localPath = join(GITHUB_CLONE_ROOT, info.owner, info.ref ? info.repo + "@" + info.ref : info.repo);

    if (existsSync(join(localPath, ".git"))) return localPath;

    mkdirSync(join(GITHUB_CLONE_ROOT, info.owner), { recursive: true });

    const ghCheck = await pi.exec("which", ["gh"], { timeout: 5000 });
    const hasGh = ghCheck.code === 0;

    let args: string[];
    if (hasGh) {
      args = ["gh", "repo", "clone", info.owner + "/" + info.repo, localPath, "--", "--depth", "1", "--single-branch"];
      if (info.ref) args.push("--branch", info.ref);
    } else {
      const gitUrl = "https://github.com/" + info.owner + "/" + info.repo + ".git";
      args = ["${lib.getExe pkgs.git}", "clone", "--depth", "1", "--single-branch"];
      if (info.ref) args.push("--branch", info.ref);
      args.push(gitUrl, localPath);
    }

    const result = await pi.exec(args[0], args.slice(1), { signal, timeout: 30000 });
    if (result.code !== 0) return null;
    return localPath;
  }

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

  function isBinaryContentType(contentType: string): boolean {
    const ct = contentType.toLowerCase().split(";")[0].trim();
    return BINARY_CONTENT_TYPES.some(prefix => ct.startsWith(prefix));
  }

  async function checkContentType(url: string, signal?: AbortSignal): Promise<{ ok: true } | { ok: false; error: string }> {
    try {
      const resp = await fetch(url, {
        method: "HEAD",
        signal: AbortSignal.any([
          AbortSignal.timeout(10000),
          ...(signal ? [signal] : []),
        ]),
        headers: {
          "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
        },
      });
      const ct = resp.headers.get("content-type") || "";
      if (isBinaryContentType(ct)) {
        return { ok: false, error: "Unsupported content type: " + ct.split(";")[0].trim() + ". This is a binary file, not a web page." };
      }
      return { ok: true };
    } catch {
      // HEAD failed — proceed anyway, let readability-cli handle it
      return { ok: true };
    }
  }

  function isLikelyJSRendered(html: string): boolean {
    const bodyMatch = html.match(/<body[^>]*>([\s\S]*?)<\/body>/i);
    if (!bodyMatch) return false;
    const bodyHtml = bodyMatch[1];
    const textContent = bodyHtml
      .replace(/<script[\s\S]*?<\/script>/gi, "")
      .replace(/<style[\s\S]*?<\/style>/gi, "")
      .replace(/<[^>]+>/g, "")
      .replace(/\s+/g, " ")
      .trim();
    const scriptCount = (html.match(/<script/gi) || []).length;
    return textContent.length < 500 && scriptCount > 3;
  }

  async function fetchWithJinaReader(url: string, signal?: AbortSignal): Promise<string | null> {
    try {
      const resp = await fetch(JINA_READER_BASE + url, {
        signal: AbortSignal.any([
          AbortSignal.timeout(JINA_TIMEOUT_MS),
          ...(signal ? [signal] : []),
        ]),
        headers: {
          "Accept": "text/markdown",
          "X-No-Cache": "true",
        },
      });
      if (!resp.ok) return null;
      const content = await resp.text();
      const markdownStart = content.indexOf("Markdown Content:");
      const markdown = markdownStart >= 0 ? content.slice(markdownStart + 17).trim() : content.trim();
      if (markdown.length < 100 ||
          markdown.startsWith("Loading...") ||
          markdown.startsWith("Please enable JavaScript")) {
        return null;
      }
      return markdown;
    } catch {
      return null;
    }
  }

  async function detectJSRendered(url: string, signal?: AbortSignal): Promise<string | null> {
    try {
      const resp = await fetch(url, {
        signal: AbortSignal.any([
          AbortSignal.timeout(15000),
          ...(signal ? [signal] : []),
        ]),
        headers: {
          "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
          "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        },
      });
      if (!resp.ok) return null;
      const html = await resp.text();
      if (isLikelyJSRendered(html)) {
        return "Page appears to be JavaScript-rendered (content loads dynamically). Readability and Jina Reader both failed to extract content.";
      }
      return null;
    } catch {
      return null;
    }
  }

  async function fetchGenericUrl(pi: ExtensionAPI, url: string, signal?: AbortSignal): Promise<string> {
    // 1. Content-type pre-check
    const ctCheck = await checkContentType(url, signal);
    if (!ctCheck.ok) throw new Error(ctCheck.error);

    // 2. Try readability-cli
    const result = await pi.exec(
      "${lib.getExe pkgs.nodePackages.readability-cli}",
      ["--quiet", "-p", "text-content", url],
      { signal, timeout: 30000 }
    );
    if (result.code === 0 && result.stdout.trim().length >= MIN_USEFUL_CONTENT) {
      return result.stdout.trim();
    }

    // 3. Fallback: Jina Reader
    const jinaContent = await fetchWithJinaReader(url, signal);
    if (jinaContent && jinaContent.length >= MIN_USEFUL_CONTENT) {
      return jinaContent;
    }

    // 4. Detect JS-rendered page for better error message
    const jsHint = await detectJSRendered(url, signal);
    if (jsHint) throw new Error(jsHint);

    // If readability got something short, return it with a warning
    if (result.code === 0 && result.stdout.trim().length > 0) {
      return result.stdout.trim() + "\n\n[Warning: extracted content appears incomplete]";
    }
    if (jinaContent && jinaContent.length > 0) {
      return jinaContent + "\n\n[Warning: extracted content appears incomplete]";
    }

    throw new Error(result.stderr || "Failed to fetch URL — no readable content extracted");
  }

  function pLimit(concurrency: number): <T>(fn: () => Promise<T>) => Promise<T> {
    let active = 0;
    const queue: Array<() => void> = [];
    return <T>(fn: () => Promise<T>): Promise<T> => {
      return new Promise<T>((resolve, reject) => {
        const run = () => {
          active++;
          fn().then(resolve, reject).finally(() => {
            active--;
            if (queue.length > 0) queue.shift()!();
          });
        };
        if (active < concurrency) run();
        else queue.push(run);
      });
    };
  }

  async function fetchSingleUrl(pi: ExtensionAPI, url: string, signal?: AbortSignal): Promise<string> {
    const ghInfo = parseGitHubUrl(url);
    if (ghInfo) {
      const localPath = await cloneGitHub(pi, ghInfo, signal);
      if (!localPath) throw new Error("Failed to clone " + ghInfo.owner + "/" + ghInfo.repo);
      return generateGitHubContent(localPath, ghInfo);
    }
    if (isRedditUrl(url)) {
      return fetchReddit(url, signal);
    }
    return fetchGenericUrl(pi, url, signal);
  }

  export default function (pi: ExtensionAPI) {
    pi.registerTool({
      name: "fetch_url",
      label: "Fetch URL",
      description: "Fetch web page(s) and return readable text content. Supports single URL or batch fetching. GitHub URLs are cloned locally. Reddit URLs use the JSON API. Other URLs try readability extraction with Jina Reader fallback. Binary content types are detected and skipped.",
      promptSnippet: "Fetch a URL and extract readable text content",
      parameters: Type.Object({
        url: Type.Optional(Type.String({ description: "Single URL to fetch. Mutually exclusive with 'urls'." })),
        urls: Type.Optional(Type.Array(Type.String(), { description: "Multiple URLs to fetch concurrently (max 10). Mutually exclusive with 'url'." })),
      }),
      async execute(_toolCallId, params, signal) {
        const maxLength = 50000;

        const hasUrl = params.url !== undefined && params.url !== null;
        const hasUrls = params.urls !== undefined && params.urls !== null;

        if (hasUrl && hasUrls) {
          return {
            content: [{ type: "text", text: "Error: 'url' and 'urls' are mutually exclusive. Use one or the other." }],
            details: {},
          };
        }
        if (!hasUrl && !hasUrls) {
          return {
            content: [{ type: "text", text: "Error: provide either 'url' or 'urls'." }],
            details: {},
          };
        }

        const urlList: string[] = hasUrls
          ? (params.urls as string[]).filter(u => typeof u === "string" && u.trim().length > 0).slice(0, 10)
          : [params.url as string];

        if (urlList.length === 0) {
          return {
            content: [{ type: "text", text: "Error: no valid URLs provided." }],
            details: {},
          };
        }

        // Single URL — simple path
        if (urlList.length === 1) {
          let text: string;
          try {
            text = await fetchSingleUrl(pi, urlList[0], signal);
          } catch (e: any) {
            throw e;
          }
          const truncated = text.length > maxLength
            ? text.slice(0, maxLength) + "\n\n[Truncated - content exceeded " + maxLength + " characters]"
            : text;
          return {
            content: [{ type: "text", text: truncated || "No readable content found." }],
            details: {},
          };
        }

        // Batch — concurrent with limit
        const limit = pLimit(CONCURRENT_LIMIT);
        const results = await Promise.allSettled(
          urlList.map(u => limit(() => fetchSingleUrl(pi, u, signal)))
        );

        const sections: string[] = [];
        for (let i = 0; i < urlList.length; i++) {
          const header = "--- [" + (i + 1) + "/" + urlList.length + "] " + urlList[i] + " ---";
          const settled = results[i];
          let body: string;
          if (settled.status === "rejected") {
            body = "Error: " + (settled.reason?.message || String(settled.reason));
          } else {
            body = settled.value || "No readable content found.";
          }
          // Truncate individual results in batch mode
          const perUrlMax = Math.floor(maxLength / urlList.length);
          if (body.length > perUrlMax) {
            body = body.slice(0, perUrlMax) + "\n\n[Truncated]";
          }
          sections.push(header + "\n" + body);
        }

        const combined = sections.join("\n\n");
        return {
          content: [{ type: "text", text: combined }],
          details: { urlCount: urlList.length, results: results.map(r => r.status) },
        };
      },
    });
  }
''
