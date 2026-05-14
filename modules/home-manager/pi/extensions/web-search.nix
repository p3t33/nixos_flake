{ config, lib, ... }:

let
  cfg = config.custom.programs.pi;
  braveApiKeyPath = cfg.webSearch.braveApiKeyFile;
in
{
  options.custom.programs.pi.webSearch.braveApiKeyFile = lib.mkOption {
    type = lib.types.nullOr lib.types.path;
    default = null;
    description = "Path to a file containing the Brave Search API key.";
  };

  config = lib.mkIf cfg.enable {
    warnings = lib.optional (braveApiKeyPath == null)
      "pi: web-search extension has no Brave API key configured. Web search will be unavailable at runtime.";

    custom.programs.pi.extensions.web-search = ''
      import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
      import { Type } from "@sinclair/typebox";
      import { readFileSync } from "node:fs";

      interface SearchResult {
        title: string;
        url: string;
        snippet: string;
      }

      interface SearchResponse {
        results: SearchResult[];
        provider: string;
      }

      const PROVIDERS = ["brave"] as const;
      type Provider = typeof PROVIDERS[number];

      function getBraveApiKey(): string {
        const envKey = process.env.BRAVE_API_KEY;
        if (envKey) return envKey;
        ${if braveApiKeyPath != null then ''
        try {
          return readFileSync("${braveApiKeyPath}", "utf-8").trim();
        } catch {}
        '' else ""}
        throw new Error("BRAVE_API_KEY not set and no secret file available.");
      }

      async function searchBrave(query: string, numResults: number, signal?: AbortSignal): Promise<SearchResponse> {
        const apiKey = getBraveApiKey();

        const params = new URLSearchParams({
          q: query,
          count: String(Math.min(numResults, 20)),
        });

        const resp = await fetch("https://api.search.brave.com/res/v1/web/search?" + params, {
          headers: {
            "Accept": "application/json",
            "Accept-Encoding": "gzip",
            "X-Subscription-Token": apiKey,
          },
          signal,
        });

        if (!resp.ok) {
          const body = await resp.text().catch(() => "");
          throw new Error("Brave API returned " + resp.status + ": " + body);
        }

        const data = await resp.json() as any;
        const results: SearchResult[] = (data.web?.results ?? []).map((r: any) => ({
          title: r.title ?? "",
          url: r.url ?? "",
          snippet: r.description ?? "",
        }));

        return { results, provider: "brave" };
      }

      function resolveProvider(): Provider {
        try { getBraveApiKey(); return "brave"; } catch {}
        throw new Error("No search provider available. Set BRAVE_API_KEY or configure a secret.");
      }

      async function doSearch(query: string, numResults: number, provider: Provider, signal?: AbortSignal): Promise<SearchResponse> {
        switch (provider) {
          case "brave": return searchBrave(query, numResults, signal);
        }
      }

      function formatResults(query: string, response: SearchResponse): string {
        if (response.results.length === 0) return "No results found for: " + query;

        let output = "## Results for: \"" + query + "\" (" + response.provider + ")\n\n";
        for (const r of response.results) {
          output += "### " + r.title + "\n" + r.url + "\n" + r.snippet + "\n\n";
        }
        return output;
      }

      export default function (pi: ExtensionAPI) {
        pi.registerTool({
          name: "web_search",
          label: "Web Search",
          description: "Search the web and return results with titles, URLs, and snippets. Use when you need to find information, documentation, or resources online.",
          promptSnippet: "Search the web for information",
          parameters: Type.Object({
            query: Type.String({ description: "Search query" }),
            numResults: Type.Optional(Type.Number({ description: "Number of results (default: 5, max: 20)" })),
            provider: Type.Optional(Type.String({ description: "Search provider to use (default: auto)" })),
          }),
          async execute(_toolCallId, params, signal) {
            const numResults = Math.min(params.numResults ?? 5, 20);
            const provider = (params.provider as Provider) ?? resolveProvider();
            const response = await doSearch(params.query, numResults, provider, signal);
            const text = formatResults(params.query, response);

            return {
              content: [{ type: "text", text }],
              details: {
                provider: response.provider,
                resultCount: response.results.length,
                query: params.query,
              },
            };
          },
        });
      }
    '';
  };
}
