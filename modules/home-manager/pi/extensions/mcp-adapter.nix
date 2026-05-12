{ port, host }:

''
  import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
  import { Type } from "@sinclair/typebox";

  const GATEWAY_URL = "http://${host}:${toString port}/mcp";

  async function mcpCall(method: string, params: Record<string, unknown>, signal?: AbortSignal): Promise<unknown> {
    const response = await fetch(GATEWAY_URL, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ jsonrpc: "2.0", id: 1, method, params }),
      signal,
    });
    if (!response.ok) {
      throw new Error("mcp-gateway returned HTTP " + response.status + ": " + (await response.text()));
    }
    const json = await response.json() as any;
    if (json.error) {
      throw new Error("MCP error " + json.error.code + ": " + json.error.message);
    }
    return json.result;
  }

  function formatResult(result: any): string {
    if (!result) return "No result";
    if (result.content) {
      return result.content
        .map((c: any) => c.text ?? JSON.stringify(c))
        .join("\n");
    }
    return JSON.stringify(result, null, 2);
  }

  export default function (pi: ExtensionAPI) {
    pi.registerTool({
      name: "mcp_search_tools",
      label: "MCP Search Tools",
      description: "Search for available MCP tools across all connected backends by keyword.",
      promptSnippet: "Search MCP gateway tools by keyword",
      parameters: Type.Object({
        query: Type.String({ description: "Search query to find relevant tools" }),
      }),
      async execute(_id, params, signal) {
        const result = await mcpCall("tools/call", {
          name: "gateway_search_tools",
          arguments: { query: params.query },
        }, signal);
        return {
          content: [{ type: "text", text: formatResult(result) }],
          details: {},
        };
      },
    });

    pi.registerTool({
      name: "mcp_list_servers",
      label: "MCP List Servers",
      description: "List all MCP backend servers connected to the gateway.",
      promptSnippet: "List connected MCP servers",
      parameters: Type.Object({}),
      async execute(_id, _params, signal) {
        const result = await mcpCall("tools/call", {
          name: "gateway_list_servers",
          arguments: {},
        }, signal);
        return {
          content: [{ type: "text", text: formatResult(result) }],
          details: {},
        };
      },
    });

    pi.registerTool({
      name: "mcp_list_tools",
      label: "MCP List Tools",
      description: "List all tools available on a specific MCP backend server.",
      promptSnippet: "List tools on a specific MCP server",
      parameters: Type.Object({
        server: Type.String({ description: "Backend server name to list tools from" }),
      }),
      async execute(_id, params, signal) {
        const result = await mcpCall("tools/call", {
          name: "gateway_list_tools",
          arguments: { server: params.server },
        }, signal);
        return {
          content: [{ type: "text", text: formatResult(result) }],
          details: {},
        };
      },
    });

    pi.registerTool({
      name: "mcp_invoke",
      label: "MCP Invoke Tool",
      description: "Invoke a tool on a specific MCP backend server. Use mcp_search_tools first to discover available tools and their parameters.",
      promptSnippet: "Invoke a specific MCP tool on a backend server",
      parameters: Type.Object({
        backend: Type.String({ description: "Backend server name" }),
        tool: Type.String({ description: "Tool name to invoke" }),
        args: Type.Optional(Type.Record(Type.String(), Type.Unknown(), { description: "Arguments to pass to the tool" })),
      }),
      async execute(_id, params, signal) {
        const result = await mcpCall("tools/call", {
          name: "gateway_invoke",
          arguments: {
            server: params.backend,
            tool: params.tool,
            args: params.args ?? {},
          },
        }, signal);
        return {
          content: [{ type: "text", text: formatResult(result) }],
          details: {},
        };
      },
    });
  }
''
