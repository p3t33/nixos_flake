{ lib, pkgs }:

''
  import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
  import { Type } from "@sinclair/typebox";
  import { mkdtemp, readFile, readdir, writeFile } from "node:fs/promises";
  import { tmpdir } from "node:os";
  import { basename, extname, join, resolve } from "node:path";
  import { pathToFileURL } from "node:url";

  type ParsedDocument = {
    text: string;
    method: string;
  };

  const maxLength = 50000;
  const pdftotext = "${lib.getExe' pkgs."poppler-utils" "pdftotext"}";
  const pandoc = "${lib.getExe pkgs.pandoc}";
  const libreoffice = "${lib.getExe' pkgs.libreoffice "libreoffice"}";
  const unzip = "${lib.getExe pkgs.unzip}";

  const pandocExtensions = new Set([
    ".docx",
    ".odt",
    ".epub",
    ".html",
    ".htm",
    ".md",
    ".rst",
  ]);

  const libreOfficeExtensions = new Set([
    ".doc",
    ".docx",
    ".odt",
    ".rtf",
    ".ppt",
    ".pptx",
    ".xls",
    ".xlsx",
    ".ods",
    ".odp",
    ".txt",
    ".csv",
  ]);

  function normalizeInputPath(path: string, cwd: string): string {
    const cleaned = path.startsWith("@") ? path.slice(1) : path;
    return resolve(cwd, cleaned);
  }

  function normalizeFormat(format: unknown): "plain" | "markdown" | "xml" {
    if (format === undefined) return "plain";
    if (format === "plain" || format === "markdown" || format === "xml") return format;
    throw new Error("format must be one of: plain, markdown, xml");
  }

  function decodeXmlEntities(text: string): string {
    return text
      .replace(/&lt;/g, "<")
      .replace(/&gt;/g, ">")
      .replace(/&amp;/g, "&")
      .replace(/&quot;/g, '"')
      .replace(/&apos;/g, "'");
  }

  function xmlToText(xml: string): string {
    return decodeXmlEntities(
      xml
        .replace(/<w:tab\s*\/?\s*>/g, "\t")
        .replace(/<w:br\s*\/?\s*>/g, "\n")
        .replace(/<text:line-break\s*\/?\s*>/g, "\n")
        .replace(/<\/(w:p|text:p|text:h)>/g, "\n")
        .replace(/<[^>]+>/g, "")
    )
      .replace(/[ \t]+\n/g, "\n")
      .replace(/\n{3,}/g, "\n\n")
      .trim();
  }

  async function runCommand(
    pi: ExtensionAPI,
    command: string,
    args: string[],
    signal: AbortSignal | undefined,
    timeout = 120000
  ): Promise<string> {
    const result = await pi.exec(command, args, { signal, timeout });
    if (result.code !== 0) {
      throw new Error((result.stderr || result.stdout || "command failed").trim());
    }
    return result.stdout;
  }

  async function parsePdf(pi: ExtensionAPI, path: string, signal?: AbortSignal): Promise<ParsedDocument> {
    const text = await runCommand(pi, pdftotext, [path, "-"], signal);
    return { text, method: "pdftotext" };
  }

  async function parseWithPandoc(
    pi: ExtensionAPI,
    path: string,
    format: "plain" | "markdown" | "xml",
    signal?: AbortSignal
  ): Promise<ParsedDocument> {
    const target = format === "markdown" ? "gfm" : "plain";
    const text = await runCommand(pi, pandoc, [path, "-t", target, "--wrap=none"], signal);
    return { text, method: "pandoc -> " + target };
  }

  async function parseWithLibreOffice(
    pi: ExtensionAPI,
    path: string,
    signal?: AbortSignal
  ): Promise<ParsedDocument> {
    const outDir = await mkdtemp(join(tmpdir(), "pi-doc-out-"));
    const profileDir = await mkdtemp(join(tmpdir(), "pi-lo-profile-"));
    await runCommand(
      pi,
      libreoffice,
      [
        "-env:UserInstallation=" + pathToFileURL(profileDir).href,
        "--headless",
        "--convert-to",
        "txt:Text",
        "--outdir",
        outDir,
        path,
      ],
      signal
    );
    const files = await readdir(outDir);
    const textFile = files.find((file) => file.toLowerCase().endsWith(".txt"));
    if (!textFile) throw new Error("LibreOffice did not produce a text file");
    const text = await readFile(join(outDir, textFile), "utf8");
    return { text, method: "libreoffice -> txt" };
  }

  async function readZipXml(
    pi: ExtensionAPI,
    path: string,
    entry: string,
    format: "plain" | "markdown" | "xml",
    signal?: AbortSignal
  ): Promise<ParsedDocument> {
    const xml = await runCommand(pi, unzip, ["-p", path, entry], signal);
    return {
      text: format === "xml" ? xml : xmlToText(xml),
      method: "unzip " + entry + (format === "xml" ? "" : " -> text"),
    };
  }

  async function saveIfTruncated(text: string, originalPath: string): Promise<{
    text: string;
    truncated: boolean;
    fullTextPath?: string;
  }> {
    if (text.length <= maxLength) return { text, truncated: false };
    const outDir = await mkdtemp(join(tmpdir(), "pi-doc-full-"));
    const fullTextPath = join(outDir, basename(originalPath) + ".txt");
    await writeFile(fullTextPath, text, "utf8");
    return {
      text: text.slice(0, maxLength) + "\n\n[Truncated - extracted content exceeded " + maxLength + " characters. Full text saved to: " + fullTextPath + "]",
      truncated: true,
      fullTextPath,
    };
  }

  export default function (pi: ExtensionAPI) {
    pi.registerTool({
      name: "parse_document",
      label: "Parse Document",
      description: "Extract readable text from local PDF, Word, OpenOffice/LibreOffice, RTF, EPUB, spreadsheet, and presentation files. Output is truncated to 50KB; full extracted text is saved to a temp file when truncated.",
      promptSnippet: "Extract readable text from local document files like PDF, DOCX, ODT, DOC, RTF, EPUB, XLSX, and PPTX",
      promptGuidelines: [
        "Use parse_document when the user asks to read, summarize, inspect, or quote a local PDF, Word, OpenOffice/LibreOffice, EPUB, spreadsheet, or presentation file.",
      ],
      parameters: Type.Object({
        path: Type.String({ description: "Path to the local document file" }),
        format: Type.Optional(Type.String({ description: "Output format: plain, markdown, or xml. Defaults to plain." })),
      }),
      async execute(_toolCallId, params, signal, _onUpdate, ctx) {
        const path = normalizeInputPath(params.path, ctx.cwd);
        const format = normalizeFormat(params.format);
        const extension = extname(path).toLowerCase();
        const attempts: string[] = [];

        async function attempt(name: string, parse: () => Promise<ParsedDocument>): Promise<ParsedDocument | undefined> {
          try {
            const parsed = await parse();
            if (parsed.text.trim() !== "") return parsed;
            attempts.push(name + ": empty output");
          } catch (error) {
            attempts.push(name + ": " + (error instanceof Error ? error.message : String(error)));
          }
          return undefined;
        }

        let parsed: ParsedDocument | undefined;

        if (extension === ".pdf") {
          parsed = await attempt("pdftotext", () => parsePdf(pi, path, signal));
          if (!parsed) {
            throw new Error("Could not extract text from PDF. It may be scanned/image-only; OCR support is not enabled. Attempts: " + attempts.join("; "));
          }
        } else if (format === "xml" && extension === ".docx") {
          parsed = await attempt("docx xml", () => readZipXml(pi, path, "word/document.xml", format, signal));
        } else if (format === "xml" && extension === ".odt") {
          parsed = await attempt("odt xml", () => readZipXml(pi, path, "content.xml", format, signal));
        } else {
          if (pandocExtensions.has(extension)) {
            parsed = await attempt("pandoc", () => parseWithPandoc(pi, path, format, signal));
          }
          if (!parsed && libreOfficeExtensions.has(extension)) {
            parsed = await attempt("libreoffice", () => parseWithLibreOffice(pi, path, signal));
          }
          if (!parsed && extension === ".docx") {
            parsed = await attempt("docx xml fallback", () => readZipXml(pi, path, "word/document.xml", format, signal));
          }
          if (!parsed && extension === ".odt") {
            parsed = await attempt("odt xml fallback", () => readZipXml(pi, path, "content.xml", format, signal));
          }
        }

        if (!parsed) {
          throw new Error("Unsupported or unreadable document type " + extension + ". Attempts: " + attempts.join("; "));
        }

        const text = parsed.text.trim();
        const output = await saveIfTruncated(text, path);
        return {
          content: [{ type: "text", text: output.text || "No readable content found." }],
          details: {
            path,
            format,
            extension,
            method: parsed.method,
            characters: text.length,
            truncated: output.truncated,
            fullTextPath: output.fullTextPath,
            attempts,
          },
        };
      },
    });
  }
''
