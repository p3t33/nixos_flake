{ lib, pkgs }:

let
  ocrmypdf = pkgs.ocrmypdf;
in
''
  import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
  import { Type } from "@sinclair/typebox";
  import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
  import { tmpdir } from "node:os";
  import { basename, dirname, extname, join, resolve } from "node:path";

  const ocrmypdf = "${lib.getExe ocrmypdf}";
  const maxLength = 50000;
  const allowedLanguages = new Set(["eng", "heb"]);

  function normalizeInputPath(path: string, cwd: string): string {
    const cleaned = path.startsWith("@") ? path.slice(1) : path;
    return resolve(cwd, cleaned);
  }

  function normalizeLanguages(value: unknown): string {
    if (value === undefined || value === null) return "eng+heb";
    if (typeof value !== "string") throw new Error("languages must be a string like 'eng+heb'");
    const parts = value.split("+").map((p) => p.trim()).filter(Boolean);
    if (parts.length === 0) throw new Error("languages must list at least one language code");
    for (const part of parts) {
      if (!allowedLanguages.has(part)) {
        throw new Error("Unsupported language '" + part + "'. Only 'eng' and 'heb' are installed.");
      }
    }
    return parts.join("+");
  }

  function normalizeMode(value: unknown): "force" | "skip" | "redo" | "default" {
    if (value === undefined || value === null) return "force";
    if (value === "force" || value === "skip" || value === "redo" || value === "default") return value;
    throw new Error("mode must be one of: force, skip, redo, default");
  }

  function defaultOutputPath(inputPath: string): string {
    const dir = dirname(inputPath);
    const ext = extname(inputPath);
    const base = basename(inputPath, ext);
    return join(dir, base + "_ocr.pdf");
  }

  export default function (pi: ExtensionAPI) {
    pi.registerTool({
      name: "ocr",
      label: "OCR PDF",
      description: "Run OCR on a scanned PDF and return the recognized text. Supports English and Hebrew (default 'eng+heb'). Produces a searchable PDF alongside the original and returns the extracted text. Output text is truncated to 50KB; the searchable PDF path is always reported.",
      promptSnippet: "OCR a scanned PDF (English/Hebrew) into searchable text",
      promptGuidelines: [
        "Use ocr when the user gives a scanned or image-only PDF and wants its text, especially English or Hebrew documents. parse_document fails on such files; ocr handles them.",
      ],
      parameters: Type.Object({
        path: Type.String({ description: "Path to the input PDF file" }),
        languages: Type.Optional(Type.String({ description: "Tesseract languages joined by '+'. Installed: eng, heb. Defaults to 'eng+heb'." })),
        mode: Type.Optional(Type.String({ description: "OCR mode: 'force' (default, rasterize and OCR every page), 'skip' (only OCR pages with no text), 'redo' (strip existing OCR text and redo), 'default' (error if the PDF already has text)." })),
        output: Type.Optional(Type.String({ description: "Path for the searchable output PDF. Defaults to '<input>_ocr.pdf' next to the input." })),
      }),
      async execute(_toolCallId, params, signal, onUpdate, ctx) {
        const inputPath = normalizeInputPath(params.path, ctx.cwd);
        const languages = normalizeLanguages(params.languages);
        const mode = normalizeMode(params.mode);

        if (extname(inputPath).toLowerCase() !== ".pdf") {
          throw new Error("ocr only accepts PDF input. Got: " + inputPath);
        }
        try {
          const info = await stat(inputPath);
          if (!info.isFile()) throw new Error("Not a file: " + inputPath);
        } catch (error) {
          throw new Error("Cannot read input PDF: " + (error instanceof Error ? error.message : String(error)));
        }

        const outputPath = params.output
          ? normalizeInputPath(params.output, ctx.cwd)
          : defaultOutputPath(inputPath);

        const sidecarDir = await mkdtemp(join(tmpdir(), "pi-ocr-"));
        const sidecarPath = join(sidecarDir, "ocr.txt");

        const args = ["-l", languages, "--output-type", "pdf", "--sidecar", sidecarPath];
        if (mode === "force") args.push("--force-ocr");
        else if (mode === "skip") args.push("--skip-text");
        else if (mode === "redo") args.push("--redo-ocr");
        args.push(inputPath, outputPath);

        onUpdate?.({ content: [{ type: "text", text: "Running OCR (" + languages + ", mode=" + mode + ")..." }] });

        let text = "";
        try {
          const result = await pi.exec(ocrmypdf, args, { signal, timeout: 600000 });
          if (result.code !== 0) {
            throw new Error((result.stderr || result.stdout || "ocrmypdf failed").trim());
          }
          try {
            text = (await readFile(sidecarPath, "utf8")).trim();
          } catch {
            text = "";
          }
        } finally {
          await rm(sidecarDir, { recursive: true, force: true }).catch(() => {});
        }

        const truncated = text.length > maxLength;
        const shownText = truncated
          ? text.slice(0, maxLength) + "\n\n[Truncated - OCR text exceeded " + maxLength + " characters. Full searchable PDF: " + outputPath + "]"
          : text;

        const header = "Searchable PDF written to: " + outputPath + "\n\n";
        return {
          content: [{ type: "text", text: header + (shownText || "No text recognized.") }],
          details: {
            inputPath,
            outputPath,
            languages,
            mode,
            characters: text.length,
            truncated,
          },
        };
      },
    });
  }
''
