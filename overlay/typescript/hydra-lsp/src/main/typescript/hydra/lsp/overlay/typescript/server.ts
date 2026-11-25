// Hydra language server: transport-independent core (#836, increment 1).
//
// This file holds the host-native "layer 3" logic that is not the connection itself: server state,
// the method -> handler dispatch table, and the conversions between native JSON and Hydra's JSON model.
// All protocol translation (enum <-> integer tables, omitted optionals) lives in the generated
// hydra.ext.lsp.wire module; all analysis in hydra.lsp.analysis. It has no third-party imports so that it can
// be unit-tested without an LSP client; main.ts attaches it to vscode-languageserver.

import * as Analysis from "../../analysis.js";
import * as Wire from "../../../ext/lsp/wire.js";
import * as JsonParser from "../../../core/json/parser.js";
import * as JsonWriter from "../../../core/json/writer.js";

/** Open documents (uri -> text) plus the shutdown flag. */
export interface ServerState {
  readonly documents: Map<string, string>;
  shutdownRequested: boolean;
}

export function newState(): ServerState {
  return { documents: new Map(), shutdownRequested: false };
}

/** Where a handler's output goes: notifications to publish, and the request result (if any). */
export interface Outcome {
  readonly result?: unknown;
  readonly notifications: ReadonlyArray<{ readonly method: string; readonly params: unknown }>;
}

export type Handler = (state: ServerState, params: unknown) => Outcome;

// Hydra's JSON model <-> native JSON, via the kernel's own parser and printer (no hand-rolled number handling).

export function toHydraJson(native: unknown): any {
  const parsed = JsonParser.parseJson(JSON.stringify(native === undefined ? null : native));
  // The kernel parser returns a ParseResult: success carries {value, remainder}; failure carries a ParseError.
  if (parsed.tag !== "success") {
    throw new Error("internal: could not convert native JSON: " + String(parsed.value && parsed.value.message));
  }
  return parsed.value.value;
}

export function fromHydraJson(value: any): unknown {
  return JSON.parse(JsonWriter.printJson(value));
}

function decodeOrThrow<T>(result: any, what: string): T {
  if (result.tag !== "right") throw new Error("invalid " + what + " params: " + String(result.value));
  return result.value as T;
}

/** The LSP end position of a text: line count and the length of the last line in UTF-16 code units. */
export function endPosition(text: string): { line: number; character: number } {
  const lines = text.split("\n");
  return { line: lines.length - 1, character: lines[lines.length - 1].length };
}

/** textDocument/formatting: a single whole-document edit, or no edits if the text does not parse. */
export function formattingEdits(text: string): ReadonlyArray<unknown> {
  const formatted = Analysis.format(text);
  if (formatted.tag !== "given") return [];
  const edit = {
    range: { start: { line: 0, character: 0 }, end: endPosition(text) },
    newText: formatted.value as string,
  };
  return [fromHydraJson(Wire.encodeTextEdit(edit))];
}

/** Diagnostics for a document. Increment 1 wires the publishing path; the analysis arrives with #834's spans. */
export function diagnosticsFor(_text: string): ReadonlyArray<unknown> {
  // TODO(#836): call hydra.lsp.analysis.diagnostics once #834's parseModuleWithSpansE/definitionSpanAtPath land.
  return [];
}

function publish(uri: string, version: number | undefined, text: string): Outcome["notifications"] {
  const diagnostics = diagnosticsFor(text);
  const params: Record<string, unknown> = { uri, diagnostics };
  if (version !== undefined) params.version = version;
  return [{ method: "textDocument/publishDiagnostics", params }];
}

const didOpen: Handler = (state, params) => {
  const p = decodeOrThrow<any>(Wire.decodeDidOpen(toHydraJson(params)), "didOpen");
  const doc = p.textDocument;
  state.documents.set(doc.uri, doc.text);
  return { notifications: publish(doc.uri, doc.version, doc.text) };
};

const didChange: Handler = (state, params) => {
  const p = decodeOrThrow<any>(Wire.decodeDidChange(toHydraJson(params)), "didChange");
  const changes: ReadonlyArray<any> = p.contentChanges;
  if (changes.length === 0) return { notifications: [] };
  // Full sync: the last change carries the entire new text.
  const text = changes[changes.length - 1].text as string;
  const doc = p.textDocument;
  state.documents.set(doc.uri, text);
  return { notifications: publish(doc.uri, doc.version, text) };
};

const didClose: Handler = (state, params) => {
  const p = decodeOrThrow<any>(Wire.decodeDidClose(toHydraJson(params)), "didClose");
  state.documents.delete(p.textDocument.uri);
  // Clear the document's diagnostics when it closes.
  return { notifications: [{ method: "textDocument/publishDiagnostics", params: { uri: p.textDocument.uri, diagnostics: [] } }] };
};

const formatting: Handler = (state, params) => {
  const p = decodeOrThrow<any>(Wire.decodeFormatting(toHydraJson(params)), "formatting");
  const text = state.documents.get(p.textDocument.uri);
  return { result: text === undefined ? null : formattingEdits(text), notifications: [] };
};

const shutdown: Handler = (state) => {
  state.shutdownRequested = true;
  return { result: null, notifications: [] };
};

/** The initialize result: advertises exactly what is implemented (full sync, formatting). */
export function initializeResult(): unknown {
  return fromHydraJson(Wire.encodeInitializeResult({
    capabilities: { textDocumentSync: { tag: "full" }, documentFormattingProvider: true },
  }));
}

/** The dispatch table: LSP method -> handler (lifecycle initialize/initialized/exit are handled by the connection). */
export const handlers: ReadonlyMap<string, Handler> = new Map<string, Handler>([
  ["shutdown", shutdown],
  ["textDocument/didChange", didChange],
  ["textDocument/didClose", didClose],
  ["textDocument/didOpen", didOpen],
  ["textDocument/formatting", formatting],
]);

/** Process exit code on `exit`: 0 if shutdown was requested first, else 1 (per the LSP spec). */
export function exitCode(state: ServerState): number {
  return state.shutdownRequested ? 0 : 1;
}
