/**
 * String primitive functions for Hydra-TypeScript.
 */

export function cat(parts: ReadonlyArray<string>): string {
  return parts.join("");
}

export function cat2(a: string, b: string): string {
  return a + b;
}

export function intercalate(sep: string, parts: ReadonlyArray<string>): string {
  return parts.join(sep);
}

export function lines(s: string): ReadonlyArray<string> {
  return s.split("\n");
}

export function unlines(ls: ReadonlyArray<string>): string {
  return ls.join("\n");
}

export function words(s: string): ReadonlyArray<string> {
  return s.split(/\s+/).filter((w) => w.length > 0);
}

export function unwords(ws: ReadonlyArray<string>): string {
  return ws.join(" ");
}

export function length(s: string): number {
  return s.length;
}

export function isEmpty(s: string): boolean {
  return s.length === 0;
}

export function toLower(s: string): string {
  return s.toLowerCase();
}

export function toUpper(s: string): string {
  return s.toUpperCase();
}

export function trim(s: string): string {
  return s.trim();
}

export function splitOn(sep: string, s: string): ReadonlyArray<string> {
  return s.split(sep);
}

export function startsWith(prefix: string, s: string): boolean {
  return s.startsWith(prefix);
}

export function endsWith(suffix: string, s: string): boolean {
  return s.endsWith(suffix);
}

export function contains(sub: string, s: string): boolean {
  return s.includes(sub);
}

export function replaceAll(
  search: string,
  replacement: string,
  s: string,
): string {
  return s.split(search).join(replacement);
}
