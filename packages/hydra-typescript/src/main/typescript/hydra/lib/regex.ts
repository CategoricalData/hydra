/**
 * Regular expression primitive functions for Hydra-TypeScript.
 */

export function matches(pattern: string, input: string): boolean {
  return new RegExp(pattern).test(input);
}

export function replace(
  pattern: string,
  replacement: string,
  input: string,
): string {
  return input.replace(new RegExp(pattern, "g"), replacement);
}

export function split(pattern: string, input: string): ReadonlyArray<string> {
  return input.split(new RegExp(pattern));
}
