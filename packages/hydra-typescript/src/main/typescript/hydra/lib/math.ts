/**
 * Math primitive functions for Hydra-TypeScript.
 */

export function add(a: number, b: number): number {
  return a + b;
}

export function sub(a: number, b: number): number {
  return a - b;
}

export function mul(a: number, b: number): number {
  return a * b;
}

export function div(a: number, b: number): number {
  if (b === 0) {
    throw new Error("Division by zero");
  }
  return a / b;
}

export function mod(a: number, b: number): number {
  return a % b;
}

export function neg(a: number): number {
  return -a;
}

export function abs(a: number): number {
  return Math.abs(a);
}

export function floor(a: number): number {
  return Math.floor(a);
}

export function ceiling(a: number): number {
  return Math.ceil(a);
}

export function round(a: number): number {
  return Math.round(a);
}

export function sqrt(a: number): number {
  return Math.sqrt(a);
}

export function pow(base: number, exp: number): number {
  return Math.pow(base, exp);
}

export function rem(a: number, b: number): number {
  return a % b;
}
