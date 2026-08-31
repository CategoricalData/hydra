// Shared integral-arithmetic helpers.
//
// These live in their OWN leaf module rather than in libraries.ts (the primitive
// registry) so that primitive implementations can use them without importing the
// registry. libraries.ts imports every lib module, so a lib module importing
// libraries.ts back would close a cycle:
//
//     libraries.ts -> hydra.lexical -> math.ts -> libraries.ts
//
// which throws "Cannot access '<const>' before initialization" (temporal dead
// zone) when the packed npm artifact is imported. Every other Hydra host keeps
// primitives free of any dependency on its registry module; this file restores
// that invariant for TypeScript. See #677 (floor/Knuth division) for how the
// helpers came to be shared in the first place.
//
// Nothing here may import any other hydra module: it must stay a leaf.

export const INT_WIDTH_BITS: Record<string, number> =
  { int8: 8, int16: 16, int32: 32, int64: 64 };

export const UINT_WIDTH_BITS: Record<string, number> =
  { uint8: 8, uint16: 16, uint32: 32, uint64: 64 };

/** Wrap a bigint into the two's-complement range of the named integer width. */
export const wrapInt = (widthTag: string, r: bigint): number | bigint => {
  if (widthTag === "bigint") return r;
  const signedBits = INT_WIDTH_BITS[widthTag];
  if (signedBits !== undefined) {
    const m = 1n << BigInt(signedBits);
    let w = ((r % m) + m) % m;
    if (w >= m / 2n) w -= m;
    return signedBits > 32 ? w : Number(w);
  }
  const unsignedBits = UINT_WIDTH_BITS[widthTag];
  if (unsignedBits === undefined) {
    throw new Error(`wrapInt: unknown integer width '${widthTag}'`);
  }
  const m = 1n << BigInt(unsignedBits);
  const w = ((r % m) + m) % m;
  return unsignedBits > 32 ? w : Number(w);
};

/** Floor division (Knuth), as distinct from truncating division. */
export const floorDivBig = (a: bigint, b: bigint): bigint => {
  const q = a / b;
  return (a % b !== 0n) && ((a < 0n) !== (b < 0n)) ? q - 1n : q;
};

/** Floor modulus (Knuth): result carries the sign of the divisor. */
export const floorModBig = (a: bigint, b: bigint): bigint => {
  const r = a % b;
  return r !== 0n && ((r < 0n) !== (b < 0n)) ? r + b : r;
};
