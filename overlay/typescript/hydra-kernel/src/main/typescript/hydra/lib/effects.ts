// Hand-written runtime: hydra.lib.effects primitives.
//
// In TypeScript the Hydra type `effect<t>` is transparent (effect<t> = t):
// effectful programs are ordinary eager native code and "running the effect"
// simply means forcing the value. These primitives therefore reduce to ordinary
// function applications, mirroring the Python reference in
// overlay/python/hydra-kernel/.../hydra/python/lib/effects.py.

export const apply = (f: (a: any) => any, a: any): any => f(a);

export const bind = (a: any, f: (a: any) => any): any => f(a);

export const compose = (f: (a: any) => any, g: (b: any) => any, a: any): any => g(f(a));

export const foldl = (f: (acc: any, x: any) => any, acc: any, xs: readonly any[]): any => {
  let r = acc;
  for (const x of xs) r = f(r, x);
  return r;
};

export const map = (f: (a: any) => any, a: any): any => f(a);

export const mapList = (f: (a: any) => any, xs: readonly any[]): readonly any[] =>
  xs.map(f);

export const mapOptional = (f: (a: any) => any, m: any): any =>
  m.tag === "given" ? { tag: "given", value: f(m.value) } : m;

export const pure = <A>(x: A): A => x;
