// Hand-written runtime: hydra.core.lib.functions primitives.

export const absurd = <A, B>(_v: A): B => {
  throw new Error("hydra.core.lib.functions.absurd: void has no inhabitants");
};

export const identity = <A>(a: A): A => a;
