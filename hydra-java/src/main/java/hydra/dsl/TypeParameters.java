package hydra.dsl;

/**
 * Phantom type variables for use in DSL signatures.
 *
 * <p>Java interface fields cannot introduce type parameters, so universally
 * quantified type variables (like Haskell's {@code x} in {@code x -> Term -> x})
 * are represented using these marker interfaces instead.
 *
 * <p>Usage: {@code import hydra.dsl.TypeParameters.*;}
 */
public interface TypeParameters {
    interface X {}
    interface Y {}
    interface Z {}
}
