// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * A set of constraints on valid type and term expressions, characterizing a language
 */
public class LanguageConstraints implements Serializable, Comparable<LanguageConstraints> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coders.LanguageConstraints");

  public static final hydra.core.Name ELIMINATION_VARIANTS = new hydra.core.Name("eliminationVariants");

  public static final hydra.core.Name LITERAL_VARIANTS = new hydra.core.Name("literalVariants");

  public static final hydra.core.Name FLOAT_TYPES = new hydra.core.Name("floatTypes");

  public static final hydra.core.Name FUNCTION_VARIANTS = new hydra.core.Name("functionVariants");

  public static final hydra.core.Name INTEGER_TYPES = new hydra.core.Name("integerTypes");

  public static final hydra.core.Name TERM_VARIANTS = new hydra.core.Name("termVariants");

  public static final hydra.core.Name TYPE_VARIANTS = new hydra.core.Name("typeVariants");

  public static final hydra.core.Name TYPES = new hydra.core.Name("types");

  /**
   * All supported elimination variants
   */
  public final hydra.util.PersistentSet<hydra.variants.EliminationVariant> eliminationVariants;

  /**
   * All supported literal variants
   */
  public final hydra.util.PersistentSet<hydra.variants.LiteralVariant> literalVariants;

  /**
   * All supported float types
   */
  public final hydra.util.PersistentSet<hydra.core.FloatType> floatTypes;

  /**
   * All supported function variants
   */
  public final hydra.util.PersistentSet<hydra.variants.FunctionVariant> functionVariants;

  /**
   * All supported integer types
   */
  public final hydra.util.PersistentSet<hydra.core.IntegerType> integerTypes;

  /**
   * All supported term variants
   */
  public final hydra.util.PersistentSet<hydra.variants.TermVariant> termVariants;

  /**
   * All supported type variants
   */
  public final hydra.util.PersistentSet<hydra.variants.TypeVariant> typeVariants;

  /**
   * A logical set of types, as a predicate which tests a type for inclusion
   */
  public final java.util.function.Function<hydra.core.Type, Boolean> types;

  public LanguageConstraints (hydra.util.PersistentSet<hydra.variants.EliminationVariant> eliminationVariants, hydra.util.PersistentSet<hydra.variants.LiteralVariant> literalVariants, hydra.util.PersistentSet<hydra.core.FloatType> floatTypes, hydra.util.PersistentSet<hydra.variants.FunctionVariant> functionVariants, hydra.util.PersistentSet<hydra.core.IntegerType> integerTypes, hydra.util.PersistentSet<hydra.variants.TermVariant> termVariants, hydra.util.PersistentSet<hydra.variants.TypeVariant> typeVariants, java.util.function.Function<hydra.core.Type, Boolean> types) {
    this.eliminationVariants = eliminationVariants;
    this.literalVariants = literalVariants;
    this.floatTypes = floatTypes;
    this.functionVariants = functionVariants;
    this.integerTypes = integerTypes;
    this.termVariants = termVariants;
    this.typeVariants = typeVariants;
    this.types = types;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LanguageConstraints)) {
      return false;
    }
    LanguageConstraints o = (LanguageConstraints) other;
    return java.util.Objects.equals(
      this.eliminationVariants,
      o.eliminationVariants) && java.util.Objects.equals(
      this.literalVariants,
      o.literalVariants) && java.util.Objects.equals(
      this.floatTypes,
      o.floatTypes) && java.util.Objects.equals(
      this.functionVariants,
      o.functionVariants) && java.util.Objects.equals(
      this.integerTypes,
      o.integerTypes) && java.util.Objects.equals(
      this.termVariants,
      o.termVariants) && java.util.Objects.equals(
      this.typeVariants,
      o.typeVariants) && java.util.Objects.equals(
      this.types,
      o.types);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(eliminationVariants) + 3 * java.util.Objects.hashCode(literalVariants) + 5 * java.util.Objects.hashCode(floatTypes) + 7 * java.util.Objects.hashCode(functionVariants) + 11 * java.util.Objects.hashCode(integerTypes) + 13 * java.util.Objects.hashCode(termVariants) + 17 * java.util.Objects.hashCode(typeVariants) + 19 * java.util.Objects.hashCode(types);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LanguageConstraints other) {
    int cmp = 0;
    cmp = ((Comparable) eliminationVariants).compareTo(other.eliminationVariants);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) literalVariants).compareTo(other.literalVariants);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) floatTypes).compareTo(other.floatTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) functionVariants).compareTo(other.functionVariants);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) integerTypes).compareTo(other.integerTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) termVariants).compareTo(other.termVariants);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) typeVariants).compareTo(other.typeVariants);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      types.hashCode(),
      other.types.hashCode());
  }

  public LanguageConstraints withEliminationVariants(hydra.util.PersistentSet<hydra.variants.EliminationVariant> eliminationVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withLiteralVariants(hydra.util.PersistentSet<hydra.variants.LiteralVariant> literalVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withFloatTypes(hydra.util.PersistentSet<hydra.core.FloatType> floatTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withFunctionVariants(hydra.util.PersistentSet<hydra.variants.FunctionVariant> functionVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withIntegerTypes(hydra.util.PersistentSet<hydra.core.IntegerType> integerTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withTermVariants(hydra.util.PersistentSet<hydra.variants.TermVariant> termVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withTypeVariants(hydra.util.PersistentSet<hydra.variants.TypeVariant> typeVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }

  public LanguageConstraints withTypes(java.util.function.Function<hydra.core.Type, Boolean> types) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
}
