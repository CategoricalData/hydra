// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * A set of constraints on valid type and term expressions, characterizing a language
 */
public class LanguageConstraints implements Serializable, Comparable<LanguageConstraints> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.LanguageConstraints");
  
  public static final hydra.core.Name FIELD_NAME_ELIMINATION_VARIANTS = new hydra.core.Name("eliminationVariants");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL_VARIANTS = new hydra.core.Name("literalVariants");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT_TYPES = new hydra.core.Name("floatTypes");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION_VARIANTS = new hydra.core.Name("functionVariants");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER_TYPES = new hydra.core.Name("integerTypes");
  
  public static final hydra.core.Name FIELD_NAME_TERM_VARIANTS = new hydra.core.Name("termVariants");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_VARIANTS = new hydra.core.Name("typeVariants");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  /**
   * All supported elimination variants
   */
  public final java.util.Set<hydra.variants.EliminationVariant> eliminationVariants;
  
  /**
   * All supported literal variants
   */
  public final java.util.Set<hydra.variants.LiteralVariant> literalVariants;
  
  /**
   * All supported float types
   */
  public final java.util.Set<hydra.core.FloatType> floatTypes;
  
  /**
   * All supported function variants
   */
  public final java.util.Set<hydra.variants.FunctionVariant> functionVariants;
  
  /**
   * All supported integer types
   */
  public final java.util.Set<hydra.core.IntegerType> integerTypes;
  
  /**
   * All supported term variants
   */
  public final java.util.Set<hydra.variants.TermVariant> termVariants;
  
  /**
   * All supported type variants
   */
  public final java.util.Set<hydra.variants.TypeVariant> typeVariants;
  
  /**
   * A logical set of types, as a predicate which tests a type for inclusion
   */
  public final java.util.function.Function<hydra.core.Type, Boolean> types;
  
  public LanguageConstraints (java.util.Set<hydra.variants.EliminationVariant> eliminationVariants, java.util.Set<hydra.variants.LiteralVariant> literalVariants, java.util.Set<hydra.core.FloatType> floatTypes, java.util.Set<hydra.variants.FunctionVariant> functionVariants, java.util.Set<hydra.core.IntegerType> integerTypes, java.util.Set<hydra.variants.TermVariant> termVariants, java.util.Set<hydra.variants.TypeVariant> typeVariants, java.util.function.Function<hydra.core.Type, Boolean> types) {
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
    LanguageConstraints o = (LanguageConstraints) (other);
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
    cmp = Integer.compare(
      eliminationVariants.hashCode(),
      other.eliminationVariants.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      literalVariants.hashCode(),
      other.literalVariants.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      floatTypes.hashCode(),
      other.floatTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      functionVariants.hashCode(),
      other.functionVariants.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      integerTypes.hashCode(),
      other.integerTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      termVariants.hashCode(),
      other.termVariants.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeVariants.hashCode(),
      other.typeVariants.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      types.hashCode(),
      other.types.hashCode());
  }
  
  public LanguageConstraints withEliminationVariants(java.util.Set<hydra.variants.EliminationVariant> eliminationVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withLiteralVariants(java.util.Set<hydra.variants.LiteralVariant> literalVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFloatTypes(java.util.Set<hydra.core.FloatType> floatTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFunctionVariants(java.util.Set<hydra.variants.FunctionVariant> functionVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withIntegerTypes(java.util.Set<hydra.core.IntegerType> integerTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTermVariants(java.util.Set<hydra.variants.TermVariant> termVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypeVariants(java.util.Set<hydra.variants.TypeVariant> typeVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypes(java.util.function.Function<hydra.core.Type, Boolean> types) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
}
