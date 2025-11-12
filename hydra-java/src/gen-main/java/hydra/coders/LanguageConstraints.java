// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * A set of constraints on valid type and term expressions, characterizing a language
 */
public class LanguageConstraints implements Serializable {
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
  public final java.util.Set<hydra.meta.EliminationVariant> eliminationVariants;
  
  /**
   * All supported literal variants
   */
  public final java.util.Set<hydra.meta.LiteralVariant> literalVariants;
  
  /**
   * All supported float types
   */
  public final java.util.Set<hydra.core.FloatType> floatTypes;
  
  /**
   * All supported function variants
   */
  public final java.util.Set<hydra.meta.FunctionVariant> functionVariants;
  
  /**
   * All supported integer types
   */
  public final java.util.Set<hydra.core.IntegerType> integerTypes;
  
  /**
   * All supported term variants
   */
  public final java.util.Set<hydra.meta.TermVariant> termVariants;
  
  /**
   * All supported type variants
   */
  public final java.util.Set<hydra.meta.TypeVariant> typeVariants;
  
  /**
   * A logical set of types, as a predicate which tests a type for inclusion
   */
  public final java.util.function.Function<hydra.core.Type, Boolean> types;
  
  public LanguageConstraints (java.util.Set<hydra.meta.EliminationVariant> eliminationVariants, java.util.Set<hydra.meta.LiteralVariant> literalVariants, java.util.Set<hydra.core.FloatType> floatTypes, java.util.Set<hydra.meta.FunctionVariant> functionVariants, java.util.Set<hydra.core.IntegerType> integerTypes, java.util.Set<hydra.meta.TermVariant> termVariants, java.util.Set<hydra.meta.TypeVariant> typeVariants, java.util.function.Function<hydra.core.Type, Boolean> types) {
    java.util.Objects.requireNonNull((eliminationVariants));
    java.util.Objects.requireNonNull((literalVariants));
    java.util.Objects.requireNonNull((floatTypes));
    java.util.Objects.requireNonNull((functionVariants));
    java.util.Objects.requireNonNull((integerTypes));
    java.util.Objects.requireNonNull((termVariants));
    java.util.Objects.requireNonNull((typeVariants));
    java.util.Objects.requireNonNull((types));
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
    return eliminationVariants.equals(o.eliminationVariants) && literalVariants.equals(o.literalVariants) && floatTypes.equals(o.floatTypes) && functionVariants.equals(o.functionVariants) && integerTypes.equals(o.integerTypes) && termVariants.equals(o.termVariants) && typeVariants.equals(o.typeVariants) && types.equals(o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * eliminationVariants.hashCode() + 3 * literalVariants.hashCode() + 5 * floatTypes.hashCode() + 7 * functionVariants.hashCode() + 11 * integerTypes.hashCode() + 13 * termVariants.hashCode() + 17 * typeVariants.hashCode() + 19 * types.hashCode();
  }
  
  public LanguageConstraints withEliminationVariants(java.util.Set<hydra.meta.EliminationVariant> eliminationVariants) {
    java.util.Objects.requireNonNull((eliminationVariants));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withLiteralVariants(java.util.Set<hydra.meta.LiteralVariant> literalVariants) {
    java.util.Objects.requireNonNull((literalVariants));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFloatTypes(java.util.Set<hydra.core.FloatType> floatTypes) {
    java.util.Objects.requireNonNull((floatTypes));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFunctionVariants(java.util.Set<hydra.meta.FunctionVariant> functionVariants) {
    java.util.Objects.requireNonNull((functionVariants));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withIntegerTypes(java.util.Set<hydra.core.IntegerType> integerTypes) {
    java.util.Objects.requireNonNull((integerTypes));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTermVariants(java.util.Set<hydra.meta.TermVariant> termVariants) {
    java.util.Objects.requireNonNull((termVariants));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypeVariants(java.util.Set<hydra.meta.TypeVariant> typeVariants) {
    java.util.Objects.requireNonNull((typeVariants));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypes(java.util.function.Function<hydra.core.Type, Boolean> types) {
    java.util.Objects.requireNonNull((types));
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
}
