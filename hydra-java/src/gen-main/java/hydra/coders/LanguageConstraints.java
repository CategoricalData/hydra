package hydra.coders;

/**
 * A set of constraints on valid type and term expressions, characterizing a language
 */
public class LanguageConstraints<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/coders.LanguageConstraints");
  
  /**
   * All supported elimination variants
   */
  public final java.util.Set<hydra.mantle.EliminationVariant> eliminationVariants;
  
  /**
   * All supported literal variants
   */
  public final java.util.Set<hydra.mantle.LiteralVariant> literalVariants;
  
  /**
   * All supported float types
   */
  public final java.util.Set<hydra.core.FloatType> floatTypes;
  
  /**
   * All supported function variants
   */
  public final java.util.Set<hydra.mantle.FunctionVariant> functionVariants;
  
  /**
   * All supported integer types
   */
  public final java.util.Set<hydra.core.IntegerType> integerTypes;
  
  /**
   * All supported term variants
   */
  public final java.util.Set<hydra.mantle.TermVariant> termVariants;
  
  /**
   * All supported type variants
   */
  public final java.util.Set<hydra.mantle.TypeVariant> typeVariants;
  
  /**
   * A logical set of types, as a predicate which tests a type for inclusion
   */
  public final java.util.function.Function<hydra.core.Type<M>, Boolean> types;
  
  public LanguageConstraints (java.util.Set<hydra.mantle.EliminationVariant> eliminationVariants, java.util.Set<hydra.mantle.LiteralVariant> literalVariants, java.util.Set<hydra.core.FloatType> floatTypes, java.util.Set<hydra.mantle.FunctionVariant> functionVariants, java.util.Set<hydra.core.IntegerType> integerTypes, java.util.Set<hydra.mantle.TermVariant> termVariants, java.util.Set<hydra.mantle.TypeVariant> typeVariants, java.util.function.Function<hydra.core.Type<M>, Boolean> types) {
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
  
  public LanguageConstraints withEliminationVariants(java.util.Set<hydra.mantle.EliminationVariant> eliminationVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withLiteralVariants(java.util.Set<hydra.mantle.LiteralVariant> literalVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFloatTypes(java.util.Set<hydra.core.FloatType> floatTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFunctionVariants(java.util.Set<hydra.mantle.FunctionVariant> functionVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withIntegerTypes(java.util.Set<hydra.core.IntegerType> integerTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTermVariants(java.util.Set<hydra.mantle.TermVariant> termVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypeVariants(java.util.Set<hydra.mantle.TypeVariant> typeVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypes(java.util.function.Function<hydra.core.Type<M>, Boolean> types) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
}