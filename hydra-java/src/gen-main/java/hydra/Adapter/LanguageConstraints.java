package hydra.adapter;

public class LanguageConstraints<M> {
  public final java.util.Set<hydra.core.EliminationVariant> eliminationVariants;
  
  public final java.util.Set<hydra.core.LiteralVariant> literalVariants;
  
  public final java.util.Set<hydra.core.FloatType> floatTypes;
  
  public final java.util.Set<hydra.core.FunctionVariant> functionVariants;
  
  public final java.util.Set<hydra.core.IntegerType> integerTypes;
  
  public final java.util.Set<hydra.core.TermVariant> termVariants;
  
  public final java.util.Set<hydra.core.TypeVariant> typeVariants;
  
  public final java.util.function.Function<hydra.core.Type<M>, Boolean> types;
  
  public LanguageConstraints (java.util.Set<hydra.core.EliminationVariant> eliminationVariants, java.util.Set<hydra.core.LiteralVariant> literalVariants, java.util.Set<hydra.core.FloatType> floatTypes, java.util.Set<hydra.core.FunctionVariant> functionVariants, java.util.Set<hydra.core.IntegerType> integerTypes, java.util.Set<hydra.core.TermVariant> termVariants, java.util.Set<hydra.core.TypeVariant> typeVariants, java.util.function.Function<hydra.core.Type<M>, Boolean> types) {
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
  
  public LanguageConstraints withEliminationVariants(java.util.Set<hydra.core.EliminationVariant> eliminationVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withLiteralVariants(java.util.Set<hydra.core.LiteralVariant> literalVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFloatTypes(java.util.Set<hydra.core.FloatType> floatTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withFunctionVariants(java.util.Set<hydra.core.FunctionVariant> functionVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withIntegerTypes(java.util.Set<hydra.core.IntegerType> integerTypes) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTermVariants(java.util.Set<hydra.core.TermVariant> termVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypeVariants(java.util.Set<hydra.core.TypeVariant> typeVariants) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
  
  public LanguageConstraints withTypes(java.util.function.Function<hydra.core.Type<M>, Boolean> types) {
    return new LanguageConstraints(eliminationVariants, literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants, types);
  }
}