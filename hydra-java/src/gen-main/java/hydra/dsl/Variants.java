// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.variants
 */
public interface Variants {
  static hydra.phantoms.TTerm<hydra.variants.EliminationVariant> eliminationVariantRecord() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.EliminationVariant"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.EliminationVariant> eliminationVariantUnion() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.EliminationVariant"), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.EliminationVariant> eliminationVariantWrap() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.EliminationVariant"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.FunctionVariant> functionVariantElimination() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.FunctionVariant"), new hydra.core.Field(new hydra.core.Name("elimination"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.FunctionVariant> functionVariantLambda() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.FunctionVariant"), new hydra.core.Field(new hydra.core.Name("lambda"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.FunctionVariant> functionVariantPrimitive() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.FunctionVariant"), new hydra.core.Field(new hydra.core.Name("primitive"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.LiteralVariant> literalVariantBinary() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.LiteralVariant> literalVariantBoolean() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.LiteralVariant> literalVariantFloat() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.LiteralVariant> literalVariantInteger() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.LiteralVariant> literalVariantString() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.LiteralVariant"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantAnnotated() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("annotated"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantApplication() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantEither() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantFunction() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantLet() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("let"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantList() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantLiteral() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantMap() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantMaybe() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantPair() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantRecord() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantSet() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantTypeApplication() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("typeApplication"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantTypeLambda() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("typeLambda"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantUnion() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantUnit() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantVariable() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariantWrap() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TermVariant"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantAnnotated() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("annotated"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantApplication() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantEither() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("either"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantForall() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("forall"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantFunction() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantList() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("list"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantLiteral() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantMap() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("map"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantMaybe() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("maybe"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantPair() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("pair"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantRecord() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantSet() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("set"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantUnion() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantUnit() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantVariable() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> typeVariantWrap() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.variants.TypeVariant"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Unit()))));
  }
}
