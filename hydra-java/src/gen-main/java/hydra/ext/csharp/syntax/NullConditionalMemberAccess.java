// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NullConditionalMemberAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NullConditionalMemberAccess");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_DEPENDENT_ACCESS = new hydra.core.Name("dependentAccess");
  
  public final hydra.ext.csharp.syntax.PrimaryExpression expression;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments;
  
  public final java.util.List<hydra.ext.csharp.syntax.DependentAccess> dependentAccess;
  
  public NullConditionalMemberAccess (hydra.ext.csharp.syntax.PrimaryExpression expression, hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments, java.util.List<hydra.ext.csharp.syntax.DependentAccess> dependentAccess) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((typeArguments));
    java.util.Objects.requireNonNull((dependentAccess));
    this.expression = expression;
    this.identifier = identifier;
    this.typeArguments = typeArguments;
    this.dependentAccess = dependentAccess;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullConditionalMemberAccess)) {
      return false;
    }
    NullConditionalMemberAccess o = (NullConditionalMemberAccess) (other);
    return expression.equals(o.expression) && identifier.equals(o.identifier) && typeArguments.equals(o.typeArguments) && dependentAccess.equals(o.dependentAccess);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * identifier.hashCode() + 5 * typeArguments.hashCode() + 7 * dependentAccess.hashCode();
  }
  
  public NullConditionalMemberAccess withExpression(hydra.ext.csharp.syntax.PrimaryExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new NullConditionalMemberAccess(expression, identifier, typeArguments, dependentAccess);
  }
  
  public NullConditionalMemberAccess withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new NullConditionalMemberAccess(expression, identifier, typeArguments, dependentAccess);
  }
  
  public NullConditionalMemberAccess withTypeArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new NullConditionalMemberAccess(expression, identifier, typeArguments, dependentAccess);
  }
  
  public NullConditionalMemberAccess withDependentAccess(java.util.List<hydra.ext.csharp.syntax.DependentAccess> dependentAccess) {
    java.util.Objects.requireNonNull((dependentAccess));
    return new NullConditionalMemberAccess(expression, identifier, typeArguments, dependentAccess);
  }
}