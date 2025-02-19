// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class TypeParameterConstraintsClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TypeParameterConstraintsClause");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.ext.csharp.syntax.TypeParameter parameter;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraints> constraints;
  
  public TypeParameterConstraintsClause (hydra.ext.csharp.syntax.TypeParameter parameter, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraints> constraints) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((constraints));
    this.parameter = parameter;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameterConstraintsClause)) {
      return false;
    }
    TypeParameterConstraintsClause o = (TypeParameterConstraintsClause) (other);
    return parameter.equals(o.parameter) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * constraints.hashCode();
  }
  
  public TypeParameterConstraintsClause withParameter(hydra.ext.csharp.syntax.TypeParameter parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new TypeParameterConstraintsClause(parameter, constraints);
  }
  
  public TypeParameterConstraintsClause withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraints> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new TypeParameterConstraintsClause(parameter, constraints);
  }
}