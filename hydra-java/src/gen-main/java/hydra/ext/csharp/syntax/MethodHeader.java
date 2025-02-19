// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class MethodHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MethodHeader");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMETERS = new hydra.core.Name("typeParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.ext.csharp.syntax.MemberName name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public MethodHeader (hydra.ext.csharp.syntax.MemberName name, hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters, hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typeParameters));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((constraints));
    this.name = name;
    this.typeParameters = typeParameters;
    this.parameters = parameters;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodHeader)) {
      return false;
    }
    MethodHeader o = (MethodHeader) (other);
    return name.equals(o.name) && typeParameters.equals(o.typeParameters) && parameters.equals(o.parameters) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typeParameters.hashCode() + 5 * parameters.hashCode() + 7 * constraints.hashCode();
  }
  
  public MethodHeader withName(hydra.ext.csharp.syntax.MemberName name) {
    java.util.Objects.requireNonNull((name));
    return new MethodHeader(name, typeParameters, parameters, constraints);
  }
  
  public MethodHeader withTypeParameters(hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters) {
    java.util.Objects.requireNonNull((typeParameters));
    return new MethodHeader(name, typeParameters, parameters, constraints);
  }
  
  public MethodHeader withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new MethodHeader(name, typeParameters, parameters, constraints);
  }
  
  public MethodHeader withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new MethodHeader(name, typeParameters, parameters, constraints);
  }
}