// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class DelegateHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DelegateHeader");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMETERS = new hydra.core.Name("typeParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.VariantTypeParameters> typeParameters;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public DelegateHeader (hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.VariantTypeParameters> typeParameters, hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
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
    if (!(other instanceof DelegateHeader)) {
      return false;
    }
    DelegateHeader o = (DelegateHeader) (other);
    return name.equals(o.name) && typeParameters.equals(o.typeParameters) && parameters.equals(o.parameters) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typeParameters.hashCode() + 5 * parameters.hashCode() + 7 * constraints.hashCode();
  }
  
  public DelegateHeader withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new DelegateHeader(name, typeParameters, parameters, constraints);
  }
  
  public DelegateHeader withTypeParameters(hydra.util.Opt<hydra.ext.csharp.syntax.VariantTypeParameters> typeParameters) {
    java.util.Objects.requireNonNull((typeParameters));
    return new DelegateHeader(name, typeParameters, parameters, constraints);
  }
  
  public DelegateHeader withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new DelegateHeader(name, typeParameters, parameters, constraints);
  }
  
  public DelegateHeader withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new DelegateHeader(name, typeParameters, parameters, constraints);
  }
}