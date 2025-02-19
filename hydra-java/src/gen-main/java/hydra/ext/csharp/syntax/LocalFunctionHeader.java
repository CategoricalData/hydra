// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class LocalFunctionHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LocalFunctionHeader");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMETERS = new hydra.core.Name("typeParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters;
  
  public final hydra.ext.csharp.syntax.FormalParameterList parameters;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public LocalFunctionHeader (hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters, hydra.ext.csharp.syntax.FormalParameterList parameters, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((typeParameters));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((constraints));
    this.identifier = identifier;
    this.typeParameters = typeParameters;
    this.parameters = parameters;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalFunctionHeader)) {
      return false;
    }
    LocalFunctionHeader o = (LocalFunctionHeader) (other);
    return identifier.equals(o.identifier) && typeParameters.equals(o.typeParameters) && parameters.equals(o.parameters) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * typeParameters.hashCode() + 5 * parameters.hashCode() + 7 * constraints.hashCode();
  }
  
  public LocalFunctionHeader withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new LocalFunctionHeader(identifier, typeParameters, parameters, constraints);
  }
  
  public LocalFunctionHeader withTypeParameters(hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters) {
    java.util.Objects.requireNonNull((typeParameters));
    return new LocalFunctionHeader(identifier, typeParameters, parameters, constraints);
  }
  
  public LocalFunctionHeader withParameters(hydra.ext.csharp.syntax.FormalParameterList parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new LocalFunctionHeader(identifier, typeParameters, parameters, constraints);
  }
  
  public LocalFunctionHeader withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new LocalFunctionHeader(identifier, typeParameters, parameters, constraints);
  }
}