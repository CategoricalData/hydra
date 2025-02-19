// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfaceMethodHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceMethodHeader");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMETERS = new hydra.core.Name("typeParameters");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public InterfaceMethodHeader (hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters, hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((typeParameters));
    java.util.Objects.requireNonNull((constraints));
    this.name = name;
    this.parameters = parameters;
    this.typeParameters = typeParameters;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceMethodHeader)) {
      return false;
    }
    InterfaceMethodHeader o = (InterfaceMethodHeader) (other);
    return name.equals(o.name) && parameters.equals(o.parameters) && typeParameters.equals(o.typeParameters) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * parameters.hashCode() + 5 * typeParameters.hashCode() + 7 * constraints.hashCode();
  }
  
  public InterfaceMethodHeader withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceMethodHeader(name, parameters, typeParameters, constraints);
  }
  
  public InterfaceMethodHeader withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new InterfaceMethodHeader(name, parameters, typeParameters, constraints);
  }
  
  public InterfaceMethodHeader withTypeParameters(hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> typeParameters) {
    java.util.Objects.requireNonNull((typeParameters));
    return new InterfaceMethodHeader(name, parameters, typeParameters, constraints);
  }
  
  public InterfaceMethodHeader withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new InterfaceMethodHeader(name, parameters, typeParameters, constraints);
  }
}