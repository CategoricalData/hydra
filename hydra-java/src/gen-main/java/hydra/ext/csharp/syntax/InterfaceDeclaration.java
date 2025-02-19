// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfaceDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_PARTIAL = new hydra.core.Name("partial");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.InterfaceModifier> modifiers;
  
  public final Boolean partial;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.VariantTypeParameters> parameters;
  
  public final java.util.List<hydra.ext.csharp.syntax.InterfaceType> base;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public final java.util.List<hydra.ext.csharp.syntax.InterfaceMemberDeclaration> body;
  
  public InterfaceDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.InterfaceModifier> modifiers, Boolean partial, hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.VariantTypeParameters> parameters, java.util.List<hydra.ext.csharp.syntax.InterfaceType> base, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints, java.util.List<hydra.ext.csharp.syntax.InterfaceMemberDeclaration> body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((partial));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((base));
    java.util.Objects.requireNonNull((constraints));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.partial = partial;
    this.name = name;
    this.parameters = parameters;
    this.base = base;
    this.constraints = constraints;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceDeclaration)) {
      return false;
    }
    InterfaceDeclaration o = (InterfaceDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && partial.equals(o.partial) && name.equals(o.name) && parameters.equals(o.parameters) && base.equals(o.base) && constraints.equals(o.constraints) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * partial.hashCode() + 7 * name.hashCode() + 11 * parameters.hashCode() + 13 * base.hashCode() + 17 * constraints.hashCode() + 19 * body.hashCode();
  }
  
  public InterfaceDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.InterfaceModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withPartial(Boolean partial) {
    java.util.Objects.requireNonNull((partial));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.VariantTypeParameters> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withBase(java.util.List<hydra.ext.csharp.syntax.InterfaceType> base) {
    java.util.Objects.requireNonNull((base));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public InterfaceDeclaration withBody(java.util.List<hydra.ext.csharp.syntax.InterfaceMemberDeclaration> body) {
    java.util.Objects.requireNonNull((body));
    return new InterfaceDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
}