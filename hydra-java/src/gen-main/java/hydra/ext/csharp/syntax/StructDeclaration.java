// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StructDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StructDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_PARTIAL = new hydra.core.Name("partial");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACES = new hydra.core.Name("interfaces");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.StructModifier> modifiers;
  
  public final Boolean ref;
  
  public final Boolean partial;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> parameters;
  
  public final java.util.List<hydra.ext.csharp.syntax.InterfaceType> interfaces;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public final java.util.List<hydra.ext.csharp.syntax.StructMemberDeclaration> body;
  
  public StructDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.StructModifier> modifiers, Boolean ref, Boolean partial, hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> parameters, java.util.List<hydra.ext.csharp.syntax.InterfaceType> interfaces, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints, java.util.List<hydra.ext.csharp.syntax.StructMemberDeclaration> body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((ref));
    java.util.Objects.requireNonNull((partial));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((interfaces));
    java.util.Objects.requireNonNull((constraints));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.ref = ref;
    this.partial = partial;
    this.name = name;
    this.parameters = parameters;
    this.interfaces = interfaces;
    this.constraints = constraints;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructDeclaration)) {
      return false;
    }
    StructDeclaration o = (StructDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && ref.equals(o.ref) && partial.equals(o.partial) && name.equals(o.name) && parameters.equals(o.parameters) && interfaces.equals(o.interfaces) && constraints.equals(o.constraints) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * ref.hashCode() + 7 * partial.hashCode() + 11 * name.hashCode() + 13 * parameters.hashCode() + 17 * interfaces.hashCode() + 19 * constraints.hashCode() + 23 * body.hashCode();
  }
  
  public StructDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.StructModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withRef(Boolean ref) {
    java.util.Objects.requireNonNull((ref));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withPartial(Boolean partial) {
    java.util.Objects.requireNonNull((partial));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withInterfaces(java.util.List<hydra.ext.csharp.syntax.InterfaceType> interfaces) {
    java.util.Objects.requireNonNull((interfaces));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
  
  public StructDeclaration withBody(java.util.List<hydra.ext.csharp.syntax.StructMemberDeclaration> body) {
    java.util.Objects.requireNonNull((body));
    return new StructDeclaration(attributes, modifiers, ref, partial, name, parameters, interfaces, constraints, body);
  }
}