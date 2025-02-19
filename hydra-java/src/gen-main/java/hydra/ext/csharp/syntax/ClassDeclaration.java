// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ClassDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ClassDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_PARTIAL = new hydra.core.Name("partial");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.csharp.syntax.AttributeSection> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.ClassModifier> modifiers;
  
  public final Boolean partial;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> parameters;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ClassBase> base;
  
  public final java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints;
  
  public final hydra.ext.csharp.syntax.ClassBody body;
  
  public ClassDeclaration (java.util.List<hydra.ext.csharp.syntax.AttributeSection> attributes, java.util.List<hydra.ext.csharp.syntax.ClassModifier> modifiers, Boolean partial, hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> parameters, hydra.util.Opt<hydra.ext.csharp.syntax.ClassBase> base, java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints, hydra.ext.csharp.syntax.ClassBody body) {
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
    if (!(other instanceof ClassDeclaration)) {
      return false;
    }
    ClassDeclaration o = (ClassDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && partial.equals(o.partial) && name.equals(o.name) && parameters.equals(o.parameters) && base.equals(o.base) && constraints.equals(o.constraints) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * partial.hashCode() + 7 * name.hashCode() + 11 * parameters.hashCode() + 13 * base.hashCode() + 17 * constraints.hashCode() + 19 * body.hashCode();
  }
  
  public ClassDeclaration withAttributes(java.util.List<hydra.ext.csharp.syntax.AttributeSection> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.ClassModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withPartial(Boolean partial) {
    java.util.Objects.requireNonNull((partial));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.TypeParameterList> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withBase(hydra.util.Opt<hydra.ext.csharp.syntax.ClassBase> base) {
    java.util.Objects.requireNonNull((base));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withConstraints(java.util.List<hydra.ext.csharp.syntax.TypeParameterConstraintsClause> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
  
  public ClassDeclaration withBody(hydra.ext.csharp.syntax.ClassBody body) {
    java.util.Objects.requireNonNull((body));
    return new ClassDeclaration(attributes, modifiers, partial, name, parameters, base, constraints, body);
  }
}