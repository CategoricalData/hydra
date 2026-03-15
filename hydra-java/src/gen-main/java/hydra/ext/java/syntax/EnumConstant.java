// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumConstant implements Serializable, Comparable<EnumConstant> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.EnumConstant");
  
  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name BODY = new hydra.core.Name("body");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.EnumConstantModifier> modifiers;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> arguments;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> body;
  
  public EnumConstant (hydra.util.ConsList<hydra.ext.java.syntax.EnumConstantModifier> modifiers, hydra.ext.java.syntax.Identifier identifier, hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> arguments, hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.arguments = arguments;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumConstant)) {
      return false;
    }
    EnumConstant o = (EnumConstant) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.arguments,
      o.arguments) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(identifier) + 5 * java.util.Objects.hashCode(arguments) + 7 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EnumConstant other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      arguments.hashCode(),
      other.arguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      body.hashCode(),
      other.body.hashCode());
  }
  
  public EnumConstant withModifiers(hydra.util.ConsList<hydra.ext.java.syntax.EnumConstantModifier> modifiers) {
    return new EnumConstant(modifiers, identifier, arguments, body);
  }
  
  public EnumConstant withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new EnumConstant(modifiers, identifier, arguments, body);
  }
  
  public EnumConstant withArguments(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> arguments) {
    return new EnumConstant(modifiers, identifier, arguments, body);
  }
  
  public EnumConstant withBody(hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> body) {
    return new EnumConstant(modifiers, identifier, arguments, body);
  }
}
