// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AnnotationTypeElementDeclaration implements Serializable, Comparable<AnnotationTypeElementDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeElementDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final java.util.List<hydra.ext.java.syntax.AnnotationTypeElementModifier> modifiers;
  
  public final hydra.ext.java.syntax.UnannType type;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.DefaultValue> default_;
  
  public AnnotationTypeElementDeclaration (java.util.List<hydra.ext.java.syntax.AnnotationTypeElementModifier> modifiers, hydra.ext.java.syntax.UnannType type, hydra.ext.java.syntax.Identifier identifier, hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims, hydra.util.Maybe<hydra.ext.java.syntax.DefaultValue> default_) {
    this.modifiers = modifiers;
    this.type = type;
    this.identifier = identifier;
    this.dims = dims;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationTypeElementDeclaration)) {
      return false;
    }
    AnnotationTypeElementDeclaration o = (AnnotationTypeElementDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.dims,
      o.dims) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(identifier) + 7 * java.util.Objects.hashCode(dims) + 11 * java.util.Objects.hashCode(default_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotationTypeElementDeclaration other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      dims.hashCode(),
      other.dims.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      default_.hashCode(),
      other.default_.hashCode());
  }
  
  public AnnotationTypeElementDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.AnnotationTypeElementModifier> modifiers) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withType(hydra.ext.java.syntax.UnannType type) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withDims(hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withDefault(hydra.util.Maybe<hydra.ext.java.syntax.DefaultValue> default_) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
}
