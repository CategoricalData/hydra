// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class AnnotationTypeElementDeclaration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.AnnotationTypeElementDeclaration");
  
  public final java.util.List<hydra.langs.java.syntax.AnnotationTypeElementModifier> modifiers;
  
  public final hydra.langs.java.syntax.UnannType type;
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Dims> dims;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.DefaultValue> default_;
  
  public AnnotationTypeElementDeclaration (java.util.List<hydra.langs.java.syntax.AnnotationTypeElementModifier> modifiers, hydra.langs.java.syntax.UnannType type, hydra.langs.java.syntax.Identifier identifier, hydra.util.Opt<hydra.langs.java.syntax.Dims> dims, hydra.util.Opt<hydra.langs.java.syntax.DefaultValue> default_) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((dims));
    java.util.Objects.requireNonNull((default_));
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
    AnnotationTypeElementDeclaration o = (AnnotationTypeElementDeclaration) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && identifier.equals(o.identifier) && dims.equals(o.dims) && default_.equals(o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * identifier.hashCode() + 7 * dims.hashCode() + 11 * default_.hashCode();
  }
  
  public AnnotationTypeElementDeclaration withModifiers(java.util.List<hydra.langs.java.syntax.AnnotationTypeElementModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withType(hydra.langs.java.syntax.UnannType type) {
    java.util.Objects.requireNonNull((type));
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withIdentifier(hydra.langs.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withDims(hydra.util.Opt<hydra.langs.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((dims));
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withDefault(hydra.util.Opt<hydra.langs.java.syntax.DefaultValue> default_) {
    java.util.Objects.requireNonNull((default_));
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
}