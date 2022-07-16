package hydra.ext.java.syntax;

public class AnnotationTypeElementDeclaration {
  public final java.util.List<AnnotationTypeElementModifier> modifiers;
  
  public final UnannType type;
  
  public final Identifier identifier;
  
  public final java.util.Optional<Dims> dims;
  
  public final java.util.Optional<DefaultValue> default_;
  
  public AnnotationTypeElementDeclaration (java.util.List<AnnotationTypeElementModifier> modifiers, UnannType type, Identifier identifier, java.util.Optional<Dims> dims, java.util.Optional<DefaultValue> default_) {
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
  
  public AnnotationTypeElementDeclaration withModifiers(java.util.List<AnnotationTypeElementModifier> modifiers) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withType(UnannType type) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withIdentifier(Identifier identifier) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withDims(java.util.Optional<Dims> dims) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
  
  public AnnotationTypeElementDeclaration withDefault(java.util.Optional<DefaultValue> default_) {
    return new AnnotationTypeElementDeclaration(modifiers, type, identifier, dims, default_);
  }
}