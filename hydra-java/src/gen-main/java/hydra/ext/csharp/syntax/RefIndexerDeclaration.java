// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefIndexerDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefIndexerDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATOR = new hydra.core.Name("declarator");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.IndexerModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.RefKind refKind;
  
  public final hydra.ext.csharp.syntax.IndexerDeclarator declarator;
  
  public final hydra.ext.csharp.syntax.RefIndexerBody body;
  
  public RefIndexerDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.IndexerModifier> modifiers, hydra.ext.csharp.syntax.RefKind refKind, hydra.ext.csharp.syntax.IndexerDeclarator declarator, hydra.ext.csharp.syntax.RefIndexerBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((declarator));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.refKind = refKind;
    this.declarator = declarator;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefIndexerDeclaration)) {
      return false;
    }
    RefIndexerDeclaration o = (RefIndexerDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && refKind.equals(o.refKind) && declarator.equals(o.declarator) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * refKind.hashCode() + 7 * declarator.hashCode() + 11 * body.hashCode();
  }
  
  public RefIndexerDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new RefIndexerDeclaration(attributes, modifiers, refKind, declarator, body);
  }
  
  public RefIndexerDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.IndexerModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new RefIndexerDeclaration(attributes, modifiers, refKind, declarator, body);
  }
  
  public RefIndexerDeclaration withRefKind(hydra.ext.csharp.syntax.RefKind refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new RefIndexerDeclaration(attributes, modifiers, refKind, declarator, body);
  }
  
  public RefIndexerDeclaration withDeclarator(hydra.ext.csharp.syntax.IndexerDeclarator declarator) {
    java.util.Objects.requireNonNull((declarator));
    return new RefIndexerDeclaration(attributes, modifiers, refKind, declarator, body);
  }
  
  public RefIndexerDeclaration withBody(hydra.ext.csharp.syntax.RefIndexerBody body) {
    java.util.Objects.requireNonNull((body));
    return new RefIndexerDeclaration(attributes, modifiers, refKind, declarator, body);
  }
}