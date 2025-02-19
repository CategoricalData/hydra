// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StandardIndexerDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StandardIndexerDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATOR = new hydra.core.Name("declarator");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.IndexerModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.IndexerDeclarator declarator;
  
  public final hydra.ext.csharp.syntax.IndexerBody body;
  
  public StandardIndexerDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.IndexerModifier> modifiers, hydra.ext.csharp.syntax.IndexerDeclarator declarator, hydra.ext.csharp.syntax.IndexerBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((declarator));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.declarator = declarator;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandardIndexerDeclaration)) {
      return false;
    }
    StandardIndexerDeclaration o = (StandardIndexerDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && declarator.equals(o.declarator) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * declarator.hashCode() + 7 * body.hashCode();
  }
  
  public StandardIndexerDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new StandardIndexerDeclaration(attributes, modifiers, declarator, body);
  }
  
  public StandardIndexerDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.IndexerModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new StandardIndexerDeclaration(attributes, modifiers, declarator, body);
  }
  
  public StandardIndexerDeclaration withDeclarator(hydra.ext.csharp.syntax.IndexerDeclarator declarator) {
    java.util.Objects.requireNonNull((declarator));
    return new StandardIndexerDeclaration(attributes, modifiers, declarator, body);
  }
  
  public StandardIndexerDeclaration withBody(hydra.ext.csharp.syntax.IndexerBody body) {
    java.util.Objects.requireNonNull((body));
    return new StandardIndexerDeclaration(attributes, modifiers, declarator, body);
  }
}