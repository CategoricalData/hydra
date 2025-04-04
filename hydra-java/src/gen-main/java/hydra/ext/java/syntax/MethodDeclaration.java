// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * Note: simple methods cannot have annotations
   */
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.java.syntax.MethodModifier> modifiers;
  
  public final hydra.ext.java.syntax.MethodHeader header;
  
  public final hydra.ext.java.syntax.MethodBody body;
  
  public MethodDeclaration (java.util.List<hydra.ext.java.syntax.Annotation> annotations, java.util.List<hydra.ext.java.syntax.MethodModifier> modifiers, hydra.ext.java.syntax.MethodHeader header, hydra.ext.java.syntax.MethodBody body) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((body));
    this.annotations = annotations;
    this.modifiers = modifiers;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodDeclaration)) {
      return false;
    }
    MethodDeclaration o = (MethodDeclaration) (other);
    return annotations.equals(o.annotations) && modifiers.equals(o.modifiers) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * modifiers.hashCode() + 5 * header.hashCode() + 7 * body.hashCode();
  }
  
  public MethodDeclaration withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
  
  public MethodDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.MethodModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
  
  public MethodDeclaration withHeader(hydra.ext.java.syntax.MethodHeader header) {
    java.util.Objects.requireNonNull((header));
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
  
  public MethodDeclaration withBody(hydra.ext.java.syntax.MethodBody body) {
    java.util.Objects.requireNonNull((body));
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
}