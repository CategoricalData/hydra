// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodDeclaration implements Serializable, Comparable<MethodDeclaration> {
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
    MethodDeclaration o = (MethodDeclaration) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.header,
      o.header) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(modifiers) + 5 * java.util.Objects.hashCode(header) + 7 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodDeclaration other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) header).compareTo(other.header);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public MethodDeclaration withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
  
  public MethodDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.MethodModifier> modifiers) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
  
  public MethodDeclaration withHeader(hydra.ext.java.syntax.MethodHeader header) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
  
  public MethodDeclaration withBody(hydra.ext.java.syntax.MethodBody body) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
}
