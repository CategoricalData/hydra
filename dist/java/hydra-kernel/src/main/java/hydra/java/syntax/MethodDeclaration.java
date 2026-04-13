// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class MethodDeclaration implements Serializable, Comparable<MethodDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.MethodDeclaration");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name HEADER = new hydra.core.Name("header");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * Note: simple methods cannot have annotations
   */
  public final java.util.List<hydra.java.syntax.Annotation> annotations;

  public final java.util.List<hydra.java.syntax.MethodModifier> modifiers;

  public final hydra.java.syntax.MethodHeader header;

  public final hydra.java.syntax.MethodBody body;

  public MethodDeclaration (java.util.List<hydra.java.syntax.Annotation> annotations, java.util.List<hydra.java.syntax.MethodModifier> modifiers, hydra.java.syntax.MethodHeader header, hydra.java.syntax.MethodBody body) {
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
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      modifiers,
      other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      header,
      other.header);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public MethodDeclaration withAnnotations(java.util.List<hydra.java.syntax.Annotation> annotations) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }

  public MethodDeclaration withModifiers(java.util.List<hydra.java.syntax.MethodModifier> modifiers) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }

  public MethodDeclaration withHeader(hydra.java.syntax.MethodHeader header) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }

  public MethodDeclaration withBody(hydra.java.syntax.MethodBody body) {
    return new MethodDeclaration(annotations, modifiers, header, body);
  }
}
