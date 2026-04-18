// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A variable declaration (var, let, const)
 */
public class VariableDeclaration implements Serializable, Comparable<VariableDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.VariableDeclaration");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name DECLARATIONS = new hydra.core.Name("declarations");

  public final hydra.javaScript.syntax.VariableKind kind;

  public final java.util.List<hydra.javaScript.syntax.VariableDeclarator> declarations;

  public VariableDeclaration (hydra.javaScript.syntax.VariableKind kind, java.util.List<hydra.javaScript.syntax.VariableDeclarator> declarations) {
    this.kind = kind;
    this.declarations = declarations;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclaration)) {
      return false;
    }
    VariableDeclaration o = (VariableDeclaration) other;
    return java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.declarations,
      o.declarations);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kind) + 3 * java.util.Objects.hashCode(declarations);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      kind,
      other.kind);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      declarations,
      other.declarations);
  }

  public VariableDeclaration withKind(hydra.javaScript.syntax.VariableKind kind) {
    return new VariableDeclaration(kind, declarations);
  }

  public VariableDeclaration withDeclarations(java.util.List<hydra.javaScript.syntax.VariableDeclarator> declarations) {
    return new VariableDeclaration(kind, declarations);
  }
}
