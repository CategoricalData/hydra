// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A class declaration
 */
public class ClassDeclaration implements Serializable, Comparable<ClassDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ClassDeclaration");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name SUPER_CLASS = new hydra.core.Name("superClass");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * Class name
   */
  public final hydra.javaScript.syntax.Identifier id;

  /**
   * Optional superclass
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> superClass;

  /**
   * Class body
   */
  public final java.util.List<hydra.javaScript.syntax.MethodDefinition> body;

  public ClassDeclaration (hydra.javaScript.syntax.Identifier id, hydra.util.Maybe<hydra.javaScript.syntax.Expression> superClass, java.util.List<hydra.javaScript.syntax.MethodDefinition> body) {
    this.id = id;
    this.superClass = superClass;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassDeclaration)) {
      return false;
    }
    ClassDeclaration o = (ClassDeclaration) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.superClass,
      o.superClass) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(superClass) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      superClass,
      other.superClass);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public ClassDeclaration withId(hydra.javaScript.syntax.Identifier id) {
    return new ClassDeclaration(id, superClass, body);
  }

  public ClassDeclaration withSuperClass(hydra.util.Maybe<hydra.javaScript.syntax.Expression> superClass) {
    return new ClassDeclaration(id, superClass, body);
  }

  public ClassDeclaration withBody(java.util.List<hydra.javaScript.syntax.MethodDefinition> body) {
    return new ClassDeclaration(id, superClass, body);
  }
}
