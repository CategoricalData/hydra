// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A variable declarator (id = init)
 */
public class VariableDeclarator implements Serializable, Comparable<VariableDeclarator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.VariableDeclarator");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public final hydra.javaScript.syntax.Pattern id;

  public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> init;

  public VariableDeclarator (hydra.javaScript.syntax.Pattern id, hydra.util.Maybe<hydra.javaScript.syntax.Expression> init) {
    this.id = id;
    this.init = init;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclarator)) {
      return false;
    }
    VariableDeclarator o = (VariableDeclarator) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.init,
      o.init);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(init);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableDeclarator other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      init,
      other.init);
  }

  public VariableDeclarator withId(hydra.javaScript.syntax.Pattern id) {
    return new VariableDeclarator(id, init);
  }

  public VariableDeclarator withInit(hydra.util.Maybe<hydra.javaScript.syntax.Expression> init) {
    return new VariableDeclarator(id, init);
  }
}
