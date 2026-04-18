// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A template literal (backtick string with interpolations)
 */
public class TemplateLiteral implements Serializable, Comparable<TemplateLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.TemplateLiteral");

  public static final hydra.core.Name QUASIS = new hydra.core.Name("quasis");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  /**
   * The static string parts
   */
  public final java.util.List<hydra.javaScript.syntax.TemplateElement> quasis;

  /**
   * The interpolated expressions
   */
  public final java.util.List<hydra.javaScript.syntax.Expression> expressions;

  public TemplateLiteral (java.util.List<hydra.javaScript.syntax.TemplateElement> quasis, java.util.List<hydra.javaScript.syntax.Expression> expressions) {
    this.quasis = quasis;
    this.expressions = expressions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TemplateLiteral)) {
      return false;
    }
    TemplateLiteral o = (TemplateLiteral) other;
    return java.util.Objects.equals(
      this.quasis,
      o.quasis) && java.util.Objects.equals(
      this.expressions,
      o.expressions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(quasis) + 3 * java.util.Objects.hashCode(expressions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemplateLiteral other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      quasis,
      other.quasis);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expressions,
      other.expressions);
  }

  public TemplateLiteral withQuasis(java.util.List<hydra.javaScript.syntax.TemplateElement> quasis) {
    return new TemplateLiteral(quasis, expressions);
  }

  public TemplateLiteral withExpressions(java.util.List<hydra.javaScript.syntax.Expression> expressions) {
    return new TemplateLiteral(quasis, expressions);
  }
}
