// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A labeled statement
 */
public class LabeledStatement implements Serializable, Comparable<LabeledStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.LabeledStatement");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.javaScript.syntax.Identifier label;

  public final hydra.javaScript.syntax.Statement body;

  public LabeledStatement (hydra.javaScript.syntax.Identifier label, hydra.javaScript.syntax.Statement body) {
    this.label = label;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledStatement)) {
      return false;
    }
    LabeledStatement o = (LabeledStatement) other;
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabeledStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      label,
      other.label);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public LabeledStatement withLabel(hydra.javaScript.syntax.Identifier label) {
    return new LabeledStatement(label, body);
  }

  public LabeledStatement withBody(hydra.javaScript.syntax.Statement body) {
    return new LabeledStatement(label, body);
  }
}
