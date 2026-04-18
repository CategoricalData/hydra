// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A switch statement
 */
public class SwitchStatement implements Serializable, Comparable<SwitchStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.SwitchStatement");

  public static final hydra.core.Name DISCRIMINANT = new hydra.core.Name("discriminant");

  public static final hydra.core.Name CASES = new hydra.core.Name("cases");

  public final hydra.javaScript.syntax.Expression discriminant;

  public final java.util.List<hydra.javaScript.syntax.SwitchCase> cases;

  public SwitchStatement (hydra.javaScript.syntax.Expression discriminant, java.util.List<hydra.javaScript.syntax.SwitchCase> cases) {
    this.discriminant = discriminant;
    this.cases = cases;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchStatement)) {
      return false;
    }
    SwitchStatement o = (SwitchStatement) other;
    return java.util.Objects.equals(
      this.discriminant,
      o.discriminant) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(discriminant) + 3 * java.util.Objects.hashCode(cases);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SwitchStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      discriminant,
      other.discriminant);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      cases,
      other.cases);
  }

  public SwitchStatement withDiscriminant(hydra.javaScript.syntax.Expression discriminant) {
    return new SwitchStatement(discriminant, cases);
  }

  public SwitchStatement withCases(java.util.List<hydra.javaScript.syntax.SwitchCase> cases) {
    return new SwitchStatement(discriminant, cases);
  }
}
