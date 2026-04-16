// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ElementVariableDeclaration implements Serializable, Comparable<ElementVariableDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ElementVariableDeclaration");

  public static final hydra.core.Name TEMP = new hydra.core.Name("temp");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public final hydra.util.Maybe<Boolean> temp;

  public final String variable;

  public ElementVariableDeclaration (hydra.util.Maybe<Boolean> temp, String variable) {
    this.temp = temp;
    this.variable = variable;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementVariableDeclaration)) {
      return false;
    }
    ElementVariableDeclaration o = (ElementVariableDeclaration) other;
    return java.util.Objects.equals(
      this.temp,
      o.temp) && java.util.Objects.equals(
      this.variable,
      o.variable);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(temp) + 3 * java.util.Objects.hashCode(variable);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ElementVariableDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      temp,
      other.temp);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variable,
      other.variable);
  }

  public ElementVariableDeclaration withTemp(hydra.util.Maybe<Boolean> temp) {
    return new ElementVariableDeclaration(temp, variable);
  }

  public ElementVariableDeclaration withVariable(String variable) {
    return new ElementVariableDeclaration(temp, variable);
  }
}
