// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class AssertStatement_Pair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.AssertStatement.Pair");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_SECOND = new hydra.core.Name("second");
  
  public final hydra.langs.java.syntax.Expression first;
  
  public final hydra.langs.java.syntax.Expression second;
  
  public AssertStatement_Pair (hydra.langs.java.syntax.Expression first, hydra.langs.java.syntax.Expression second) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((second));
    this.first = first;
    this.second = second;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssertStatement_Pair)) {
      return false;
    }
    AssertStatement_Pair o = (AssertStatement_Pair) (other);
    return first.equals(o.first) && second.equals(o.second);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * second.hashCode();
  }
  
  public AssertStatement_Pair withFirst(hydra.langs.java.syntax.Expression first) {
    java.util.Objects.requireNonNull((first));
    return new AssertStatement_Pair(first, second);
  }
  
  public AssertStatement_Pair withSecond(hydra.langs.java.syntax.Expression second) {
    java.util.Objects.requireNonNull((second));
    return new AssertStatement_Pair(first, second);
  }
}