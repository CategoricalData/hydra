// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AssertStatement_Pair implements Serializable, Comparable<AssertStatement_Pair> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.AssertStatement_Pair");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_SECOND = new hydra.core.Name("second");
  
  public final hydra.ext.java.syntax.Expression first;
  
  public final hydra.ext.java.syntax.Expression second;
  
  public AssertStatement_Pair (hydra.ext.java.syntax.Expression first, hydra.ext.java.syntax.Expression second) {
    this.first = first;
    this.second = second;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssertStatement_Pair)) {
      return false;
    }
    AssertStatement_Pair o = (AssertStatement_Pair) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.second,
      o.second);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(second);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AssertStatement_Pair other) {
    int cmp = 0;
    cmp = ((Comparable) first).compareTo(other.first);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) second).compareTo(other.second);
  }
  
  public AssertStatement_Pair withFirst(hydra.ext.java.syntax.Expression first) {
    return new AssertStatement_Pair(first, second);
  }
  
  public AssertStatement_Pair withSecond(hydra.ext.java.syntax.Expression second) {
    return new AssertStatement_Pair(first, second);
  }
}
