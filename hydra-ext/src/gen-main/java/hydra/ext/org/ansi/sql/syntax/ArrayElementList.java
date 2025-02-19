// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class ArrayElementList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.ArrayElementList");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.ansi.sql.syntax.ArrayElement first;
  
  public final java.util.List<hydra.ext.org.ansi.sql.syntax.ArrayElement> rest;
  
  public ArrayElementList (hydra.ext.org.ansi.sql.syntax.ArrayElement first, java.util.List<hydra.ext.org.ansi.sql.syntax.ArrayElement> rest) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((rest));
    this.first = first;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayElementList)) {
      return false;
    }
    ArrayElementList o = (ArrayElementList) (other);
    return first.equals(o.first) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * rest.hashCode();
  }
  
  public ArrayElementList withFirst(hydra.ext.org.ansi.sql.syntax.ArrayElement first) {
    java.util.Objects.requireNonNull((first));
    return new ArrayElementList(first, rest);
  }
  
  public ArrayElementList withRest(java.util.List<hydra.ext.org.ansi.sql.syntax.ArrayElement> rest) {
    java.util.Objects.requireNonNull((rest));
    return new ArrayElementList(first, rest);
  }
}