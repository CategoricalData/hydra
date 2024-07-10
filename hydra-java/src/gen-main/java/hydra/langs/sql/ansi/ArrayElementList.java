// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class ArrayElementList implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.ArrayElementList");
  
  public final hydra.langs.sql.ansi.ArrayElement first;
  
  public final java.util.List<hydra.langs.sql.ansi.ArrayElement> rest;
  
  public ArrayElementList (hydra.langs.sql.ansi.ArrayElement first, java.util.List<hydra.langs.sql.ansi.ArrayElement> rest) {
    if (first == null) {
      throw new IllegalArgumentException("null value for 'first' argument");
    }
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
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
  
  public ArrayElementList withFirst(hydra.langs.sql.ansi.ArrayElement first) {
    if (first == null) {
      throw new IllegalArgumentException("null value for 'first' argument");
    }
    return new ArrayElementList(first, rest);
  }
  
  public ArrayElementList withRest(java.util.List<hydra.langs.sql.ansi.ArrayElement> rest) {
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
    return new ArrayElementList(first, rest);
  }
}