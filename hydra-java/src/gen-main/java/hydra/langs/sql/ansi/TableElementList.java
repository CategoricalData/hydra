// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class TableElementList implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.TableElementList");
  
  public final hydra.langs.sql.ansi.TableElement first;
  
  public final java.util.List<hydra.langs.sql.ansi.TableElement> rest;
  
  public TableElementList (hydra.langs.sql.ansi.TableElement first, java.util.List<hydra.langs.sql.ansi.TableElement> rest) {
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
    if (!(other instanceof TableElementList)) {
      return false;
    }
    TableElementList o = (TableElementList) (other);
    return first.equals(o.first) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * rest.hashCode();
  }
  
  public TableElementList withFirst(hydra.langs.sql.ansi.TableElement first) {
    if (first == null) {
      throw new IllegalArgumentException("null value for 'first' argument");
    }
    return new TableElementList(first, rest);
  }
  
  public TableElementList withRest(java.util.List<hydra.langs.sql.ansi.TableElement> rest) {
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
    return new TableElementList(first, rest);
  }
}