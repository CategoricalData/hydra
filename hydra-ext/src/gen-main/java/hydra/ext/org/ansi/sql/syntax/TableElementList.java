// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class TableElementList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.TableElementList");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.ansi.sql.syntax.TableElement first;
  
  public final java.util.List<hydra.ext.org.ansi.sql.syntax.TableElement> rest;
  
  public TableElementList (hydra.ext.org.ansi.sql.syntax.TableElement first, java.util.List<hydra.ext.org.ansi.sql.syntax.TableElement> rest) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((rest));
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
  
  public TableElementList withFirst(hydra.ext.org.ansi.sql.syntax.TableElement first) {
    java.util.Objects.requireNonNull((first));
    return new TableElementList(first, rest);
  }
  
  public TableElementList withRest(java.util.List<hydra.ext.org.ansi.sql.syntax.TableElement> rest) {
    java.util.Objects.requireNonNull((rest));
    return new TableElementList(first, rest);
  }
}