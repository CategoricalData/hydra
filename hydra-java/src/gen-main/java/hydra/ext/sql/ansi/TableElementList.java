package hydra.ext.sql.ansi;

public class TableElementList {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.TableElementList");
  
  public final hydra.ext.sql.ansi.TableElement first;
  
  public final java.util.List<hydra.ext.sql.ansi.TableElement> rest;
  
  public TableElementList (hydra.ext.sql.ansi.TableElement first, java.util.List<hydra.ext.sql.ansi.TableElement> rest) {
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
  
  public TableElementList withFirst(hydra.ext.sql.ansi.TableElement first) {
    return new TableElementList(first, rest);
  }
  
  public TableElementList withRest(java.util.List<hydra.ext.sql.ansi.TableElement> rest) {
    return new TableElementList(first, rest);
  }
}