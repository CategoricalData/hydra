// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class InsertStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.InsertStatement");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS_AND_SOURCE = new hydra.core.Name("columnsAndSource");
  
  public final hydra.ext.org.ansi.sql.syntax.InsertionTarget target;
  
  public final hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource columnsAndSource;
  
  public InsertStatement (hydra.ext.org.ansi.sql.syntax.InsertionTarget target, hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource columnsAndSource) {
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((columnsAndSource));
    this.target = target;
    this.columnsAndSource = columnsAndSource;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InsertStatement)) {
      return false;
    }
    InsertStatement o = (InsertStatement) (other);
    return target.equals(o.target) && columnsAndSource.equals(o.columnsAndSource);
  }
  
  @Override
  public int hashCode() {
    return 2 * target.hashCode() + 3 * columnsAndSource.hashCode();
  }
  
  public InsertStatement withTarget(hydra.ext.org.ansi.sql.syntax.InsertionTarget target) {
    java.util.Objects.requireNonNull((target));
    return new InsertStatement(target, columnsAndSource);
  }
  
  public InsertStatement withColumnsAndSource(hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource columnsAndSource) {
    java.util.Objects.requireNonNull((columnsAndSource));
    return new InsertStatement(target, columnsAndSource);
  }
}