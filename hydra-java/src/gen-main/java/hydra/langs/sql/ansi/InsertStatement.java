// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class InsertStatement implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.InsertStatement");
  
  public final hydra.langs.sql.ansi.InsertionTarget target;
  
  public final hydra.langs.sql.ansi.InsertColumnsAndSource columnsAndSource;
  
  public InsertStatement (hydra.langs.sql.ansi.InsertionTarget target, hydra.langs.sql.ansi.InsertColumnsAndSource columnsAndSource) {
    if (target == null) {
      throw new IllegalArgumentException("null value for 'target' argument");
    }
    if (columnsAndSource == null) {
      throw new IllegalArgumentException("null value for 'columnsAndSource' argument");
    }
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
  
  public InsertStatement withTarget(hydra.langs.sql.ansi.InsertionTarget target) {
    if (target == null) {
      throw new IllegalArgumentException("null value for 'target' argument");
    }
    return new InsertStatement(target, columnsAndSource);
  }
  
  public InsertStatement withColumnsAndSource(hydra.langs.sql.ansi.InsertColumnsAndSource columnsAndSource) {
    if (columnsAndSource == null) {
      throw new IllegalArgumentException("null value for 'columnsAndSource' argument");
    }
    return new InsertStatement(target, columnsAndSource);
  }
}