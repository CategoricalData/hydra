package hydra.ext.sql.ansi;

public class InsertStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.InsertStatement");
  
  public final hydra.ext.sql.ansi.InsertionTarget target;
  
  public final hydra.ext.sql.ansi.InsertColumnsAndSource columnsAndSource;
  
  public InsertStatement (hydra.ext.sql.ansi.InsertionTarget target, hydra.ext.sql.ansi.InsertColumnsAndSource columnsAndSource) {
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
  
  public InsertStatement withTarget(hydra.ext.sql.ansi.InsertionTarget target) {
    return new InsertStatement(target, columnsAndSource);
  }
  
  public InsertStatement withColumnsAndSource(hydra.ext.sql.ansi.InsertColumnsAndSource columnsAndSource) {
    return new InsertStatement(target, columnsAndSource);
  }
}