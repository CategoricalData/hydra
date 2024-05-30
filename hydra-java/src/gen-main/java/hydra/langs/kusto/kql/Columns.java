package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class Columns implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Columns");
  
  private Columns () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(All instance) ;
    
    R visit(Single instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Columns instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(All instance) {
      return otherwise((instance));
    }
    
    default R visit(Single instance) {
      return otherwise((instance));
    }
  }
  
  public static final class All extends hydra.langs.kusto.kql.Columns implements Serializable {
    public All () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Single extends hydra.langs.kusto.kql.Columns implements Serializable {
    public final hydra.langs.kusto.kql.ColumnName value;
    
    public Single (hydra.langs.kusto.kql.ColumnName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}