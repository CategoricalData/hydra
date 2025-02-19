// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class GlobalOrLocal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.GlobalOrLocal");
  
  public static final hydra.core.Name FIELD_NAME_GLOBAL = new hydra.core.Name("global");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL = new hydra.core.Name("local");
  
  private GlobalOrLocal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Global instance) ;
    
    R visit(Local instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GlobalOrLocal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Global instance) {
      return otherwise((instance));
    }
    
    default R visit(Local instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Global extends hydra.ext.org.ansi.sql.syntax.GlobalOrLocal implements Serializable {
    public Global () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Global)) {
        return false;
      }
      Global o = (Global) (other);
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
  
  public static final class Local extends hydra.ext.org.ansi.sql.syntax.GlobalOrLocal implements Serializable {
    public Local () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) (other);
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
}