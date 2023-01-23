package hydra.ext.sql.ansi;

public abstract class OverrideClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.OverrideClause");
  
  private OverrideClause () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(OVERRIDINGSpUSERSpVALUE instance) ;
    
    R visit(OVERRIDINGSpSYSTEMSpVALUE instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OverrideClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(OVERRIDINGSpUSERSpVALUE instance) {
      return otherwise((instance));
    }
    
    default R visit(OVERRIDINGSpSYSTEMSpVALUE instance) {
      return otherwise((instance));
    }
  }
  
  public static final class OVERRIDINGSpUSERSpVALUE extends hydra.ext.sql.ansi.OverrideClause {
    public OVERRIDINGSpUSERSpVALUE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OVERRIDINGSpUSERSpVALUE)) {
        return false;
      }
      OVERRIDINGSpUSERSpVALUE o = (OVERRIDINGSpUSERSpVALUE) (other);
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
  
  public static final class OVERRIDINGSpSYSTEMSpVALUE extends hydra.ext.sql.ansi.OverrideClause {
    public OVERRIDINGSpSYSTEMSpVALUE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OVERRIDINGSpSYSTEMSpVALUE)) {
        return false;
      }
      OVERRIDINGSpSYSTEMSpVALUE o = (OVERRIDINGSpSYSTEMSpVALUE) (other);
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