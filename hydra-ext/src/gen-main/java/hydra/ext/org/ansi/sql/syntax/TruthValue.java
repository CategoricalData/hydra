// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class TruthValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.TruthValue");
  
  public static final hydra.core.Name FIELD_NAME_T_R_U_E = new hydra.core.Name("tRUE");
  
  public static final hydra.core.Name FIELD_NAME_F_A_L_S_E = new hydra.core.Name("fALSE");
  
  public static final hydra.core.Name FIELD_NAME_U_N_K_N_O_W_N = new hydra.core.Name("uNKNOWN");
  
  private TruthValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(TRUE instance) ;
    
    R visit(FALSE instance) ;
    
    R visit(UNKNOWN instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TruthValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(TRUE instance) {
      return otherwise((instance));
    }
    
    default R visit(FALSE instance) {
      return otherwise((instance));
    }
    
    default R visit(UNKNOWN instance) {
      return otherwise((instance));
    }
  }
  
  public static final class TRUE extends hydra.ext.org.ansi.sql.syntax.TruthValue implements Serializable {
    public TRUE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TRUE)) {
        return false;
      }
      TRUE o = (TRUE) (other);
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
  
  public static final class FALSE extends hydra.ext.org.ansi.sql.syntax.TruthValue implements Serializable {
    public FALSE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FALSE)) {
        return false;
      }
      FALSE o = (FALSE) (other);
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
  
  public static final class UNKNOWN extends hydra.ext.org.ansi.sql.syntax.TruthValue implements Serializable {
    public UNKNOWN () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UNKNOWN)) {
        return false;
      }
      UNKNOWN o = (UNKNOWN) (other);
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