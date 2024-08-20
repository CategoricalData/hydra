// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class OverrideClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.OverrideClause");
  
  public static final hydra.core.Name FIELD_NAME_O_V_E_R_R_I_D_I_N_G_SP_U_S_E_R_SP_V_A_L_U_E = new hydra.core.Name("oVERRIDINGSpUSERSpVALUE");
  
  public static final hydra.core.Name FIELD_NAME_O_V_E_R_R_I_D_I_N_G_SP_S_Y_S_T_E_M_SP_V_A_L_U_E = new hydra.core.Name("oVERRIDINGSpSYSTEMSpVALUE");
  
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
  
  public static final class OVERRIDINGSpUSERSpVALUE extends hydra.ext.sql.ansi.OverrideClause implements Serializable {
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
  
  public static final class OVERRIDINGSpSYSTEMSpVALUE extends hydra.ext.sql.ansi.OverrideClause implements Serializable {
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