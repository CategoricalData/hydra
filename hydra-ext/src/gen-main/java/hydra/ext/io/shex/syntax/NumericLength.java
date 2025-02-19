// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NumericLength implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericLength");
  
  public static final hydra.core.Name FIELD_NAME_T_O_T_A_L_D_I_G_I_T_S = new hydra.core.Name("tOTALDIGITS");
  
  public static final hydra.core.Name FIELD_NAME_F_R_A_C_T_I_O_N_D_I_G_I_T_S = new hydra.core.Name("fRACTIONDIGITS");
  
  private NumericLength () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(TOTALDIGITS instance) ;
    
    R visit(FRACTIONDIGITS instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericLength instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(TOTALDIGITS instance) {
      return otherwise((instance));
    }
    
    default R visit(FRACTIONDIGITS instance) {
      return otherwise((instance));
    }
  }
  
  public static final class TOTALDIGITS extends hydra.ext.io.shex.syntax.NumericLength implements Serializable {
    public TOTALDIGITS () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TOTALDIGITS)) {
        return false;
      }
      TOTALDIGITS o = (TOTALDIGITS) (other);
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
  
  public static final class FRACTIONDIGITS extends hydra.ext.io.shex.syntax.NumericLength implements Serializable {
    public FRACTIONDIGITS () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FRACTIONDIGITS)) {
        return false;
      }
      FRACTIONDIGITS o = (FRACTIONDIGITS) (other);
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