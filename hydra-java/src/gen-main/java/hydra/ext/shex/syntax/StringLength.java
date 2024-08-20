// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class StringLength implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLength");
  
  public static final hydra.core.Name FIELD_NAME_L_E_N_G_T_H = new hydra.core.Name("lENGTH");
  
  public static final hydra.core.Name FIELD_NAME_M_I_N_L_E_N_G_T_H = new hydra.core.Name("mINLENGTH");
  
  public static final hydra.core.Name FIELD_NAME_M_A_X_L_E_N_G_T_H = new hydra.core.Name("mAXLENGTH");
  
  private StringLength () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LENGTH instance) ;
    
    R visit(MINLENGTH instance) ;
    
    R visit(MAXLENGTH instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringLength instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LENGTH instance) {
      return otherwise((instance));
    }
    
    default R visit(MINLENGTH instance) {
      return otherwise((instance));
    }
    
    default R visit(MAXLENGTH instance) {
      return otherwise((instance));
    }
  }
  
  public static final class LENGTH extends hydra.ext.shex.syntax.StringLength implements Serializable {
    public LENGTH () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LENGTH)) {
        return false;
      }
      LENGTH o = (LENGTH) (other);
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
  
  public static final class MINLENGTH extends hydra.ext.shex.syntax.StringLength implements Serializable {
    public MINLENGTH () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MINLENGTH)) {
        return false;
      }
      MINLENGTH o = (MINLENGTH) (other);
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
  
  public static final class MAXLENGTH extends hydra.ext.shex.syntax.StringLength implements Serializable {
    public MAXLENGTH () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MAXLENGTH)) {
        return false;
      }
      MAXLENGTH o = (MAXLENGTH) (other);
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
