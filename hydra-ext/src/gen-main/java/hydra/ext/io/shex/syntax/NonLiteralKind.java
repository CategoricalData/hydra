// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NonLiteralKind implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.NonLiteralKind");
  
  public static final hydra.core.Name FIELD_NAME_I_R_I = new hydra.core.Name("iRI");
  
  public static final hydra.core.Name FIELD_NAME_B_N_O_D_E = new hydra.core.Name("bNODE");
  
  public static final hydra.core.Name FIELD_NAME_N_O_N_L_I_T_E_R_A_L = new hydra.core.Name("nONLITERAL");
  
  private NonLiteralKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(IRI instance) ;
    
    R visit(BNODE instance) ;
    
    R visit(NONLITERAL instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonLiteralKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(IRI instance) {
      return otherwise((instance));
    }
    
    default R visit(BNODE instance) {
      return otherwise((instance));
    }
    
    default R visit(NONLITERAL instance) {
      return otherwise((instance));
    }
  }
  
  public static final class IRI extends hydra.ext.io.shex.syntax.NonLiteralKind implements Serializable {
    public IRI () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IRI)) {
        return false;
      }
      IRI o = (IRI) (other);
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
  
  public static final class BNODE extends hydra.ext.io.shex.syntax.NonLiteralKind implements Serializable {
    public BNODE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BNODE)) {
        return false;
      }
      BNODE o = (BNODE) (other);
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
  
  public static final class NONLITERAL extends hydra.ext.io.shex.syntax.NonLiteralKind implements Serializable {
    public NONLITERAL () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NONLITERAL)) {
        return false;
      }
      NONLITERAL o = (NONLITERAL) (other);
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