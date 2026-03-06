// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NumericRange implements Serializable, Comparable<NumericRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericRange");
  
  public static final hydra.core.Name M_I_N_I_N_C_L_U_S_I_V_E = new hydra.core.Name("MININCLUSIVE");
  
  public static final hydra.core.Name M_I_N_E_X_C_L_U_S_I_V_E = new hydra.core.Name("MINEXCLUSIVE");
  
  public static final hydra.core.Name M_A_X_I_N_C_L_U_S_I_V_E = new hydra.core.Name("MAXINCLUSIVE");
  
  public static final hydra.core.Name M_A_X_E_X_C_L_U_S_I_V_E = new hydra.core.Name("MAXEXCLUSIVE");
  
  private NumericRange () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(MININCLUSIVE instance) ;
    
    R visit(MINEXCLUSIVE instance) ;
    
    R visit(MAXINCLUSIVE instance) ;
    
    R visit(MAXEXCLUSIVE instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericRange instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(MININCLUSIVE instance) {
      return otherwise(instance);
    }
    
    default R visit(MINEXCLUSIVE instance) {
      return otherwise(instance);
    }
    
    default R visit(MAXINCLUSIVE instance) {
      return otherwise(instance);
    }
    
    default R visit(MAXEXCLUSIVE instance) {
      return otherwise(instance);
    }
  }
  
  public static final class MININCLUSIVE extends hydra.ext.io.shex.syntax.NumericRange implements Serializable {
    public MININCLUSIVE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MININCLUSIVE)) {
        return false;
      }
      MININCLUSIVE o = (MININCLUSIVE) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NumericRange other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MINEXCLUSIVE extends hydra.ext.io.shex.syntax.NumericRange implements Serializable {
    public MINEXCLUSIVE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MINEXCLUSIVE)) {
        return false;
      }
      MINEXCLUSIVE o = (MINEXCLUSIVE) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NumericRange other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MAXINCLUSIVE extends hydra.ext.io.shex.syntax.NumericRange implements Serializable {
    public MAXINCLUSIVE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MAXINCLUSIVE)) {
        return false;
      }
      MAXINCLUSIVE o = (MAXINCLUSIVE) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NumericRange other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MAXEXCLUSIVE extends hydra.ext.io.shex.syntax.NumericRange implements Serializable {
    public MAXEXCLUSIVE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MAXEXCLUSIVE)) {
        return false;
      }
      MAXEXCLUSIVE o = (MAXEXCLUSIVE) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NumericRange other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
