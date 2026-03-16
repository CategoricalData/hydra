// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class PnLocal_Sequence_Option_ListOfAlts_Elmt implements Serializable, Comparable<PnLocal_Sequence_Option_ListOfAlts_Elmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt");
  
  public static final hydra.core.Name PN_CHARS = new hydra.core.Name("PnChars");
  
  public static final hydra.core.Name PERIOD = new hydra.core.Name("Period");
  
  public static final hydra.core.Name COLON = new hydra.core.Name("Colon");
  
  public static final hydra.core.Name PLX = new hydra.core.Name("Plx");
  
  private PnLocal_Sequence_Option_ListOfAlts_Elmt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnChars instance) ;
    
    R visit(Period instance) ;
    
    R visit(Colon instance) ;
    
    R visit(Plx instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnLocal_Sequence_Option_ListOfAlts_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(PnChars instance) {
      return otherwise(instance);
    }
    
    default R visit(Period instance) {
      return otherwise(instance);
    }
    
    default R visit(Colon instance) {
      return otherwise(instance);
    }
    
    default R visit(Plx instance) {
      return otherwise(instance);
    }
  }
  
  public static final class PnChars extends hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt implements Serializable {
    public final hydra.ext.io.shex.syntax.PnChars value;
    
    public PnChars (hydra.ext.io.shex.syntax.PnChars value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnChars)) {
        return false;
      }
      PnChars o = (PnChars) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnLocal_Sequence_Option_ListOfAlts_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PnChars o = (PnChars) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Period extends hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt implements Serializable {
    public Period () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Period)) {
        return false;
      }
      Period o = (Period) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnLocal_Sequence_Option_ListOfAlts_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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
  
  public static final class Colon extends hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt implements Serializable {
    public Colon () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Colon)) {
        return false;
      }
      Colon o = (Colon) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnLocal_Sequence_Option_ListOfAlts_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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
  
  public static final class Plx extends hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt implements Serializable {
    public final hydra.ext.io.shex.syntax.Plx value;
    
    public Plx (hydra.ext.io.shex.syntax.Plx value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plx)) {
        return false;
      }
      Plx o = (Plx) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnLocal_Sequence_Option_ListOfAlts_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Plx o = (Plx) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
