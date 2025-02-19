// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class PnPrefix_Sequence_Option_Alts implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts");
  
  public static final hydra.core.Name FIELD_NAME_PN_CHARS = new hydra.core.Name("pnChars");
  
  public static final hydra.core.Name FIELD_NAME_PERIOD = new hydra.core.Name("period");
  
  private PnPrefix_Sequence_Option_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnChars instance) ;
    
    R visit(Period instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnPrefix_Sequence_Option_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PnChars instance) {
      return otherwise((instance));
    }
    
    default R visit(Period instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PnChars extends hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.PnChars value;
    
    public PnChars (hydra.ext.io.shex.syntax.PnChars value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnChars)) {
        return false;
      }
      PnChars o = (PnChars) (other);
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
  
  public static final class Period extends hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts implements Serializable {
    public Period () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Period)) {
        return false;
      }
      Period o = (Period) (other);
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