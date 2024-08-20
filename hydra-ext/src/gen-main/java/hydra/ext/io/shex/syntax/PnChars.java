// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class PnChars implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.PnChars");
  
  public static final hydra.core.Name FIELD_NAME_PN_CHARS_U = new hydra.core.Name("pnCharsU");
  
  public static final hydra.core.Name FIELD_NAME_MINUS = new hydra.core.Name("minus");
  
  public static final hydra.core.Name FIELD_NAME_REGEX = new hydra.core.Name("regex");
  
  private PnChars () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnCharsU instance) ;
    
    R visit(Minus instance) ;
    
    R visit(Regex instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnChars instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PnCharsU instance) {
      return otherwise((instance));
    }
    
    default R visit(Minus instance) {
      return otherwise((instance));
    }
    
    default R visit(Regex instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PnCharsU extends hydra.ext.io.shex.syntax.PnChars implements Serializable {
    public final hydra.ext.io.shex.syntax.PnCharsU value;
    
    public PnCharsU (hydra.ext.io.shex.syntax.PnCharsU value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnCharsU)) {
        return false;
      }
      PnCharsU o = (PnCharsU) (other);
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
  
  public static final class Minus extends hydra.ext.io.shex.syntax.PnChars implements Serializable {
    public Minus () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) (other);
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
  
  public static final class Regex extends hydra.ext.io.shex.syntax.PnChars implements Serializable {
    public final String value;
    
    public Regex (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regex)) {
        return false;
      }
      Regex o = (Regex) (other);
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
}