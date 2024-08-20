// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class PnLocal_Alts implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnLocal.Alts");
  
  public static final hydra.core.Name FIELD_NAME_PN_CHARS_U = new hydra.core.Name("pnCharsU");
  
  public static final hydra.core.Name FIELD_NAME_COLON = new hydra.core.Name("colon");
  
  public static final hydra.core.Name FIELD_NAME_REGEX = new hydra.core.Name("regex");
  
  public static final hydra.core.Name FIELD_NAME_PLX = new hydra.core.Name("plx");
  
  private PnLocal_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnCharsU instance) ;
    
    R visit(Colon instance) ;
    
    R visit(Regex instance) ;
    
    R visit(Plx instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnLocal_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PnCharsU instance) {
      return otherwise((instance));
    }
    
    default R visit(Colon instance) {
      return otherwise((instance));
    }
    
    default R visit(Regex instance) {
      return otherwise((instance));
    }
    
    default R visit(Plx instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PnCharsU extends hydra.ext.shex.syntax.PnLocal_Alts implements Serializable {
    public final hydra.ext.shex.syntax.PnCharsU value;
    
    public PnCharsU (hydra.ext.shex.syntax.PnCharsU value) {
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
  
  public static final class Colon extends hydra.ext.shex.syntax.PnLocal_Alts implements Serializable {
    public Colon () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Colon)) {
        return false;
      }
      Colon o = (Colon) (other);
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
  
  public static final class Regex extends hydra.ext.shex.syntax.PnLocal_Alts implements Serializable {
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
  
  public static final class Plx extends hydra.ext.shex.syntax.PnLocal_Alts implements Serializable {
    public final hydra.ext.shex.syntax.Plx value;
    
    public Plx (hydra.ext.shex.syntax.Plx value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plx)) {
        return false;
      }
      Plx o = (Plx) (other);
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
