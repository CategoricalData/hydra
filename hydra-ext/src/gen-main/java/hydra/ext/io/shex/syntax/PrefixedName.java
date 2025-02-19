// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class PrefixedName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.PrefixedName");
  
  public static final hydra.core.Name FIELD_NAME_PNAME_LN = new hydra.core.Name("pnameLn");
  
  public static final hydra.core.Name FIELD_NAME_PNAME_NS = new hydra.core.Name("pnameNs");
  
  private PrefixedName () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnameLn instance) ;
    
    R visit(PnameNs instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrefixedName instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PnameLn instance) {
      return otherwise((instance));
    }
    
    default R visit(PnameNs instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PnameLn extends hydra.ext.io.shex.syntax.PrefixedName implements Serializable {
    public final hydra.ext.io.shex.syntax.PnameLn value;
    
    public PnameLn (hydra.ext.io.shex.syntax.PnameLn value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnameLn)) {
        return false;
      }
      PnameLn o = (PnameLn) (other);
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
  
  public static final class PnameNs extends hydra.ext.io.shex.syntax.PrefixedName implements Serializable {
    public final hydra.ext.io.shex.syntax.PnameNs value;
    
    public PnameNs (hydra.ext.io.shex.syntax.PnameNs value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnameNs)) {
        return false;
      }
      PnameNs o = (PnameNs) (other);
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