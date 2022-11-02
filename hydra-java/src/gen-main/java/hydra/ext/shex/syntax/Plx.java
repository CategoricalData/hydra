package hydra.ext.shex.syntax;

public abstract class Plx {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Plx");
  
  private Plx () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Percent instance) ;
    
    R visit(PnLocalEsc instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Plx instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Percent instance) {
      return otherwise((instance));
    }
    
    default R visit(PnLocalEsc instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Percent extends hydra.ext.shex.syntax.Plx {
    public final hydra.ext.shex.syntax.Percent value;
    
    public Percent (hydra.ext.shex.syntax.Percent value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Percent)) {
        return false;
      }
      Percent o = (Percent) (other);
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
  
  public static final class PnLocalEsc extends hydra.ext.shex.syntax.Plx {
    public final hydra.ext.shex.syntax.PnLocalEsc value;
    
    public PnLocalEsc (hydra.ext.shex.syntax.PnLocalEsc value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnLocalEsc)) {
        return false;
      }
      PnLocalEsc o = (PnLocalEsc) (other);
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