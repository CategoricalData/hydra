package hydra.ext.shex.syntax;

public abstract class PnCharsBase {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnCharsBase");
  
  private PnCharsBase () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Regex instance) ;
    
    R visit(Regex2 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnCharsBase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Regex instance) {
      return otherwise((instance));
    }
    
    default R visit(Regex2 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Regex extends hydra.ext.shex.syntax.PnCharsBase {
    public final String value;
    
    public Regex (String value) {
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
  
  public static final class Regex2 extends hydra.ext.shex.syntax.PnCharsBase {
    public final String value;
    
    public Regex2 (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regex2)) {
        return false;
      }
      Regex2 o = (Regex2) (other);
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