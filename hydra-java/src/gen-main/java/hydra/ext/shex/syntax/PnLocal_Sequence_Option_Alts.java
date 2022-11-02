package hydra.ext.shex.syntax;

public abstract class PnLocal_Sequence_Option_Alts {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnLocal.Sequence.Option.Alts");
  
  private PnLocal_Sequence_Option_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnChars instance) ;
    
    R visit(Colon instance) ;
    
    R visit(Plx instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnLocal_Sequence_Option_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PnChars instance) {
      return otherwise((instance));
    }
    
    default R visit(Colon instance) {
      return otherwise((instance));
    }
    
    default R visit(Plx instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PnChars extends hydra.ext.shex.syntax.PnLocal_Sequence_Option_Alts {
    public final hydra.ext.shex.syntax.PnChars value;
    
    public PnChars (hydra.ext.shex.syntax.PnChars value) {
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
  
  public static final class Colon extends hydra.ext.shex.syntax.PnLocal_Sequence_Option_Alts {
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
  
  public static final class Plx extends hydra.ext.shex.syntax.PnLocal_Sequence_Option_Alts {
    public final hydra.ext.shex.syntax.Plx value;
    
    public Plx (hydra.ext.shex.syntax.Plx value) {
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