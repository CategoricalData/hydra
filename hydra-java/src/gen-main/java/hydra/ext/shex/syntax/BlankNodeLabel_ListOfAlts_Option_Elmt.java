package hydra.ext.shex.syntax;

public abstract class BlankNodeLabel_ListOfAlts_Option_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.BlankNodeLabel.ListOfAlts.Option.Elmt");
  
  private BlankNodeLabel_ListOfAlts_Option_Elmt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PnChars instance) ;
    
    R visit(Period instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BlankNodeLabel_ListOfAlts_Option_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PnChars instance) {
      return otherwise((instance));
    }
    
    default R visit(Period instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PnChars extends hydra.ext.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt {
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
  
  public static final class Period extends hydra.ext.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt {
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