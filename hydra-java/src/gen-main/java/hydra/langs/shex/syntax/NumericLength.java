package hydra.langs.shex.syntax;

public abstract class NumericLength {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.NumericLength");
  
  private NumericLength () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(TOTALDIGITS instance) ;
    
    R visit(FRACTIONDIGITS instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericLength instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(TOTALDIGITS instance) {
      return otherwise((instance));
    }
    
    default R visit(FRACTIONDIGITS instance) {
      return otherwise((instance));
    }
  }
  
  public static final class TOTALDIGITS extends hydra.langs.shex.syntax.NumericLength {
    public TOTALDIGITS () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TOTALDIGITS)) {
        return false;
      }
      TOTALDIGITS o = (TOTALDIGITS) (other);
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
  
  public static final class FRACTIONDIGITS extends hydra.langs.shex.syntax.NumericLength {
    public FRACTIONDIGITS () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FRACTIONDIGITS)) {
        return false;
      }
      FRACTIONDIGITS o = (FRACTIONDIGITS) (other);
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