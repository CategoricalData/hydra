package hydra.ext.java.syntax;

public abstract class SwitchLabel {
  private SwitchLabel () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Constant instance) ;
    
    R visit(EnumConstant instance) ;
    
    R visit(Default instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SwitchLabel instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Constant instance) {
      return otherwise((instance));
    }
    
    default R visit(EnumConstant instance) {
      return otherwise((instance));
    }
    
    default R visit(Default instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Constant extends hydra.ext.java.syntax.SwitchLabel {
    public final hydra.ext.java.syntax.ConstantExpression value;
    
    public Constant (hydra.ext.java.syntax.ConstantExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) (other);
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
  
  public static final class EnumConstant extends hydra.ext.java.syntax.SwitchLabel {
    public final hydra.ext.java.syntax.EnumConstantName value;
    
    public EnumConstant (hydra.ext.java.syntax.EnumConstantName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EnumConstant)) {
        return false;
      }
      EnumConstant o = (EnumConstant) (other);
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
  
  public static final class Default extends hydra.ext.java.syntax.SwitchLabel {
    public Default () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Default)) {
        return false;
      }
      Default o = (Default) (other);
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