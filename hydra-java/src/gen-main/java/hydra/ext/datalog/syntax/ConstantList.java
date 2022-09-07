package hydra.ext.datalog.syntax;

public abstract class ConstantList {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.ConstantList");
  
  private ConstantList () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Single instance) ;
    
    R visit(Multiple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConstantList instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Single instance) {
      return otherwise((instance));
    }
    
    default R visit(Multiple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Single extends hydra.ext.datalog.syntax.ConstantList {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.Single");
    
    public final hydra.ext.datalog.syntax.Constant value;
    
    public Single (hydra.ext.datalog.syntax.Constant value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) (other);
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
  
  public static final class Multiple extends hydra.ext.datalog.syntax.ConstantList {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.Multiple");
    
    public final hydra.ext.datalog.syntax.ConstantList_Multiple value;
    
    public Multiple (hydra.ext.datalog.syntax.ConstantList_Multiple value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiple)) {
        return false;
      }
      Multiple o = (Multiple) (other);
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