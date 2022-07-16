package hydra.ext.java.syntax;

public abstract class Statement {
  private Statement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(WithoutTrailing instance) ;
    
    R visit(Labeled instance) ;
    
    R visit(IfThen instance) ;
    
    R visit(IfThenElse instance) ;
    
    R visit(While instance) ;
    
    R visit(For instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Statement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(WithoutTrailing instance) {
      return otherwise((instance));
    }
    
    default R visit(Labeled instance) {
      return otherwise((instance));
    }
    
    default R visit(IfThen instance) {
      return otherwise((instance));
    }
    
    default R visit(IfThenElse instance) {
      return otherwise((instance));
    }
    
    default R visit(While instance) {
      return otherwise((instance));
    }
    
    default R visit(For instance) {
      return otherwise((instance));
    }
  }
  
  public static final class WithoutTrailing extends Statement {
    public final StatementWithoutTrailingSubstatement value;
    
    public WithoutTrailing (StatementWithoutTrailingSubstatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithoutTrailing)) {
        return false;
      }
      WithoutTrailing o = (WithoutTrailing) (other);
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
  
  public static final class Labeled extends Statement {
    public final LabeledStatement value;
    
    public Labeled (LabeledStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Labeled)) {
        return false;
      }
      Labeled o = (Labeled) (other);
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
  
  public static final class IfThen extends Statement {
    public final IfThenStatement value;
    
    public IfThen (IfThenStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IfThen)) {
        return false;
      }
      IfThen o = (IfThen) (other);
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
  
  public static final class IfThenElse extends Statement {
    public final IfThenElseStatement value;
    
    public IfThenElse (IfThenElseStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IfThenElse)) {
        return false;
      }
      IfThenElse o = (IfThenElse) (other);
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
  
  public static final class While extends Statement {
    public final WhileStatement value;
    
    public While (WhileStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof While)) {
        return false;
      }
      While o = (While) (other);
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
  
  public static final class For extends Statement {
    public final ForStatement value;
    
    public For (ForStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) (other);
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