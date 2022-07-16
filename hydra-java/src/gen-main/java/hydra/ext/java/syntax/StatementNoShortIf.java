package hydra.ext.java.syntax;

public abstract class StatementNoShortIf {
  private StatementNoShortIf () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(WithoutTrailing instance) ;
    
    R visit(Labeled instance) ;
    
    R visit(IfThenElse instance) ;
    
    R visit(While instance) ;
    
    R visit(For instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementNoShortIf instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(WithoutTrailing instance) {
      return otherwise((instance));
    }
    
    default R visit(Labeled instance) {
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
  
  public static final class WithoutTrailing extends StatementNoShortIf {
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
  
  public static final class Labeled extends StatementNoShortIf {
    public final LabeledStatementNoShortIf value;
    
    public Labeled (LabeledStatementNoShortIf value) {
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
  
  public static final class IfThenElse extends StatementNoShortIf {
    public final IfThenElseStatementNoShortIf value;
    
    public IfThenElse (IfThenElseStatementNoShortIf value) {
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
  
  public static final class While extends StatementNoShortIf {
    public final WhileStatementNoShortIf value;
    
    public While (WhileStatementNoShortIf value) {
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
  
  public static final class For extends StatementNoShortIf {
    public final ForStatementNoShortIf value;
    
    public For (ForStatementNoShortIf value) {
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