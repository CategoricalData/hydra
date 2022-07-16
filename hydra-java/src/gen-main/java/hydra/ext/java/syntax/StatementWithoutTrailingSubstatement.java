package hydra.ext.java.syntax;

public abstract class StatementWithoutTrailingSubstatement {
  private StatementWithoutTrailingSubstatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Block instance) ;
    
    R visit(Empty instance) ;
    
    R visit(Expression instance) ;
    
    R visit(Assert instance) ;
    
    R visit(Switch instance) ;
    
    R visit(Do instance) ;
    
    R visit(Break instance) ;
    
    R visit(Continue instance) ;
    
    R visit(Return instance) ;
    
    R visit(Synchronized instance) ;
    
    R visit(Throw instance) ;
    
    R visit(Try instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementWithoutTrailingSubstatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Block instance) {
      return otherwise((instance));
    }
    
    default R visit(Empty instance) {
      return otherwise((instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
    
    default R visit(Assert instance) {
      return otherwise((instance));
    }
    
    default R visit(Switch instance) {
      return otherwise((instance));
    }
    
    default R visit(Do instance) {
      return otherwise((instance));
    }
    
    default R visit(Break instance) {
      return otherwise((instance));
    }
    
    default R visit(Continue instance) {
      return otherwise((instance));
    }
    
    default R visit(Return instance) {
      return otherwise((instance));
    }
    
    default R visit(Synchronized instance) {
      return otherwise((instance));
    }
    
    default R visit(Throw instance) {
      return otherwise((instance));
    }
    
    default R visit(Try instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Block extends StatementWithoutTrailingSubstatement {
    public final Block value;
    
    public Block (Block value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) (other);
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
  
  public static final class Empty extends StatementWithoutTrailingSubstatement {
    public final EmptyStatement value;
    
    public Empty (EmptyStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Empty)) {
        return false;
      }
      Empty o = (Empty) (other);
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
  
  public static final class Expression extends StatementWithoutTrailingSubstatement {
    public final ExpressionStatement value;
    
    public Expression (ExpressionStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) (other);
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
  
  public static final class Assert extends StatementWithoutTrailingSubstatement {
    public final AssertStatement value;
    
    public Assert (AssertStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assert)) {
        return false;
      }
      Assert o = (Assert) (other);
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
  
  public static final class Switch extends StatementWithoutTrailingSubstatement {
    public final SwitchStatement value;
    
    public Switch (SwitchStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Switch)) {
        return false;
      }
      Switch o = (Switch) (other);
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
  
  public static final class Do extends StatementWithoutTrailingSubstatement {
    public final DoStatement value;
    
    public Do (DoStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Do)) {
        return false;
      }
      Do o = (Do) (other);
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
  
  public static final class Break extends StatementWithoutTrailingSubstatement {
    public final BreakStatement value;
    
    public Break (BreakStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Break)) {
        return false;
      }
      Break o = (Break) (other);
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
  
  public static final class Continue extends StatementWithoutTrailingSubstatement {
    public final ContinueStatement value;
    
    public Continue (ContinueStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Continue)) {
        return false;
      }
      Continue o = (Continue) (other);
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
  
  public static final class Return extends StatementWithoutTrailingSubstatement {
    public final ReturnStatement value;
    
    public Return (ReturnStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) (other);
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
  
  public static final class Synchronized extends StatementWithoutTrailingSubstatement {
    public final SynchronizedStatement value;
    
    public Synchronized (SynchronizedStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Synchronized)) {
        return false;
      }
      Synchronized o = (Synchronized) (other);
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
  
  public static final class Throw extends StatementWithoutTrailingSubstatement {
    public final ThrowStatement value;
    
    public Throw (ThrowStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Throw)) {
        return false;
      }
      Throw o = (Throw) (other);
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
  
  public static final class Try extends StatementWithoutTrailingSubstatement {
    public final TryStatement value;
    
    public Try (TryStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Try)) {
        return false;
      }
      Try o = (Try) (other);
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