package hydra.ext.java.syntax;

public abstract class StatementWithoutTrailingSubstatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.StatementWithoutTrailingSubstatement");
  
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
  
  public static final class Block extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Block");
    
    public final hydra.ext.java.syntax.Block value;
    
    public Block (hydra.ext.java.syntax.Block value) {
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
  
  public static final class Empty extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Empty");
    
    public final hydra.ext.java.syntax.EmptyStatement value;
    
    public Empty (hydra.ext.java.syntax.EmptyStatement value) {
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
  
  public static final class Expression extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Expression");
    
    public final hydra.ext.java.syntax.ExpressionStatement value;
    
    public Expression (hydra.ext.java.syntax.ExpressionStatement value) {
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
  
  public static final class Assert extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Assert");
    
    public final hydra.ext.java.syntax.AssertStatement value;
    
    public Assert (hydra.ext.java.syntax.AssertStatement value) {
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
  
  public static final class Switch extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Switch");
    
    public final hydra.ext.java.syntax.SwitchStatement value;
    
    public Switch (hydra.ext.java.syntax.SwitchStatement value) {
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
  
  public static final class Do extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Do");
    
    public final hydra.ext.java.syntax.DoStatement value;
    
    public Do (hydra.ext.java.syntax.DoStatement value) {
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
  
  public static final class Break extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Break");
    
    public final hydra.ext.java.syntax.BreakStatement value;
    
    public Break (hydra.ext.java.syntax.BreakStatement value) {
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
  
  public static final class Continue extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Continue");
    
    public final hydra.ext.java.syntax.ContinueStatement value;
    
    public Continue (hydra.ext.java.syntax.ContinueStatement value) {
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
  
  public static final class Return extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Return");
    
    public final hydra.ext.java.syntax.ReturnStatement value;
    
    public Return (hydra.ext.java.syntax.ReturnStatement value) {
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
  
  public static final class Synchronized extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Synchronized");
    
    public final hydra.ext.java.syntax.SynchronizedStatement value;
    
    public Synchronized (hydra.ext.java.syntax.SynchronizedStatement value) {
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
  
  public static final class Throw extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Throw");
    
    public final hydra.ext.java.syntax.ThrowStatement value;
    
    public Throw (hydra.ext.java.syntax.ThrowStatement value) {
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
  
  public static final class Try extends hydra.ext.java.syntax.StatementWithoutTrailingSubstatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Try");
    
    public final hydra.ext.java.syntax.TryStatement value;
    
    public Try (hydra.ext.java.syntax.TryStatement value) {
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