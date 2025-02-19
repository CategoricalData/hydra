// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class EmbeddedStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.EmbeddedStatement");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_EMPTY = new hydra.core.Name("empty");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_SELECTION = new hydra.core.Name("selection");
  
  public static final hydra.core.Name FIELD_NAME_ITERATION = new hydra.core.Name("iteration");
  
  public static final hydra.core.Name FIELD_NAME_JUMP = new hydra.core.Name("jump");
  
  public static final hydra.core.Name FIELD_NAME_TRY = new hydra.core.Name("try");
  
  public static final hydra.core.Name FIELD_NAME_CHECKED = new hydra.core.Name("checked");
  
  public static final hydra.core.Name FIELD_NAME_UNCHECKED = new hydra.core.Name("unchecked");
  
  public static final hydra.core.Name FIELD_NAME_LOCK = new hydra.core.Name("lock");
  
  public static final hydra.core.Name FIELD_NAME_USING = new hydra.core.Name("using");
  
  public static final hydra.core.Name FIELD_NAME_YIELD = new hydra.core.Name("yield");
  
  public static final hydra.core.Name FIELD_NAME_UNSAFE = new hydra.core.Name("unsafe");
  
  public static final hydra.core.Name FIELD_NAME_FIXED = new hydra.core.Name("fixed");
  
  private EmbeddedStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Block instance) ;
    
    R visit(Empty instance) ;
    
    R visit(Expression instance) ;
    
    R visit(Selection instance) ;
    
    R visit(Iteration instance) ;
    
    R visit(Jump instance) ;
    
    R visit(Try instance) ;
    
    R visit(Checked instance) ;
    
    R visit(Unchecked instance) ;
    
    R visit(Lock instance) ;
    
    R visit(Using instance) ;
    
    R visit(Yield instance) ;
    
    R visit(Unsafe instance) ;
    
    R visit(Fixed instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EmbeddedStatement instance) {
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
    
    default R visit(Selection instance) {
      return otherwise((instance));
    }
    
    default R visit(Iteration instance) {
      return otherwise((instance));
    }
    
    default R visit(Jump instance) {
      return otherwise((instance));
    }
    
    default R visit(Try instance) {
      return otherwise((instance));
    }
    
    default R visit(Checked instance) {
      return otherwise((instance));
    }
    
    default R visit(Unchecked instance) {
      return otherwise((instance));
    }
    
    default R visit(Lock instance) {
      return otherwise((instance));
    }
    
    default R visit(Using instance) {
      return otherwise((instance));
    }
    
    default R visit(Yield instance) {
      return otherwise((instance));
    }
    
    default R visit(Unsafe instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Block extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public Block (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Empty extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public Empty () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Empty)) {
        return false;
      }
      Empty o = (Empty) (other);
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
  
  public static final class Expression extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.StatementExpression value;
    
    public Expression (hydra.ext.csharp.syntax.StatementExpression value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Selection extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.SelectionStatement value;
    
    public Selection (hydra.ext.csharp.syntax.SelectionStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Selection)) {
        return false;
      }
      Selection o = (Selection) (other);
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
  
  public static final class Iteration extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.IterationStatement value;
    
    public Iteration (hydra.ext.csharp.syntax.IterationStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iteration)) {
        return false;
      }
      Iteration o = (Iteration) (other);
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
  
  public static final class Jump extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.JumpStatement value;
    
    public Jump (hydra.ext.csharp.syntax.JumpStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Jump)) {
        return false;
      }
      Jump o = (Jump) (other);
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
  
  public static final class Try extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.TryStatement value;
    
    public Try (hydra.ext.csharp.syntax.TryStatement value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Checked extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public Checked (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Checked)) {
        return false;
      }
      Checked o = (Checked) (other);
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
  
  public static final class Unchecked extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public Unchecked (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unchecked)) {
        return false;
      }
      Unchecked o = (Unchecked) (other);
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
  
  public static final class Lock extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.LockStatement value;
    
    public Lock (hydra.ext.csharp.syntax.LockStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lock)) {
        return false;
      }
      Lock o = (Lock) (other);
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
  
  public static final class Using extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.UsingStatement value;
    
    public Using (hydra.ext.csharp.syntax.UsingStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Using)) {
        return false;
      }
      Using o = (Using) (other);
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
  
  public static final class Yield extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.YieldStatement value;
    
    public Yield (hydra.ext.csharp.syntax.YieldStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Yield)) {
        return false;
      }
      Yield o = (Yield) (other);
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
  
  public static final class Unsafe extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public Unsafe (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unsafe)) {
        return false;
      }
      Unsafe o = (Unsafe) (other);
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
  
  public static final class Fixed extends hydra.ext.csharp.syntax.EmbeddedStatement implements Serializable {
    public final hydra.ext.csharp.syntax.FixedStatement value;
    
    public Fixed (hydra.ext.csharp.syntax.FixedStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed)) {
        return false;
      }
      Fixed o = (Fixed) (other);
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