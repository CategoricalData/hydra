// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class IterationStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IterationStatement");
  
  public static final hydra.core.Name FIELD_NAME_WHILE = new hydra.core.Name("while");
  
  public static final hydra.core.Name FIELD_NAME_DO = new hydra.core.Name("do");
  
  public static final hydra.core.Name FIELD_NAME_FOR = new hydra.core.Name("for");
  
  public static final hydra.core.Name FIELD_NAME_FOREACH = new hydra.core.Name("foreach");
  
  private IterationStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(While instance) ;
    
    R visit(Do instance) ;
    
    R visit(For instance) ;
    
    R visit(Foreach instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IterationStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(While instance) {
      return otherwise((instance));
    }
    
    default R visit(Do instance) {
      return otherwise((instance));
    }
    
    default R visit(For instance) {
      return otherwise((instance));
    }
    
    default R visit(Foreach instance) {
      return otherwise((instance));
    }
  }
  
  public static final class While extends hydra.ext.csharp.syntax.IterationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.WhileStatement value;
    
    public While (hydra.ext.csharp.syntax.WhileStatement value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Do extends hydra.ext.csharp.syntax.IterationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.DoStatement value;
    
    public Do (hydra.ext.csharp.syntax.DoStatement value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class For extends hydra.ext.csharp.syntax.IterationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.ForStatement value;
    
    public For (hydra.ext.csharp.syntax.ForStatement value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Foreach extends hydra.ext.csharp.syntax.IterationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.ForeachStatement value;
    
    public Foreach (hydra.ext.csharp.syntax.ForeachStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Foreach)) {
        return false;
      }
      Foreach o = (Foreach) (other);
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