// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class QueryBodyClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.QueryBodyClause");
  
  public static final hydra.core.Name FIELD_NAME_FROM = new hydra.core.Name("from");
  
  public static final hydra.core.Name FIELD_NAME_LET = new hydra.core.Name("let");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
  public static final hydra.core.Name FIELD_NAME_JOIN = new hydra.core.Name("join");
  
  public static final hydra.core.Name FIELD_NAME_ORDERBY = new hydra.core.Name("orderby");
  
  private QueryBodyClause () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(From instance) ;
    
    R visit(Let instance) ;
    
    R visit(Where instance) ;
    
    R visit(Join instance) ;
    
    R visit(Orderby instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(QueryBodyClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(From instance) {
      return otherwise((instance));
    }
    
    default R visit(Let instance) {
      return otherwise((instance));
    }
    
    default R visit(Where instance) {
      return otherwise((instance));
    }
    
    default R visit(Join instance) {
      return otherwise((instance));
    }
    
    default R visit(Orderby instance) {
      return otherwise((instance));
    }
  }
  
  public static final class From extends hydra.ext.csharp.syntax.QueryBodyClause implements Serializable {
    public final hydra.ext.csharp.syntax.FromClause value;
    
    public From (hydra.ext.csharp.syntax.FromClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof From)) {
        return false;
      }
      From o = (From) (other);
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
  
  public static final class Let extends hydra.ext.csharp.syntax.QueryBodyClause implements Serializable {
    public final hydra.ext.csharp.syntax.LetClause value;
    
    public Let (hydra.ext.csharp.syntax.LetClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) (other);
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
  
  public static final class Where extends hydra.ext.csharp.syntax.QueryBodyClause implements Serializable {
    public final hydra.ext.csharp.syntax.BooleanExpression value;
    
    public Where (hydra.ext.csharp.syntax.BooleanExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Where)) {
        return false;
      }
      Where o = (Where) (other);
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
  
  public static final class Join extends hydra.ext.csharp.syntax.QueryBodyClause implements Serializable {
    public final hydra.ext.csharp.syntax.JoinClause value;
    
    public Join (hydra.ext.csharp.syntax.JoinClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Join)) {
        return false;
      }
      Join o = (Join) (other);
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
  
  public static final class Orderby extends hydra.ext.csharp.syntax.QueryBodyClause implements Serializable {
    public final java.util.List<hydra.ext.csharp.syntax.Ordering> value;
    
    public Orderby (java.util.List<hydra.ext.csharp.syntax.Ordering> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Orderby)) {
        return false;
      }
      Orderby o = (Orderby) (other);
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