// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NonAssignmentExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NonAssignmentExpression");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATION = new hydra.core.Name("declaration");
  
  public static final hydra.core.Name FIELD_NAME_CONDITIONAL = new hydra.core.Name("conditional");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA = new hydra.core.Name("lambda");
  
  public static final hydra.core.Name FIELD_NAME_QUERY = new hydra.core.Name("query");
  
  private NonAssignmentExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Declaration instance) ;
    
    R visit(Conditional instance) ;
    
    R visit(Lambda instance) ;
    
    R visit(Query instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonAssignmentExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Declaration instance) {
      return otherwise((instance));
    }
    
    default R visit(Conditional instance) {
      return otherwise((instance));
    }
    
    default R visit(Lambda instance) {
      return otherwise((instance));
    }
    
    default R visit(Query instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Declaration extends hydra.ext.csharp.syntax.NonAssignmentExpression implements Serializable {
    public final hydra.ext.csharp.syntax.DeclarationExpression value;
    
    public Declaration (hydra.ext.csharp.syntax.DeclarationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Declaration)) {
        return false;
      }
      Declaration o = (Declaration) (other);
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
  
  public static final class Conditional extends hydra.ext.csharp.syntax.NonAssignmentExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ConditionalExpression value;
    
    public Conditional (hydra.ext.csharp.syntax.ConditionalExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conditional)) {
        return false;
      }
      Conditional o = (Conditional) (other);
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
  
  public static final class Lambda extends hydra.ext.csharp.syntax.NonAssignmentExpression implements Serializable {
    public final hydra.ext.csharp.syntax.LambdaExpression value;
    
    public Lambda (hydra.ext.csharp.syntax.LambdaExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) (other);
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
  
  public static final class Query extends hydra.ext.csharp.syntax.NonAssignmentExpression implements Serializable {
    public final hydra.ext.csharp.syntax.QueryExpression value;
    
    public Query (hydra.ext.csharp.syntax.QueryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Query)) {
        return false;
      }
      Query o = (Query) (other);
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