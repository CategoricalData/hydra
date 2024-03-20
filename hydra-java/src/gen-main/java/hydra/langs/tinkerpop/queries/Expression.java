package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public abstract class Expression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.Expression");
  
  private Expression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Associative instance) ;
    
    R visit(Binary instance) ;
    
    R visit(Property instance) ;
    
    R visit(Unary instance) ;
    
    R visit(Variable instance) ;
    
    R visit(Vertex instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Associative instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
    
    default R visit(Vertex instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Associative extends hydra.langs.tinkerpop.queries.Expression implements Serializable {
    public final hydra.langs.tinkerpop.queries.AssociativeExpression value;
    
    public Associative (hydra.langs.tinkerpop.queries.AssociativeExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Associative)) {
        return false;
      }
      Associative o = (Associative) (other);
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
  
  public static final class Binary extends hydra.langs.tinkerpop.queries.Expression implements Serializable {
    public final hydra.langs.tinkerpop.queries.BinaryExpression value;
    
    public Binary (hydra.langs.tinkerpop.queries.BinaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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
  
  public static final class Property extends hydra.langs.tinkerpop.queries.Expression implements Serializable {
    public final hydra.langs.tinkerpop.queries.PropertyProjection value;
    
    public Property (hydra.langs.tinkerpop.queries.PropertyProjection value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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
  
  public static final class Unary extends hydra.langs.tinkerpop.queries.Expression implements Serializable {
    public final hydra.langs.tinkerpop.queries.UnaryExpression value;
    
    public Unary (hydra.langs.tinkerpop.queries.UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) (other);
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
  
  public static final class Variable extends hydra.langs.tinkerpop.queries.Expression implements Serializable {
    public final hydra.langs.tinkerpop.queries.Variable value;
    
    public Variable (hydra.langs.tinkerpop.queries.Variable value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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
  
  public static final class Vertex extends hydra.langs.tinkerpop.queries.Expression implements Serializable {
    public final hydra.langs.tinkerpop.queries.VertexPattern value;
    
    public Vertex (hydra.langs.tinkerpop.queries.VertexPattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) (other);
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