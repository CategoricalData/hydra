// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ComparisonOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.ComparisonOperator");
  
  public static final hydra.core.Name FIELD_NAME_EQ = new hydra.core.Name("eq");
  
  public static final hydra.core.Name FIELD_NAME_NEQ = new hydra.core.Name("neq");
  
  public static final hydra.core.Name FIELD_NAME_LT = new hydra.core.Name("lt");
  
  public static final hydra.core.Name FIELD_NAME_GT = new hydra.core.Name("gt");
  
  public static final hydra.core.Name FIELD_NAME_LTE = new hydra.core.Name("lte");
  
  public static final hydra.core.Name FIELD_NAME_GTE = new hydra.core.Name("gte");
  
  private ComparisonOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Eq instance) ;
    
    R visit(Neq instance) ;
    
    R visit(Lt instance) ;
    
    R visit(Gt instance) ;
    
    R visit(Lte instance) ;
    
    R visit(Gte instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ComparisonOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Eq instance) {
      return otherwise((instance));
    }
    
    default R visit(Neq instance) {
      return otherwise((instance));
    }
    
    default R visit(Lt instance) {
      return otherwise((instance));
    }
    
    default R visit(Gt instance) {
      return otherwise((instance));
    }
    
    default R visit(Lte instance) {
      return otherwise((instance));
    }
    
    default R visit(Gte instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Eq extends hydra.ext.cypher.openCypher.ComparisonOperator implements Serializable {
    public final Boolean value;
    
    public Eq (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Eq)) {
        return false;
      }
      Eq o = (Eq) (other);
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
  
  public static final class Neq extends hydra.ext.cypher.openCypher.ComparisonOperator implements Serializable {
    public final Boolean value;
    
    public Neq (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Neq)) {
        return false;
      }
      Neq o = (Neq) (other);
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
  
  public static final class Lt extends hydra.ext.cypher.openCypher.ComparisonOperator implements Serializable {
    public final Boolean value;
    
    public Lt (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lt)) {
        return false;
      }
      Lt o = (Lt) (other);
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
  
  public static final class Gt extends hydra.ext.cypher.openCypher.ComparisonOperator implements Serializable {
    public final Boolean value;
    
    public Gt (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gt)) {
        return false;
      }
      Gt o = (Gt) (other);
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
  
  public static final class Lte extends hydra.ext.cypher.openCypher.ComparisonOperator implements Serializable {
    public final Boolean value;
    
    public Lte (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lte)) {
        return false;
      }
      Lte o = (Lte) (other);
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
  
  public static final class Gte extends hydra.ext.cypher.openCypher.ComparisonOperator implements Serializable {
    public final Boolean value;
    
    public Gte (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gte)) {
        return false;
      }
      Gte o = (Gte) (other);
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
