// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public abstract class ComparisonOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.ComparisonOperator");
  
  public static final hydra.core.Name FIELD_NAME_EQ = new hydra.core.Name("eq");
  
  public static final hydra.core.Name FIELD_NAME_NEQ = new hydra.core.Name("neq");
  
  public static final hydra.core.Name FIELD_NAME_LT = new hydra.core.Name("lt");
  
  public static final hydra.core.Name FIELD_NAME_LTE = new hydra.core.Name("lte");
  
  public static final hydra.core.Name FIELD_NAME_GT = new hydra.core.Name("gt");
  
  public static final hydra.core.Name FIELD_NAME_GTE = new hydra.core.Name("gte");
  
  private ComparisonOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Eq instance) ;
    
    R visit(Neq instance) ;
    
    R visit(Lt instance) ;
    
    R visit(Lte instance) ;
    
    R visit(Gt instance) ;
    
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
    
    default R visit(Lte instance) {
      return otherwise((instance));
    }
    
    default R visit(Gt instance) {
      return otherwise((instance));
    }
    
    default R visit(Gte instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Eq extends hydra.langs.tinkerpop.queries.ComparisonOperator implements Serializable {
    public Eq () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Eq)) {
        return false;
      }
      Eq o = (Eq) (other);
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
  
  public static final class Neq extends hydra.langs.tinkerpop.queries.ComparisonOperator implements Serializable {
    public Neq () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Neq)) {
        return false;
      }
      Neq o = (Neq) (other);
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
  
  public static final class Lt extends hydra.langs.tinkerpop.queries.ComparisonOperator implements Serializable {
    public Lt () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lt)) {
        return false;
      }
      Lt o = (Lt) (other);
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
  
  public static final class Lte extends hydra.langs.tinkerpop.queries.ComparisonOperator implements Serializable {
    public Lte () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lte)) {
        return false;
      }
      Lte o = (Lte) (other);
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
  
  public static final class Gt extends hydra.langs.tinkerpop.queries.ComparisonOperator implements Serializable {
    public Gt () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gt)) {
        return false;
      }
      Gt o = (Gt) (other);
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
  
  public static final class Gte extends hydra.langs.tinkerpop.queries.ComparisonOperator implements Serializable {
    public Gte () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gte)) {
        return false;
      }
      Gte o = (Gte) (other);
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
}