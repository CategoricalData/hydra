// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class UpdatingClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.UpdatingClause");
  
  public static final hydra.core.Name FIELD_NAME_CREATE = new hydra.core.Name("create");
  
  public static final hydra.core.Name FIELD_NAME_MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name FIELD_NAME_DELETE = new hydra.core.Name("delete");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name FIELD_NAME_REMOVE = new hydra.core.Name("remove");
  
  private UpdatingClause () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Create instance) ;
    
    R visit(Merge instance) ;
    
    R visit(Delete instance) ;
    
    R visit(Set instance) ;
    
    R visit(Remove instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UpdatingClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Create instance) {
      return otherwise((instance));
    }
    
    default R visit(Merge instance) {
      return otherwise((instance));
    }
    
    default R visit(Delete instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Remove instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Create extends hydra.ext.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Create value;
    
    public Create (hydra.ext.cypher.openCypher.Create value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Create)) {
        return false;
      }
      Create o = (Create) (other);
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
  
  public static final class Merge extends hydra.ext.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Merge value;
    
    public Merge (hydra.ext.cypher.openCypher.Merge value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Merge)) {
        return false;
      }
      Merge o = (Merge) (other);
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
  
  public static final class Delete extends hydra.ext.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Delete value;
    
    public Delete (hydra.ext.cypher.openCypher.Delete value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Delete)) {
        return false;
      }
      Delete o = (Delete) (other);
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
  
  public static final class Set extends hydra.ext.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Set value;
    
    public Set (hydra.ext.cypher.openCypher.Set value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
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
  
  public static final class Remove extends hydra.ext.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Remove value;
    
    public Remove (hydra.ext.cypher.openCypher.Remove value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Remove)) {
        return false;
      }
      Remove o = (Remove) (other);
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