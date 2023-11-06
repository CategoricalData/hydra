package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class UpdatingClause implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.UpdatingClause");
  
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
  
  public static final class Create extends hydra.langs.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.langs.cypher.openCypher.Create value;
    
    public Create (hydra.langs.cypher.openCypher.Create value) {
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
  
  public static final class Merge extends hydra.langs.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.langs.cypher.openCypher.Merge value;
    
    public Merge (hydra.langs.cypher.openCypher.Merge value) {
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
  
  public static final class Delete extends hydra.langs.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.langs.cypher.openCypher.Delete value;
    
    public Delete (hydra.langs.cypher.openCypher.Delete value) {
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
  
  public static final class Set extends hydra.langs.cypher.openCypher.UpdatingClause implements Serializable {
    public final java.util.List<hydra.langs.cypher.openCypher.SetItem> value;
    
    public Set (java.util.List<hydra.langs.cypher.openCypher.SetItem> value) {
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
  
  public static final class Remove extends hydra.langs.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.langs.cypher.openCypher.Remove value;
    
    public Remove (hydra.langs.cypher.openCypher.Remove value) {
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