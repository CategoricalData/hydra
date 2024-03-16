package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class RelationshipPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipPattern");
  
  private RelationshipPattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LeftArrow instance) ;
    
    R visit(Detail instance) ;
    
    R visit(RightArrow instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelationshipPattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LeftArrow instance) {
      return otherwise((instance));
    }
    
    default R visit(Detail instance) {
      return otherwise((instance));
    }
    
    default R visit(RightArrow instance) {
      return otherwise((instance));
    }
  }
  
  public static final class LeftArrow extends hydra.langs.cypher.openCypher.RelationshipPattern implements Serializable {
    public final Boolean value;
    
    public LeftArrow (Boolean value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftArrow)) {
        return false;
      }
      LeftArrow o = (LeftArrow) (other);
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
  
  public static final class Detail extends hydra.langs.cypher.openCypher.RelationshipPattern implements Serializable {
    public final java.util.Optional<hydra.langs.cypher.openCypher.RelationshipDetail> value;
    
    public Detail (java.util.Optional<hydra.langs.cypher.openCypher.RelationshipDetail> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Detail)) {
        return false;
      }
      Detail o = (Detail) (other);
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
  
  public static final class RightArrow extends hydra.langs.cypher.openCypher.RelationshipPattern implements Serializable {
    public final Boolean value;
    
    public RightArrow (Boolean value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightArrow)) {
        return false;
      }
      RightArrow o = (RightArrow) (other);
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