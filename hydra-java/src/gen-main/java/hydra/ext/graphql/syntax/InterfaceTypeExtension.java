package hydra.ext.graphql.syntax;

public abstract class InterfaceTypeExtension {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InterfaceTypeExtension");
  
  private InterfaceTypeExtension () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Sequence2 instance) ;
    
    R visit(Sequence3 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InterfaceTypeExtension instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence3 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.ext.graphql.syntax.InterfaceTypeExtension {
    public final hydra.ext.graphql.syntax.InterfaceTypeExtension_Sequence value;
    
    public Sequence (hydra.ext.graphql.syntax.InterfaceTypeExtension_Sequence value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class Sequence2 extends hydra.ext.graphql.syntax.InterfaceTypeExtension {
    public final hydra.ext.graphql.syntax.InterfaceTypeExtension_Sequence2 value;
    
    public Sequence2 (hydra.ext.graphql.syntax.InterfaceTypeExtension_Sequence2 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) (other);
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
  
  public static final class Sequence3 extends hydra.ext.graphql.syntax.InterfaceTypeExtension {
    public final hydra.ext.graphql.syntax.InterfaceTypeExtension_Sequence3 value;
    
    public Sequence3 (hydra.ext.graphql.syntax.InterfaceTypeExtension_Sequence3 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence3)) {
        return false;
      }
      Sequence3 o = (Sequence3) (other);
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