package hydra.ext.graphql.syntax;

public abstract class ExecutableDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ExecutableDefinition");
  
  private ExecutableDefinition () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Operation instance) ;
    
    R visit(Fragment instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExecutableDefinition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Operation instance) {
      return otherwise((instance));
    }
    
    default R visit(Fragment instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Operation extends hydra.ext.graphql.syntax.ExecutableDefinition {
    public final hydra.ext.graphql.syntax.OperationDefinition value;
    
    public Operation (hydra.ext.graphql.syntax.OperationDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Operation)) {
        return false;
      }
      Operation o = (Operation) (other);
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
  
  public static final class Fragment extends hydra.ext.graphql.syntax.ExecutableDefinition {
    public final hydra.ext.graphql.syntax.FragmentDefinition value;
    
    public Fragment (hydra.ext.graphql.syntax.FragmentDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fragment)) {
        return false;
      }
      Fragment o = (Fragment) (other);
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