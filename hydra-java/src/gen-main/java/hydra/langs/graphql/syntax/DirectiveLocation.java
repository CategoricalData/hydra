// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public abstract class DirectiveLocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.DirectiveLocation");
  
  public static final hydra.core.Name FIELD_NAME_EXECUTABLE = new hydra.core.Name("executable");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_SYSTEM = new hydra.core.Name("typeSystem");
  
  private DirectiveLocation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Executable instance) ;
    
    R visit(TypeSystem instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DirectiveLocation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Executable instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeSystem instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Executable extends hydra.langs.graphql.syntax.DirectiveLocation implements Serializable {
    public final hydra.langs.graphql.syntax.ExecutableDirectiveLocation value;
    
    public Executable (hydra.langs.graphql.syntax.ExecutableDirectiveLocation value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Executable)) {
        return false;
      }
      Executable o = (Executable) (other);
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
  
  public static final class TypeSystem extends hydra.langs.graphql.syntax.DirectiveLocation implements Serializable {
    public final hydra.langs.graphql.syntax.TypeSystemDirectiveLocation value;
    
    public TypeSystem (hydra.langs.graphql.syntax.TypeSystemDirectiveLocation value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeSystem)) {
        return false;
      }
      TypeSystem o = (TypeSystem) (other);
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