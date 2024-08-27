// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public abstract class Definition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Definition");
  
  public static final hydra.core.Name FIELD_NAME_EXECUTABLE = new hydra.core.Name("executable");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_SYSTEM = new hydra.core.Name("typeSystem");
  
  private Definition () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Executable instance) ;
    
    R visit(TypeSystem instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Definition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Executable instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeSystem instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Executable extends hydra.ext.graphql.syntax.Definition implements Serializable {
    public final hydra.ext.graphql.syntax.ExecutableDefinition value;
    
    public Executable (hydra.ext.graphql.syntax.ExecutableDefinition value) {
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
  
  public static final class TypeSystem extends hydra.ext.graphql.syntax.Definition implements Serializable {
    public final hydra.ext.graphql.syntax.TypeSystemDefinitionOrExtension value;
    
    public TypeSystem (hydra.ext.graphql.syntax.TypeSystemDefinitionOrExtension value) {
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