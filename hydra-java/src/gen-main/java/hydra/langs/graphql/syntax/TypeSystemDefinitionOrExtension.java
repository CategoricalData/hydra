// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public abstract class TypeSystemDefinitionOrExtension implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.TypeSystemDefinitionOrExtension");
  
  public static final hydra.core.Name FIELD_NAME_DEFINITION = new hydra.core.Name("definition");
  
  public static final hydra.core.Name FIELD_NAME_EXTENSION = new hydra.core.Name("extension");
  
  private TypeSystemDefinitionOrExtension () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Definition instance) ;
    
    R visit(Extension instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeSystemDefinitionOrExtension instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Definition instance) {
      return otherwise((instance));
    }
    
    default R visit(Extension instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Definition extends hydra.langs.graphql.syntax.TypeSystemDefinitionOrExtension implements Serializable {
    public final hydra.langs.graphql.syntax.TypeSystemDefinition value;
    
    public Definition (hydra.langs.graphql.syntax.TypeSystemDefinition value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Definition)) {
        return false;
      }
      Definition o = (Definition) (other);
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
  
  public static final class Extension extends hydra.langs.graphql.syntax.TypeSystemDefinitionOrExtension implements Serializable {
    public final hydra.langs.graphql.syntax.TypeSystemExtension value;
    
    public Extension (hydra.langs.graphql.syntax.TypeSystemExtension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extension)) {
        return false;
      }
      Extension o = (Extension) (other);
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