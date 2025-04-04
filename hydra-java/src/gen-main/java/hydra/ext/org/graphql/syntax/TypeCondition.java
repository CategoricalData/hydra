// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public abstract class TypeCondition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.TypeCondition");
  
  public static final hydra.core.Name FIELD_NAME_ON = new hydra.core.Name("on");
  
  public static final hydra.core.Name FIELD_NAME_NAMED_TYPE = new hydra.core.Name("namedType");
  
  private TypeCondition () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(On instance) ;
    
    R visit(NamedType instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeCondition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(On instance) {
      return otherwise((instance));
    }
    
    default R visit(NamedType instance) {
      return otherwise((instance));
    }
  }
  
  public static final class On extends hydra.ext.org.graphql.syntax.TypeCondition implements Serializable {
    public On () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof On)) {
        return false;
      }
      On o = (On) (other);
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
  
  public static final class NamedType extends hydra.ext.org.graphql.syntax.TypeCondition implements Serializable {
    public final hydra.ext.org.graphql.syntax.NamedType value;
    
    public NamedType (hydra.ext.org.graphql.syntax.NamedType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NamedType)) {
        return false;
      }
      NamedType o = (NamedType) (other);
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