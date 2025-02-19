// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public abstract class Selection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.Selection");
  
  public static final hydra.core.Name FIELD_NAME_FIELD = new hydra.core.Name("field");
  
  public static final hydra.core.Name FIELD_NAME_FRAGMENT_SPREAD = new hydra.core.Name("fragmentSpread");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_FRAGMENT = new hydra.core.Name("inlineFragment");
  
  private Selection () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Field instance) ;
    
    R visit(FragmentSpread instance) ;
    
    R visit(InlineFragment instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Selection instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Field instance) {
      return otherwise((instance));
    }
    
    default R visit(FragmentSpread instance) {
      return otherwise((instance));
    }
    
    default R visit(InlineFragment instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Field extends hydra.ext.org.graphql.syntax.Selection implements Serializable {
    public final hydra.ext.org.graphql.syntax.Field value;
    
    public Field (hydra.ext.org.graphql.syntax.Field value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Field)) {
        return false;
      }
      Field o = (Field) (other);
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
  
  public static final class FragmentSpread extends hydra.ext.org.graphql.syntax.Selection implements Serializable {
    public final hydra.ext.org.graphql.syntax.FragmentSpread value;
    
    public FragmentSpread (hydra.ext.org.graphql.syntax.FragmentSpread value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FragmentSpread)) {
        return false;
      }
      FragmentSpread o = (FragmentSpread) (other);
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
  
  public static final class InlineFragment extends hydra.ext.org.graphql.syntax.Selection implements Serializable {
    public final hydra.ext.org.graphql.syntax.InlineFragment value;
    
    public InlineFragment (hydra.ext.org.graphql.syntax.InlineFragment value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InlineFragment)) {
        return false;
      }
      InlineFragment o = (InlineFragment) (other);
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