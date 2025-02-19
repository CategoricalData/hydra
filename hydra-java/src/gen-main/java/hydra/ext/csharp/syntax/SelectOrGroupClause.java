// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class SelectOrGroupClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SelectOrGroupClause");
  
  public static final hydra.core.Name FIELD_NAME_SELECT = new hydra.core.Name("select");
  
  public static final hydra.core.Name FIELD_NAME_GROUP = new hydra.core.Name("group");
  
  private SelectOrGroupClause () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Select instance) ;
    
    R visit(Group instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectOrGroupClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Select instance) {
      return otherwise((instance));
    }
    
    default R visit(Group instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Select extends hydra.ext.csharp.syntax.SelectOrGroupClause implements Serializable {
    public final hydra.ext.csharp.syntax.Expression value;
    
    public Select (hydra.ext.csharp.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) (other);
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
  
  public static final class Group extends hydra.ext.csharp.syntax.SelectOrGroupClause implements Serializable {
    public final hydra.ext.csharp.syntax.GroupClause value;
    
    public Group (hydra.ext.csharp.syntax.GroupClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Group)) {
        return false;
      }
      Group o = (Group) (other);
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