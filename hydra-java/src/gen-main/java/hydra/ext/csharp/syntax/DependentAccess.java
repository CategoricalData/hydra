// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class DependentAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DependentAccess");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER_ACCESS = new hydra.core.Name("memberAccess");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_ACCESS = new hydra.core.Name("elementAccess");
  
  public static final hydra.core.Name FIELD_NAME_INVOCATION = new hydra.core.Name("invocation");
  
  private DependentAccess () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(MemberAccess instance) ;
    
    R visit(ElementAccess instance) ;
    
    R visit(Invocation instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DependentAccess instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(MemberAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(ElementAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(Invocation instance) {
      return otherwise((instance));
    }
  }
  
  public static final class MemberAccess extends hydra.ext.csharp.syntax.DependentAccess implements Serializable {
    public final hydra.ext.csharp.syntax.DependentAccessForMember value;
    
    public MemberAccess (hydra.ext.csharp.syntax.DependentAccessForMember value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MemberAccess)) {
        return false;
      }
      MemberAccess o = (MemberAccess) (other);
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
  
  public static final class ElementAccess extends hydra.ext.csharp.syntax.DependentAccess implements Serializable {
    public final hydra.ext.csharp.syntax.ArgumentList value;
    
    public ElementAccess (hydra.ext.csharp.syntax.ArgumentList value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementAccess)) {
        return false;
      }
      ElementAccess o = (ElementAccess) (other);
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
  
  public static final class Invocation extends hydra.ext.csharp.syntax.DependentAccess implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> value;
    
    public Invocation (hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Invocation)) {
        return false;
      }
      Invocation o = (Invocation) (other);
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