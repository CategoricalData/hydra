// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class MemberDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MemberDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER_ACCESS = new hydra.core.Name("memberAccess");
  
  public static final hydra.core.Name FIELD_NAME_NULL_CONDITIONAL_PROJECTION_INITIALIZER = new hydra.core.Name("nullConditionalProjectionInitializer");
  
  public static final hydra.core.Name FIELD_NAME_BASE_ACCESS = new hydra.core.Name("baseAccess");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGNMENT = new hydra.core.Name("assignment");
  
  private MemberDeclarator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(MemberAccess instance) ;
    
    R visit(NullConditionalProjectionInitializer instance) ;
    
    R visit(BaseAccess instance) ;
    
    R visit(Assignment instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MemberDeclarator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(MemberAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(NullConditionalProjectionInitializer instance) {
      return otherwise((instance));
    }
    
    default R visit(BaseAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(Assignment instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Name extends hydra.ext.csharp.syntax.MemberDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.SimpleName value;
    
    public Name (hydra.ext.csharp.syntax.SimpleName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class MemberAccess extends hydra.ext.csharp.syntax.MemberDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.MemberAccess value;
    
    public MemberAccess (hydra.ext.csharp.syntax.MemberAccess value) {
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
  
  public static final class NullConditionalProjectionInitializer extends hydra.ext.csharp.syntax.MemberDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalProjectionInitializer value;
    
    public NullConditionalProjectionInitializer (hydra.ext.csharp.syntax.NullConditionalProjectionInitializer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullConditionalProjectionInitializer)) {
        return false;
      }
      NullConditionalProjectionInitializer o = (NullConditionalProjectionInitializer) (other);
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
  
  public static final class BaseAccess extends hydra.ext.csharp.syntax.MemberDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.BaseAccess value;
    
    public BaseAccess (hydra.ext.csharp.syntax.BaseAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BaseAccess)) {
        return false;
      }
      BaseAccess o = (BaseAccess) (other);
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
  
  public static final class Assignment extends hydra.ext.csharp.syntax.MemberDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.AssignmentMemberDeclarator value;
    
    public Assignment (hydra.ext.csharp.syntax.AssignmentMemberDeclarator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assignment)) {
        return false;
      }
      Assignment o = (Assignment) (other);
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