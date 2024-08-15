// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public abstract class StatementExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.StatementExpression");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGNMENT = new hydra.core.Name("assignment");
  
  public static final hydra.core.Name FIELD_NAME_PRE_INCREMENT = new hydra.core.Name("preIncrement");
  
  public static final hydra.core.Name FIELD_NAME_PRE_DECREMENT = new hydra.core.Name("preDecrement");
  
  public static final hydra.core.Name FIELD_NAME_POST_INCREMENT = new hydra.core.Name("postIncrement");
  
  public static final hydra.core.Name FIELD_NAME_POST_DECREMENT = new hydra.core.Name("postDecrement");
  
  public static final hydra.core.Name FIELD_NAME_METHOD_INVOCATION = new hydra.core.Name("methodInvocation");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_INSTANCE_CREATION = new hydra.core.Name("classInstanceCreation");
  
  private StatementExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Assignment instance) ;
    
    R visit(PreIncrement instance) ;
    
    R visit(PreDecrement instance) ;
    
    R visit(PostIncrement instance) ;
    
    R visit(PostDecrement instance) ;
    
    R visit(MethodInvocation instance) ;
    
    R visit(ClassInstanceCreation instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Assignment instance) {
      return otherwise((instance));
    }
    
    default R visit(PreIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PreDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PostIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PostDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(MethodInvocation instance) {
      return otherwise((instance));
    }
    
    default R visit(ClassInstanceCreation instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Assignment extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.Assignment value;
    
    public Assignment (hydra.langs.java.syntax.Assignment value) {
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
  
  public static final class PreIncrement extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.PreIncrementExpression value;
    
    public PreIncrement (hydra.langs.java.syntax.PreIncrementExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreIncrement)) {
        return false;
      }
      PreIncrement o = (PreIncrement) (other);
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
  
  public static final class PreDecrement extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.PreDecrementExpression value;
    
    public PreDecrement (hydra.langs.java.syntax.PreDecrementExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreDecrement)) {
        return false;
      }
      PreDecrement o = (PreDecrement) (other);
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
  
  public static final class PostIncrement extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.PostIncrementExpression value;
    
    public PostIncrement (hydra.langs.java.syntax.PostIncrementExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostIncrement)) {
        return false;
      }
      PostIncrement o = (PostIncrement) (other);
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
  
  public static final class PostDecrement extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.PostDecrementExpression value;
    
    public PostDecrement (hydra.langs.java.syntax.PostDecrementExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostDecrement)) {
        return false;
      }
      PostDecrement o = (PostDecrement) (other);
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
  
  public static final class MethodInvocation extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.MethodInvocation value;
    
    public MethodInvocation (hydra.langs.java.syntax.MethodInvocation value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MethodInvocation)) {
        return false;
      }
      MethodInvocation o = (MethodInvocation) (other);
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
  
  public static final class ClassInstanceCreation extends hydra.langs.java.syntax.StatementExpression implements Serializable {
    public final hydra.langs.java.syntax.ClassInstanceCreationExpression value;
    
    public ClassInstanceCreation (hydra.langs.java.syntax.ClassInstanceCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassInstanceCreation)) {
        return false;
      }
      ClassInstanceCreation o = (ClassInstanceCreation) (other);
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