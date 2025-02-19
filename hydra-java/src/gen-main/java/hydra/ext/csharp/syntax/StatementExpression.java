// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class StatementExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StatementExpression");
  
  public static final hydra.core.Name FIELD_NAME_NULL_CONDITIONAL_INVOCATION = new hydra.core.Name("nullConditionalInvocation");
  
  public static final hydra.core.Name FIELD_NAME_INVOCATION = new hydra.core.Name("invocation");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_CREATION = new hydra.core.Name("objectCreation");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGNMENT = new hydra.core.Name("assignment");
  
  public static final hydra.core.Name FIELD_NAME_POST_INCREMENT = new hydra.core.Name("postIncrement");
  
  public static final hydra.core.Name FIELD_NAME_POST_DECREMENT = new hydra.core.Name("postDecrement");
  
  public static final hydra.core.Name FIELD_NAME_PRE_INCREMENT = new hydra.core.Name("preIncrement");
  
  public static final hydra.core.Name FIELD_NAME_PRE_DECREMENT = new hydra.core.Name("preDecrement");
  
  public static final hydra.core.Name FIELD_NAME_AWAIT = new hydra.core.Name("await");
  
  private StatementExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NullConditionalInvocation instance) ;
    
    R visit(Invocation instance) ;
    
    R visit(ObjectCreation instance) ;
    
    R visit(Assignment instance) ;
    
    R visit(PostIncrement instance) ;
    
    R visit(PostDecrement instance) ;
    
    R visit(PreIncrement instance) ;
    
    R visit(PreDecrement instance) ;
    
    R visit(Await instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NullConditionalInvocation instance) {
      return otherwise((instance));
    }
    
    default R visit(Invocation instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectCreation instance) {
      return otherwise((instance));
    }
    
    default R visit(Assignment instance) {
      return otherwise((instance));
    }
    
    default R visit(PostIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PostDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PreIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PreDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(Await instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NullConditionalInvocation extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalInvocationExpression value;
    
    public NullConditionalInvocation (hydra.ext.csharp.syntax.NullConditionalInvocationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullConditionalInvocation)) {
        return false;
      }
      NullConditionalInvocation o = (NullConditionalInvocation) (other);
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
  
  public static final class Invocation extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.InvocationExpression value;
    
    public Invocation (hydra.ext.csharp.syntax.InvocationExpression value) {
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
  
  public static final class ObjectCreation extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ObjectCreationExpression value;
    
    public ObjectCreation (hydra.ext.csharp.syntax.ObjectCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectCreation)) {
        return false;
      }
      ObjectCreation o = (ObjectCreation) (other);
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
  
  public static final class Assignment extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Assignment value;
    
    public Assignment (hydra.ext.csharp.syntax.Assignment value) {
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
  
  public static final class PostIncrement extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PrimaryExpression value;
    
    public PostIncrement (hydra.ext.csharp.syntax.PrimaryExpression value) {
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
  
  public static final class PostDecrement extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PrimaryExpression value;
    
    public PostDecrement (hydra.ext.csharp.syntax.PrimaryExpression value) {
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
  
  public static final class PreIncrement extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public PreIncrement (hydra.ext.csharp.syntax.UnaryExpression value) {
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
  
  public static final class PreDecrement extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public PreDecrement (hydra.ext.csharp.syntax.UnaryExpression value) {
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
  
  public static final class Await extends hydra.ext.csharp.syntax.StatementExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryExpression value;
    
    public Await (hydra.ext.csharp.syntax.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Await)) {
        return false;
      }
      Await o = (Await) (other);
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