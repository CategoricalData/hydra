package hydra.ext.java.syntax;

public abstract class StatementExpression {
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
  
  public static final class Assignment extends StatementExpression {
    public final Assignment value;
    
    public Assignment (Assignment value) {
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
  
  public static final class PreIncrement extends StatementExpression {
    public final PreIncrementExpression value;
    
    public PreIncrement (PreIncrementExpression value) {
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
  
  public static final class PreDecrement extends StatementExpression {
    public final PreDecrementExpression value;
    
    public PreDecrement (PreDecrementExpression value) {
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
  
  public static final class PostIncrement extends StatementExpression {
    public final PostIncrementExpression value;
    
    public PostIncrement (PostIncrementExpression value) {
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
  
  public static final class PostDecrement extends StatementExpression {
    public final PostDecrementExpression value;
    
    public PostDecrement (PostDecrementExpression value) {
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
  
  public static final class MethodInvocation extends StatementExpression {
    public final MethodInvocation value;
    
    public MethodInvocation (MethodInvocation value) {
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
  
  public static final class ClassInstanceCreation extends StatementExpression {
    public final ClassInstanceCreationExpression value;
    
    public ClassInstanceCreation (ClassInstanceCreationExpression value) {
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