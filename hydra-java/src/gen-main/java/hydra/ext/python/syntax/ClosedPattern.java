// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class ClosedPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ClosedPattern");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_CAPTURE = new hydra.core.Name("capture");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD = new hydra.core.Name("wildcard");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_GROUP = new hydra.core.Name("group");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_MAPPING = new hydra.core.Name("mapping");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  private ClosedPattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(Capture instance) ;
    
    R visit(Wildcard instance) ;
    
    R visit(Value instance) ;
    
    R visit(Group instance) ;
    
    R visit(Sequence instance) ;
    
    R visit(Mapping instance) ;
    
    R visit(Class_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClosedPattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Capture instance) {
      return otherwise((instance));
    }
    
    default R visit(Wildcard instance) {
      return otherwise((instance));
    }
    
    default R visit(Value instance) {
      return otherwise((instance));
    }
    
    default R visit(Group instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Mapping instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.LiteralExpression value;
    
    public Literal (hydra.ext.python.syntax.LiteralExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class Capture extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.CapturePattern value;
    
    public Capture (hydra.ext.python.syntax.CapturePattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Capture)) {
        return false;
      }
      Capture o = (Capture) (other);
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
  
  public static final class Wildcard extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public Wildcard () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) (other);
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
  
  public static final class Value extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.ValuePattern value;
    
    public Value (hydra.ext.python.syntax.ValuePattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) (other);
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
  
  public static final class Group extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.GroupPattern value;
    
    public Group (hydra.ext.python.syntax.GroupPattern value) {
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
  
  public static final class Sequence extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.SequencePattern value;
    
    public Sequence (hydra.ext.python.syntax.SequencePattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class Mapping extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.MappingPattern value;
    
    public Mapping (hydra.ext.python.syntax.MappingPattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mapping)) {
        return false;
      }
      Mapping o = (Mapping) (other);
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
  
  public static final class Class_ extends hydra.ext.python.syntax.ClosedPattern implements Serializable {
    public final hydra.ext.python.syntax.ClassPattern value;
    
    public Class_ (hydra.ext.python.syntax.ClassPattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) (other);
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