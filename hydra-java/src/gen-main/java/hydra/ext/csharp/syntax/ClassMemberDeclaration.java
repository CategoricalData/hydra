// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ClassMemberDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ClassMemberDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_FIELD = new hydra.core.Name("field");
  
  public static final hydra.core.Name FIELD_NAME_METHOD = new hydra.core.Name("method");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_EVENT = new hydra.core.Name("event");
  
  public static final hydra.core.Name FIELD_NAME_INDEXER = new hydra.core.Name("indexer");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR = new hydra.core.Name("constructor");
  
  public static final hydra.core.Name FIELD_NAME_FINALIZER = new hydra.core.Name("finalizer");
  
  public static final hydra.core.Name FIELD_NAME_STATIC_CONSTRUCTOR = new hydra.core.Name("staticConstructor");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  private ClassMemberDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Constant instance) ;
    
    R visit(Field instance) ;
    
    R visit(Method instance) ;
    
    R visit(Property instance) ;
    
    R visit(Event instance) ;
    
    R visit(Indexer instance) ;
    
    R visit(Operator instance) ;
    
    R visit(Constructor instance) ;
    
    R visit(Finalizer instance) ;
    
    R visit(StaticConstructor instance) ;
    
    R visit(Type instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassMemberDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Constant instance) {
      return otherwise((instance));
    }
    
    default R visit(Field instance) {
      return otherwise((instance));
    }
    
    default R visit(Method instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(Event instance) {
      return otherwise((instance));
    }
    
    default R visit(Indexer instance) {
      return otherwise((instance));
    }
    
    default R visit(Operator instance) {
      return otherwise((instance));
    }
    
    default R visit(Constructor instance) {
      return otherwise((instance));
    }
    
    default R visit(Finalizer instance) {
      return otherwise((instance));
    }
    
    default R visit(StaticConstructor instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Constant extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.ConstantDeclaration value;
    
    public Constant (hydra.ext.csharp.syntax.ConstantDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) (other);
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
  
  public static final class Field extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.FieldDeclaration value;
    
    public Field (hydra.ext.csharp.syntax.FieldDeclaration value) {
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
  
  public static final class Method extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.MethodDeclaration value;
    
    public Method (hydra.ext.csharp.syntax.MethodDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Method)) {
        return false;
      }
      Method o = (Method) (other);
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
  
  public static final class Property extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.PropertyDeclaration value;
    
    public Property (hydra.ext.csharp.syntax.PropertyDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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
  
  public static final class Event extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.EventDeclaration value;
    
    public Event (hydra.ext.csharp.syntax.EventDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Event)) {
        return false;
      }
      Event o = (Event) (other);
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
  
  public static final class Indexer extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.IndexerDeclaration value;
    
    public Indexer (hydra.ext.csharp.syntax.IndexerDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Indexer)) {
        return false;
      }
      Indexer o = (Indexer) (other);
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
  
  public static final class Operator extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.OperatorDeclaration value;
    
    public Operator (hydra.ext.csharp.syntax.OperatorDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Operator)) {
        return false;
      }
      Operator o = (Operator) (other);
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
  
  public static final class Constructor extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.ConstructorDeclaration value;
    
    public Constructor (hydra.ext.csharp.syntax.ConstructorDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constructor)) {
        return false;
      }
      Constructor o = (Constructor) (other);
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
  
  public static final class Finalizer extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.FinalizerDeclaration value;
    
    public Finalizer (hydra.ext.csharp.syntax.FinalizerDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Finalizer)) {
        return false;
      }
      Finalizer o = (Finalizer) (other);
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
  
  public static final class StaticConstructor extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.StaticConstructorDeclaration value;
    
    public StaticConstructor (hydra.ext.csharp.syntax.StaticConstructorDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticConstructor)) {
        return false;
      }
      StaticConstructor o = (StaticConstructor) (other);
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
  
  public static final class Type extends hydra.ext.csharp.syntax.ClassMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.TypeDeclaration value;
    
    public Type (hydra.ext.csharp.syntax.TypeDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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