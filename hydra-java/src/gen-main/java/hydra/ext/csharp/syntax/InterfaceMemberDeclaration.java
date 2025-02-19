// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class InterfaceMemberDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceMemberDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_METHOD = new hydra.core.Name("method");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_EVENT = new hydra.core.Name("event");
  
  public static final hydra.core.Name FIELD_NAME_INDEXER = new hydra.core.Name("indexer");
  
  private InterfaceMemberDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Method instance) ;
    
    R visit(Property instance) ;
    
    R visit(Event instance) ;
    
    R visit(Indexer instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InterfaceMemberDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
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
  }
  
  public static final class Method extends hydra.ext.csharp.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.InterfaceMethodDeclaration value;
    
    public Method (hydra.ext.csharp.syntax.InterfaceMethodDeclaration value) {
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
  
  public static final class Property extends hydra.ext.csharp.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.InterfacePropertyDeclaration value;
    
    public Property (hydra.ext.csharp.syntax.InterfacePropertyDeclaration value) {
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
  
  public static final class Event extends hydra.ext.csharp.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.InterfaceEventDeclaration value;
    
    public Event (hydra.ext.csharp.syntax.InterfaceEventDeclaration value) {
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
  
  public static final class Indexer extends hydra.ext.csharp.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.InterfaceIndexerDeclaration value;
    
    public Indexer (hydra.ext.csharp.syntax.InterfaceIndexerDeclaration value) {
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
}