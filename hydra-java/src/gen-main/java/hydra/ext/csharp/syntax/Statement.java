// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class Statement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Statement");
  
  public static final hydra.core.Name FIELD_NAME_LABELED = new hydra.core.Name("labeled");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATION = new hydra.core.Name("declaration");
  
  public static final hydra.core.Name FIELD_NAME_EMBEDDED = new hydra.core.Name("embedded");
  
  private Statement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Labeled instance) ;
    
    R visit(Declaration instance) ;
    
    R visit(Embedded instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Statement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Labeled instance) {
      return otherwise((instance));
    }
    
    default R visit(Declaration instance) {
      return otherwise((instance));
    }
    
    default R visit(Embedded instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Labeled extends hydra.ext.csharp.syntax.Statement implements Serializable {
    public final hydra.ext.csharp.syntax.LabeledStatement value;
    
    public Labeled (hydra.ext.csharp.syntax.LabeledStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Labeled)) {
        return false;
      }
      Labeled o = (Labeled) (other);
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
  
  public static final class Declaration extends hydra.ext.csharp.syntax.Statement implements Serializable {
    public final hydra.ext.csharp.syntax.DeclarationStatement value;
    
    public Declaration (hydra.ext.csharp.syntax.DeclarationStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Declaration)) {
        return false;
      }
      Declaration o = (Declaration) (other);
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
  
  public static final class Embedded extends hydra.ext.csharp.syntax.Statement implements Serializable {
    public final hydra.ext.csharp.syntax.EmbeddedStatement value;
    
    public Embedded (hydra.ext.csharp.syntax.EmbeddedStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Embedded)) {
        return false;
      }
      Embedded o = (Embedded) (other);
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