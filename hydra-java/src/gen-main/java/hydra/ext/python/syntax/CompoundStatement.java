// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class CompoundStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CompoundStatement");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_DEF = new hydra.core.Name("classDef");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  public static final hydra.core.Name FIELD_NAME_FOR = new hydra.core.Name("for");
  
  public static final hydra.core.Name FIELD_NAME_TRY = new hydra.core.Name("try");
  
  public static final hydra.core.Name FIELD_NAME_WHILE = new hydra.core.Name("while");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  private CompoundStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Function instance) ;
    
    R visit(If instance) ;
    
    R visit(ClassDef instance) ;
    
    R visit(With instance) ;
    
    R visit(For instance) ;
    
    R visit(Try instance) ;
    
    R visit(While instance) ;
    
    R visit(Match instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CompoundStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(If instance) {
      return otherwise((instance));
    }
    
    default R visit(ClassDef instance) {
      return otherwise((instance));
    }
    
    default R visit(With instance) {
      return otherwise((instance));
    }
    
    default R visit(For instance) {
      return otherwise((instance));
    }
    
    default R visit(Try instance) {
      return otherwise((instance));
    }
    
    default R visit(While instance) {
      return otherwise((instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Function extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.FunctionDefinition value;
    
    public Function (hydra.ext.python.syntax.FunctionDefinition value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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
  
  public static final class If extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.IfStatement value;
    
    public If (hydra.ext.python.syntax.IfStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) (other);
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
  
  public static final class ClassDef extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.ClassDefinition value;
    
    public ClassDef (hydra.ext.python.syntax.ClassDefinition value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassDef)) {
        return false;
      }
      ClassDef o = (ClassDef) (other);
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
  
  public static final class With extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.WithStatement value;
    
    public With (hydra.ext.python.syntax.WithStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) (other);
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
  
  public static final class For extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.ForStatement value;
    
    public For (hydra.ext.python.syntax.ForStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) (other);
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
  
  public static final class Try extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.TryStatement value;
    
    public Try (hydra.ext.python.syntax.TryStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Try)) {
        return false;
      }
      Try o = (Try) (other);
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
  
  public static final class While extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.WhileStatement value;
    
    public While (hydra.ext.python.syntax.WhileStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof While)) {
        return false;
      }
      While o = (While) (other);
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
  
  public static final class Match extends hydra.ext.python.syntax.CompoundStatement implements Serializable {
    public final hydra.ext.python.syntax.MatchStatement value;
    
    public Match (hydra.ext.python.syntax.MatchStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) (other);
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