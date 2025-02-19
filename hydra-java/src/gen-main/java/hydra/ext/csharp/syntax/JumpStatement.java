// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class JumpStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.JumpStatement");
  
  public static final hydra.core.Name FIELD_NAME_BREAK = new hydra.core.Name("break");
  
  public static final hydra.core.Name FIELD_NAME_CONTINUE = new hydra.core.Name("continue");
  
  public static final hydra.core.Name FIELD_NAME_GOTO = new hydra.core.Name("goto");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public static final hydra.core.Name FIELD_NAME_THROW = new hydra.core.Name("throw");
  
  private JumpStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Break instance) ;
    
    R visit(Continue instance) ;
    
    R visit(Goto instance) ;
    
    R visit(Return instance) ;
    
    R visit(Throw instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(JumpStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Break instance) {
      return otherwise((instance));
    }
    
    default R visit(Continue instance) {
      return otherwise((instance));
    }
    
    default R visit(Goto instance) {
      return otherwise((instance));
    }
    
    default R visit(Return instance) {
      return otherwise((instance));
    }
    
    default R visit(Throw instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Break extends hydra.ext.csharp.syntax.JumpStatement implements Serializable {
    public Break () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Break)) {
        return false;
      }
      Break o = (Break) (other);
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
  
  public static final class Continue extends hydra.ext.csharp.syntax.JumpStatement implements Serializable {
    public Continue () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Continue)) {
        return false;
      }
      Continue o = (Continue) (other);
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
  
  public static final class Goto extends hydra.ext.csharp.syntax.JumpStatement implements Serializable {
    public final hydra.ext.csharp.syntax.GotoStatement value;
    
    public Goto (hydra.ext.csharp.syntax.GotoStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Goto)) {
        return false;
      }
      Goto o = (Goto) (other);
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
  
  public static final class Return extends hydra.ext.csharp.syntax.JumpStatement implements Serializable {
    public final hydra.ext.csharp.syntax.ReturnStatement value;
    
    public Return (hydra.ext.csharp.syntax.ReturnStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) (other);
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
  
  public static final class Throw extends hydra.ext.csharp.syntax.JumpStatement implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.Expression> value;
    
    public Throw (hydra.util.Opt<hydra.ext.csharp.syntax.Expression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Throw)) {
        return false;
      }
      Throw o = (Throw) (other);
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