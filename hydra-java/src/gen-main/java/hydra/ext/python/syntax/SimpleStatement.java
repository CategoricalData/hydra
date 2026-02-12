// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SimpleStatement implements Serializable, Comparable<SimpleStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SimpleStatement");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGNMENT = new hydra.core.Name("assignment");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ALIAS = new hydra.core.Name("typeAlias");
  
  public static final hydra.core.Name FIELD_NAME_STAR_EXPRESSIONS = new hydra.core.Name("starExpressions");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public static final hydra.core.Name FIELD_NAME_IMPORT = new hydra.core.Name("import");
  
  public static final hydra.core.Name FIELD_NAME_RAISE = new hydra.core.Name("raise");
  
  public static final hydra.core.Name FIELD_NAME_PASS = new hydra.core.Name("pass");
  
  public static final hydra.core.Name FIELD_NAME_DEL = new hydra.core.Name("del");
  
  public static final hydra.core.Name FIELD_NAME_YIELD = new hydra.core.Name("yield");
  
  public static final hydra.core.Name FIELD_NAME_ASSERT = new hydra.core.Name("assert");
  
  public static final hydra.core.Name FIELD_NAME_BREAK = new hydra.core.Name("break");
  
  public static final hydra.core.Name FIELD_NAME_CONTINUE = new hydra.core.Name("continue");
  
  public static final hydra.core.Name FIELD_NAME_GLOBAL = new hydra.core.Name("global");
  
  public static final hydra.core.Name FIELD_NAME_NONLOCAL = new hydra.core.Name("nonlocal");
  
  private SimpleStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Assignment instance) ;
    
    R visit(TypeAlias instance) ;
    
    R visit(StarExpressions instance) ;
    
    R visit(Return instance) ;
    
    R visit(Import instance) ;
    
    R visit(Raise instance) ;
    
    R visit(Pass instance) ;
    
    R visit(Del instance) ;
    
    R visit(Yield instance) ;
    
    R visit(Assert instance) ;
    
    R visit(Break instance) ;
    
    R visit(Continue instance) ;
    
    R visit(Global instance) ;
    
    R visit(Nonlocal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimpleStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Assignment instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeAlias instance) {
      return otherwise(instance);
    }
    
    default R visit(StarExpressions instance) {
      return otherwise(instance);
    }
    
    default R visit(Return instance) {
      return otherwise(instance);
    }
    
    default R visit(Import instance) {
      return otherwise(instance);
    }
    
    default R visit(Raise instance) {
      return otherwise(instance);
    }
    
    default R visit(Pass instance) {
      return otherwise(instance);
    }
    
    default R visit(Del instance) {
      return otherwise(instance);
    }
    
    default R visit(Yield instance) {
      return otherwise(instance);
    }
    
    default R visit(Assert instance) {
      return otherwise(instance);
    }
    
    default R visit(Break instance) {
      return otherwise(instance);
    }
    
    default R visit(Continue instance) {
      return otherwise(instance);
    }
    
    default R visit(Global instance) {
      return otherwise(instance);
    }
    
    default R visit(Nonlocal instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Assignment extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.Assignment value;
    
    public Assignment (hydra.ext.python.syntax.Assignment value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assignment)) {
        return false;
      }
      Assignment o = (Assignment) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assignment o = (Assignment) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TypeAlias extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.TypeAlias value;
    
    public TypeAlias (hydra.ext.python.syntax.TypeAlias value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeAlias)) {
        return false;
      }
      TypeAlias o = (TypeAlias) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeAlias o = (TypeAlias) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StarExpressions extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.StarExpression> value;
    
    public StarExpressions (java.util.List<hydra.ext.python.syntax.StarExpression> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarExpressions)) {
        return false;
      }
      StarExpressions o = (StarExpressions) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarExpressions o = (StarExpressions) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Return extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.ReturnStatement value;
    
    public Return (hydra.ext.python.syntax.ReturnStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Return o = (Return) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Import extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.ImportStatement value;
    
    public Import (hydra.ext.python.syntax.ImportStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Import)) {
        return false;
      }
      Import o = (Import) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Import o = (Import) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Raise extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.RaiseStatement value;
    
    public Raise (hydra.ext.python.syntax.RaiseStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Raise)) {
        return false;
      }
      Raise o = (Raise) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Raise o = (Raise) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Pass extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public Pass () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pass)) {
        return false;
      }
      Pass o = (Pass) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Del extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.DelStatement value;
    
    public Del (hydra.ext.python.syntax.DelStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Del)) {
        return false;
      }
      Del o = (Del) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Del o = (Del) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Yield extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.YieldStatement value;
    
    public Yield (hydra.ext.python.syntax.YieldStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Yield)) {
        return false;
      }
      Yield o = (Yield) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Yield o = (Yield) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Assert extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final hydra.ext.python.syntax.AssertStatement value;
    
    public Assert (hydra.ext.python.syntax.AssertStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assert)) {
        return false;
      }
      Assert o = (Assert) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assert o = (Assert) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Break extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public Break () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Break)) {
        return false;
      }
      Break o = (Break) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Continue extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public Continue () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Continue)) {
        return false;
      }
      Continue o = (Continue) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Global extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.Name> value;
    
    public Global (java.util.List<hydra.ext.python.syntax.Name> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Global)) {
        return false;
      }
      Global o = (Global) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Global o = (Global) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Nonlocal extends hydra.ext.python.syntax.SimpleStatement implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.Name> value;
    
    public Nonlocal (java.util.List<hydra.ext.python.syntax.Name> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nonlocal)) {
        return false;
      }
      Nonlocal o = (Nonlocal) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SimpleStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nonlocal o = (Nonlocal) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
