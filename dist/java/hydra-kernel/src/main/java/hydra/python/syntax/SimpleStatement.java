// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class SimpleStatement implements Serializable, Comparable<SimpleStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.SimpleStatement");

  public static final hydra.core.Name ASSIGNMENT = new hydra.core.Name("assignment");

  public static final hydra.core.Name TYPE_ALIAS = new hydra.core.Name("typeAlias");

  public static final hydra.core.Name STAR_EXPRESSIONS = new hydra.core.Name("starExpressions");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public static final hydra.core.Name IMPORT = new hydra.core.Name("import");

  public static final hydra.core.Name RAISE = new hydra.core.Name("raise");

  public static final hydra.core.Name PASS = new hydra.core.Name("pass");

  public static final hydra.core.Name DEL = new hydra.core.Name("del");

  public static final hydra.core.Name YIELD = new hydra.core.Name("yield");

  public static final hydra.core.Name ASSERT = new hydra.core.Name("assert");

  public static final hydra.core.Name BREAK = new hydra.core.Name("break");

  public static final hydra.core.Name CONTINUE = new hydra.core.Name("continue");

  public static final hydra.core.Name GLOBAL = new hydra.core.Name("global");

  public static final hydra.core.Name NONLOCAL = new hydra.core.Name("nonlocal");

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

  public static final class Assignment extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.Assignment value;

    public Assignment (hydra.python.syntax.Assignment value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assignment o = (Assignment) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TypeAlias extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.TypeAlias value;

    public TypeAlias (hydra.python.syntax.TypeAlias value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeAlias o = (TypeAlias) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class StarExpressions extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final java.util.List<hydra.python.syntax.StarExpression> value;

    public StarExpressions (java.util.List<hydra.python.syntax.StarExpression> value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarExpressions o = (StarExpressions) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Return extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.ReturnStatement value;

    public Return (hydra.python.syntax.ReturnStatement value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Return o = (Return) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Import extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.ImportStatement value;

    public Import (hydra.python.syntax.ImportStatement value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Import o = (Import) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Raise extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.RaiseStatement value;

    public Raise (hydra.python.syntax.RaiseStatement value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Raise o = (Raise) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Pass extends hydra.python.syntax.SimpleStatement implements Serializable {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Del extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.DelStatement value;

    public Del (hydra.python.syntax.DelStatement value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Del o = (Del) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Yield extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.YieldStatement value;

    public Yield (hydra.python.syntax.YieldStatement value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Yield o = (Yield) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Assert extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final hydra.python.syntax.AssertStatement value;

    public Assert (hydra.python.syntax.AssertStatement value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assert o = (Assert) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Break extends hydra.python.syntax.SimpleStatement implements Serializable {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Continue extends hydra.python.syntax.SimpleStatement implements Serializable {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Global extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final java.util.List<hydra.python.syntax.Name> value;

    public Global (java.util.List<hydra.python.syntax.Name> value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Global o = (Global) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Nonlocal extends hydra.python.syntax.SimpleStatement implements Serializable {
    public final java.util.List<hydra.python.syntax.Name> value;

    public Nonlocal (java.util.List<hydra.python.syntax.Name> value) {
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
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nonlocal o = (Nonlocal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
