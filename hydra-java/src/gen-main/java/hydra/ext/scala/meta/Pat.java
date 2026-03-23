// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Pat implements Serializable, Comparable<Pat> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat");

  public static final hydra.core.Name VAR = new hydra.core.Name("var");

  public static final hydra.core.Name WILDCARD = new hydra.core.Name("wildcard");

  public static final hydra.core.Name SEQ_WILDCARD = new hydra.core.Name("seqWildcard");

  public static final hydra.core.Name BIND = new hydra.core.Name("bind");

  public static final hydra.core.Name ALTERNATIVE = new hydra.core.Name("alternative");

  public static final hydra.core.Name TUPLE = new hydra.core.Name("tuple");

  public static final hydra.core.Name REPEATED = new hydra.core.Name("repeated");

  public static final hydra.core.Name EXTRACT = new hydra.core.Name("extract");

  public static final hydra.core.Name EXTRACT_INFIX = new hydra.core.Name("extractInfix");

  public static final hydra.core.Name INTERPOLATE = new hydra.core.Name("interpolate");

  public static final hydra.core.Name XML = new hydra.core.Name("xml");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name MACRO = new hydra.core.Name("macro");

  public static final hydra.core.Name GIVEN = new hydra.core.Name("given");

  private Pat () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Var instance) ;

    R visit(Wildcard instance) ;

    R visit(SeqWildcard instance) ;

    R visit(Bind instance) ;

    R visit(Alternative instance) ;

    R visit(Tuple instance) ;

    R visit(Repeated instance) ;

    R visit(Extract instance) ;

    R visit(ExtractInfix instance) ;

    R visit(Interpolate instance) ;

    R visit(Xml instance) ;

    R visit(Typed instance) ;

    R visit(Macro instance) ;

    R visit(Given instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pat instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Var instance) {
      return otherwise(instance);
    }

    default R visit(Wildcard instance) {
      return otherwise(instance);
    }

    default R visit(SeqWildcard instance) {
      return otherwise(instance);
    }

    default R visit(Bind instance) {
      return otherwise(instance);
    }

    default R visit(Alternative instance) {
      return otherwise(instance);
    }

    default R visit(Tuple instance) {
      return otherwise(instance);
    }

    default R visit(Repeated instance) {
      return otherwise(instance);
    }

    default R visit(Extract instance) {
      return otherwise(instance);
    }

    default R visit(ExtractInfix instance) {
      return otherwise(instance);
    }

    default R visit(Interpolate instance) {
      return otherwise(instance);
    }

    default R visit(Xml instance) {
      return otherwise(instance);
    }

    default R visit(Typed instance) {
      return otherwise(instance);
    }

    default R visit(Macro instance) {
      return otherwise(instance);
    }

    default R visit(Given instance) {
      return otherwise(instance);
    }
  }

  public static final class Var extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Var value;

    public Var (hydra.ext.scala.meta.Pat_Var value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Var o = (Var) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Wildcard extends hydra.ext.scala.meta.Pat implements Serializable {
    public Wildcard () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Pat other) {
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

  public static final class SeqWildcard extends hydra.ext.scala.meta.Pat implements Serializable {
    public SeqWildcard () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SeqWildcard)) {
        return false;
      }
      SeqWildcard o = (SeqWildcard) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Pat other) {
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

  public static final class Bind extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Bind value;

    public Bind (hydra.ext.scala.meta.Pat_Bind value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bind)) {
        return false;
      }
      Bind o = (Bind) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Bind o = (Bind) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Alternative extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Alternative value;

    public Alternative (hydra.ext.scala.meta.Pat_Alternative value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alternative)) {
        return false;
      }
      Alternative o = (Alternative) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Alternative o = (Alternative) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Tuple extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Tuple value;

    public Tuple (hydra.ext.scala.meta.Pat_Tuple value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tuple o = (Tuple) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Repeated extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Repeated value;

    public Repeated (hydra.ext.scala.meta.Pat_Repeated value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Repeated o = (Repeated) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Extract extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Extract value;

    public Extract (hydra.ext.scala.meta.Pat_Extract value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extract)) {
        return false;
      }
      Extract o = (Extract) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Extract o = (Extract) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ExtractInfix extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_ExtractInfix value;

    public ExtractInfix (hydra.ext.scala.meta.Pat_ExtractInfix value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExtractInfix)) {
        return false;
      }
      ExtractInfix o = (ExtractInfix) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ExtractInfix o = (ExtractInfix) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Interpolate extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Interpolate value;

    public Interpolate (hydra.ext.scala.meta.Pat_Interpolate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interpolate)) {
        return false;
      }
      Interpolate o = (Interpolate) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Interpolate o = (Interpolate) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Xml extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Xml value;

    public Xml (hydra.ext.scala.meta.Pat_Xml value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xml)) {
        return false;
      }
      Xml o = (Xml) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Xml o = (Xml) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Typed extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Typed value;

    public Typed (hydra.ext.scala.meta.Pat_Typed value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typed)) {
        return false;
      }
      Typed o = (Typed) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Typed o = (Typed) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Macro extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Macro value;

    public Macro (hydra.ext.scala.meta.Pat_Macro value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Macro)) {
        return false;
      }
      Macro o = (Macro) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Macro o = (Macro) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Given extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Given value;

    public Given (hydra.ext.scala.meta.Pat_Given value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Given)) {
        return false;
      }
      Given o = (Given) other;
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
    public int compareTo(Pat other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Given o = (Given) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
