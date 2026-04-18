// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Term0 implements Serializable, Comparable<Term0> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Term0");

  public static final hydra.core.Name QUALID_ANNOTATED = new hydra.core.Name("qualidAnnotated");

  public static final hydra.core.Name SORT = new hydra.core.Name("sort");

  public static final hydra.core.Name PRIMITIVE_NOTATIONS = new hydra.core.Name("primitiveNotations");

  public static final hydra.core.Name EVAR = new hydra.core.Name("evar");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  public static final hydra.core.Name GENERALIZING = new hydra.core.Name("generalizing");

  public static final hydra.core.Name LTAC = new hydra.core.Name("ltac");

  public static final hydra.core.Name PARENS = new hydra.core.Name("parens");

  private Term0 () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(QualidAnnotated instance) ;

    R visit(Sort instance) ;

    R visit(PrimitiveNotations instance) ;

    R visit(Evar instance) ;

    R visit(Match instance) ;

    R visit(Record instance) ;

    R visit(Generalizing instance) ;

    R visit(Ltac instance) ;

    R visit(Parens instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term0 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(QualidAnnotated instance) {
      return otherwise(instance);
    }

    default R visit(Sort instance) {
      return otherwise(instance);
    }

    default R visit(PrimitiveNotations instance) {
      return otherwise(instance);
    }

    default R visit(Evar instance) {
      return otherwise(instance);
    }

    default R visit(Match instance) {
      return otherwise(instance);
    }

    default R visit(Record instance) {
      return otherwise(instance);
    }

    default R visit(Generalizing instance) {
      return otherwise(instance);
    }

    default R visit(Ltac instance) {
      return otherwise(instance);
    }

    default R visit(Parens instance) {
      return otherwise(instance);
    }
  }

  public static final class QualidAnnotated extends hydra.coq.syntax.Term0 implements Serializable {
    public final hydra.coq.syntax.QualidAnnotated value;

    public QualidAnnotated (hydra.coq.syntax.QualidAnnotated value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualidAnnotated)) {
        return false;
      }
      QualidAnnotated o = (QualidAnnotated) other;
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
    public int compareTo(Term0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QualidAnnotated o = (QualidAnnotated) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Sort extends hydra.coq.syntax.Term0 implements Serializable {
    public final hydra.coq.syntax.Sort value;

    public Sort (hydra.coq.syntax.Sort value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sort)) {
        return false;
      }
      Sort o = (Sort) other;
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
    public int compareTo(Term0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sort o = (Sort) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PrimitiveNotations extends hydra.coq.syntax.Term0 implements Serializable {
    public final hydra.coq.syntax.PrimitiveNotations value;

    public PrimitiveNotations (hydra.coq.syntax.PrimitiveNotations value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimitiveNotations)) {
        return false;
      }
      PrimitiveNotations o = (PrimitiveNotations) other;
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
    public int compareTo(Term0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimitiveNotations o = (PrimitiveNotations) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Evar extends hydra.coq.syntax.Term0 implements Serializable {
    public final hydra.coq.syntax.ExistentialVariable value;

    public Evar (hydra.coq.syntax.ExistentialVariable value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Evar)) {
        return false;
      }
      Evar o = (Evar) other;
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
    public int compareTo(Term0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Evar o = (Evar) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Match extends hydra.coq.syntax.Term0 implements Serializable {
    public final hydra.coq.syntax.Match value;

    public Match (hydra.coq.syntax.Match value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) other;
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
    public int compareTo(Term0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Match o = (Match) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Record extends hydra.coq.syntax.Term0 implements Serializable {
    public Record () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term0 other) {
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

  public static final class Generalizing extends hydra.coq.syntax.Term0 implements Serializable {
    public Generalizing () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Generalizing)) {
        return false;
      }
      Generalizing o = (Generalizing) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term0 other) {
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

  public static final class Ltac extends hydra.coq.syntax.Term0 implements Serializable {
    public Ltac () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ltac)) {
        return false;
      }
      Ltac o = (Ltac) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term0 other) {
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

  public static final class Parens extends hydra.coq.syntax.Term0 implements Serializable {
    public final hydra.coq.syntax.Term value;

    public Parens (hydra.coq.syntax.Term value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) other;
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
    public int compareTo(Term0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parens o = (Parens) other;
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
