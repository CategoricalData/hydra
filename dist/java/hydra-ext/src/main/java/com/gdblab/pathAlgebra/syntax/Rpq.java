// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public abstract class Rpq implements Serializable, Comparable<Rpq> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.Rpq");

  public static final hydra.core.Name PARENTHESIS = new hydra.core.Name("parenthesis");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name NEGATED = new hydra.core.Name("negated");

  public static final hydra.core.Name REVERSE = new hydra.core.Name("reverse");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  public static final hydra.core.Name PLUS = new hydra.core.Name("plus");

  public static final hydra.core.Name STAR = new hydra.core.Name("star");

  public static final hydra.core.Name CONCATENATION = new hydra.core.Name("concatenation");

  public static final hydra.core.Name ALTERNATION = new hydra.core.Name("alternation");

  private Rpq () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Parenthesis instance) ;

    R visit(Label instance) ;

    R visit(Negated instance) ;

    R visit(Reverse instance) ;

    R visit(Optional instance) ;

    R visit(Plus instance) ;

    R visit(Star instance) ;

    R visit(Concatenation instance) ;

    R visit(Alternation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Rpq instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Parenthesis instance) {
      return otherwise(instance);
    }

    default R visit(Label instance) {
      return otherwise(instance);
    }

    default R visit(Negated instance) {
      return otherwise(instance);
    }

    default R visit(Reverse instance) {
      return otherwise(instance);
    }

    default R visit(Optional instance) {
      return otherwise(instance);
    }

    default R visit(Plus instance) {
      return otherwise(instance);
    }

    default R visit(Star instance) {
      return otherwise(instance);
    }

    default R visit(Concatenation instance) {
      return otherwise(instance);
    }

    default R visit(Alternation instance) {
      return otherwise(instance);
    }
  }

  public static final class Parenthesis extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Rpq value;

    public Parenthesis (com.gdblab.pathAlgebra.syntax.Rpq value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesis)) {
        return false;
      }
      Parenthesis o = (Parenthesis) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parenthesis o = (Parenthesis) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Label extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final String value;

    public Label (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Label o = (Label) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Negated extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final String value;

    public Negated (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negated)) {
        return false;
      }
      Negated o = (Negated) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Negated o = (Negated) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Reverse extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final String value;

    public Reverse (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reverse)) {
        return false;
      }
      Reverse o = (Reverse) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Reverse o = (Reverse) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Optional extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Rpq value;

    public Optional (com.gdblab.pathAlgebra.syntax.Rpq value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Optional o = (Optional) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Plus extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Plus value;

    public Plus (com.gdblab.pathAlgebra.syntax.Plus value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Plus o = (Plus) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Star extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Star value;

    public Star (com.gdblab.pathAlgebra.syntax.Star value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Star o = (Star) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Concatenation extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Concatenation value;

    public Concatenation (com.gdblab.pathAlgebra.syntax.Concatenation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Concatenation)) {
        return false;
      }
      Concatenation o = (Concatenation) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Concatenation o = (Concatenation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Alternation extends com.gdblab.pathAlgebra.syntax.Rpq implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Alternation value;

    public Alternation (com.gdblab.pathAlgebra.syntax.Alternation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alternation)) {
        return false;
      }
      Alternation o = (Alternation) other;
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
    public int compareTo(Rpq other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Alternation o = (Alternation) other;
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
