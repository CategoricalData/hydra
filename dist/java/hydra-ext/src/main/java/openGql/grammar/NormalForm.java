// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NormalForm implements Serializable, Comparable<NormalForm> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NormalForm");

  public static final hydra.core.Name NFC = new hydra.core.Name("nfc");

  public static final hydra.core.Name NFD = new hydra.core.Name("nfd");

  public static final hydra.core.Name NFKC = new hydra.core.Name("nfkc");

  public static final hydra.core.Name NFKD = new hydra.core.Name("nfkd");

  private NormalForm () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Nfc instance) ;

    R visit(Nfd instance) ;

    R visit(Nfkc instance) ;

    R visit(Nfkd instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NormalForm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Nfc instance) {
      return otherwise(instance);
    }

    default R visit(Nfd instance) {
      return otherwise(instance);
    }

    default R visit(Nfkc instance) {
      return otherwise(instance);
    }

    default R visit(Nfkd instance) {
      return otherwise(instance);
    }
  }

  public static final class Nfc extends openGql.grammar.NormalForm implements Serializable {
    public Nfc () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nfc)) {
        return false;
      }
      Nfc o = (Nfc) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NormalForm other) {
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

  public static final class Nfd extends openGql.grammar.NormalForm implements Serializable {
    public Nfd () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nfd)) {
        return false;
      }
      Nfd o = (Nfd) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NormalForm other) {
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

  public static final class Nfkc extends openGql.grammar.NormalForm implements Serializable {
    public Nfkc () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nfkc)) {
        return false;
      }
      Nfkc o = (Nfkc) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NormalForm other) {
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

  public static final class Nfkd extends openGql.grammar.NormalForm implements Serializable {
    public Nfkd () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nfkd)) {
        return false;
      }
      Nfkd o = (Nfkd) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NormalForm other) {
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
}
