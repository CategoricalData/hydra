// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class TPrimary implements Serializable, Comparable<TPrimary> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimary");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_AND_NAME = new hydra.core.Name("primaryAndName");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_AND_SLICES = new hydra.core.Name("primaryAndSlices");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_AND_GENEXP = new hydra.core.Name("primaryAndGenexp");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_AND_ARGUMENTS = new hydra.core.Name("primaryAndArguments");
  
  public static final hydra.core.Name FIELD_NAME_ATOM = new hydra.core.Name("atom");
  
  private TPrimary () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PrimaryAndName instance) ;
    
    R visit(PrimaryAndSlices instance) ;
    
    R visit(PrimaryAndGenexp instance) ;
    
    R visit(PrimaryAndArguments instance) ;
    
    R visit(Atom instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TPrimary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(PrimaryAndName instance) {
      return otherwise(instance);
    }
    
    default R visit(PrimaryAndSlices instance) {
      return otherwise(instance);
    }
    
    default R visit(PrimaryAndGenexp instance) {
      return otherwise(instance);
    }
    
    default R visit(PrimaryAndArguments instance) {
      return otherwise(instance);
    }
    
    default R visit(Atom instance) {
      return otherwise(instance);
    }
  }
  
  public static final class PrimaryAndName extends hydra.ext.python.syntax.TPrimary implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndName value;
    
    public PrimaryAndName (hydra.ext.python.syntax.TPrimaryAndName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndName)) {
        return false;
      }
      PrimaryAndName o = (PrimaryAndName) other;
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
    public int compareTo(TPrimary other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimaryAndName o = (PrimaryAndName) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PrimaryAndSlices extends hydra.ext.python.syntax.TPrimary implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndSlices value;
    
    public PrimaryAndSlices (hydra.ext.python.syntax.TPrimaryAndSlices value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndSlices)) {
        return false;
      }
      PrimaryAndSlices o = (PrimaryAndSlices) other;
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
    public int compareTo(TPrimary other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimaryAndSlices o = (PrimaryAndSlices) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PrimaryAndGenexp extends hydra.ext.python.syntax.TPrimary implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndGenexp value;
    
    public PrimaryAndGenexp (hydra.ext.python.syntax.TPrimaryAndGenexp value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndGenexp)) {
        return false;
      }
      PrimaryAndGenexp o = (PrimaryAndGenexp) other;
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
    public int compareTo(TPrimary other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimaryAndGenexp o = (PrimaryAndGenexp) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PrimaryAndArguments extends hydra.ext.python.syntax.TPrimary implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndArguments value;
    
    public PrimaryAndArguments (hydra.ext.python.syntax.TPrimaryAndArguments value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndArguments)) {
        return false;
      }
      PrimaryAndArguments o = (PrimaryAndArguments) other;
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
    public int compareTo(TPrimary other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimaryAndArguments o = (PrimaryAndArguments) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Atom extends hydra.ext.python.syntax.TPrimary implements Serializable {
    public final hydra.ext.python.syntax.Atom value;
    
    public Atom (hydra.ext.python.syntax.Atom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Atom)) {
        return false;
      }
      Atom o = (Atom) other;
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
    public int compareTo(TPrimary other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Atom o = (Atom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
