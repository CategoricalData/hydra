// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class StarAtom implements Serializable, Comparable<StarAtom> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarAtom");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TARGET_WITH_STAR_ATOM = new hydra.core.Name("targetWithStarAtom");
  
  public static final hydra.core.Name FIELD_NAME_STAR_TARGETS_TUPLE_SEQ = new hydra.core.Name("starTargetsTupleSeq");
  
  public static final hydra.core.Name FIELD_NAME_STAR_TARGETS_LIST_SEQ = new hydra.core.Name("starTargetsListSeq");
  
  private StarAtom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(TargetWithStarAtom instance) ;
    
    R visit(StarTargetsTupleSeq instance) ;
    
    R visit(StarTargetsListSeq instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StarAtom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Name instance) {
      return otherwise(instance);
    }
    
    default R visit(TargetWithStarAtom instance) {
      return otherwise(instance);
    }
    
    default R visit(StarTargetsTupleSeq instance) {
      return otherwise(instance);
    }
    
    default R visit(StarTargetsListSeq instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Name extends hydra.ext.python.syntax.StarAtom implements Serializable {
    public final hydra.ext.python.syntax.Name value;
    
    public Name (hydra.ext.python.syntax.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(StarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TargetWithStarAtom extends hydra.ext.python.syntax.StarAtom implements Serializable {
    public final hydra.ext.python.syntax.TargetWithStarAtom value;
    
    public TargetWithStarAtom (hydra.ext.python.syntax.TargetWithStarAtom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TargetWithStarAtom)) {
        return false;
      }
      TargetWithStarAtom o = (TargetWithStarAtom) other;
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
    public int compareTo(StarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TargetWithStarAtom o = (TargetWithStarAtom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StarTargetsTupleSeq extends hydra.ext.python.syntax.StarAtom implements Serializable {
    public final hydra.util.Maybe<hydra.ext.python.syntax.StarTargetsTupleSeq> value;
    
    public StarTargetsTupleSeq (hydra.util.Maybe<hydra.ext.python.syntax.StarTargetsTupleSeq> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarTargetsTupleSeq)) {
        return false;
      }
      StarTargetsTupleSeq o = (StarTargetsTupleSeq) other;
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
    public int compareTo(StarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarTargetsTupleSeq o = (StarTargetsTupleSeq) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StarTargetsListSeq extends hydra.ext.python.syntax.StarAtom implements Serializable {
    public final hydra.util.Maybe<hydra.ext.python.syntax.StarTargetsListSeq> value;
    
    public StarTargetsListSeq (hydra.util.Maybe<hydra.ext.python.syntax.StarTargetsListSeq> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarTargetsListSeq)) {
        return false;
      }
      StarTargetsListSeq o = (StarTargetsListSeq) other;
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
    public int compareTo(StarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StarTargetsListSeq o = (StarTargetsListSeq) other;
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
