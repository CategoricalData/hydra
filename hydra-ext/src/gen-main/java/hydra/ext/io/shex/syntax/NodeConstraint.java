// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NodeConstraint implements Serializable, Comparable<NodeConstraint> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint");
  
  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name SEQUENCE2 = new hydra.core.Name("sequence2");
  
  public static final hydra.core.Name SEQUENCE3 = new hydra.core.Name("sequence3");
  
  public static final hydra.core.Name SEQUENCE4 = new hydra.core.Name("sequence4");
  
  public static final hydra.core.Name SEQUENCE5 = new hydra.core.Name("sequence5");
  
  public static final hydra.core.Name LIST_OF_XS_FACET = new hydra.core.Name("listOfXsFacet");
  
  private NodeConstraint () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Sequence2 instance) ;
    
    R visit(Sequence3 instance) ;
    
    R visit(Sequence4 instance) ;
    
    R visit(Sequence5 instance) ;
    
    R visit(ListOfXsFacet instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeConstraint instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Sequence instance) {
      return otherwise(instance);
    }
    
    default R visit(Sequence2 instance) {
      return otherwise(instance);
    }
    
    default R visit(Sequence3 instance) {
      return otherwise(instance);
    }
    
    default R visit(Sequence4 instance) {
      return otherwise(instance);
    }
    
    default R visit(Sequence5 instance) {
      return otherwise(instance);
    }
    
    default R visit(ListOfXsFacet instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Sequence extends hydra.ext.io.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> value;
    
    public Sequence (hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) other;
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
    public int compareTo(NodeConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sequence2 extends hydra.ext.io.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.ext.io.shex.syntax.NodeConstraint_Sequence2 value;
    
    public Sequence2 (hydra.ext.io.shex.syntax.NodeConstraint_Sequence2 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) other;
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
    public int compareTo(NodeConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence2 o = (Sequence2) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sequence3 extends hydra.ext.io.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.ext.io.shex.syntax.NodeConstraint_Sequence3 value;
    
    public Sequence3 (hydra.ext.io.shex.syntax.NodeConstraint_Sequence3 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence3)) {
        return false;
      }
      Sequence3 o = (Sequence3) other;
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
    public int compareTo(NodeConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence3 o = (Sequence3) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sequence4 extends hydra.ext.io.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.ext.io.shex.syntax.NodeConstraint_Sequence4 value;
    
    public Sequence4 (hydra.ext.io.shex.syntax.NodeConstraint_Sequence4 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence4)) {
        return false;
      }
      Sequence4 o = (Sequence4) other;
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
    public int compareTo(NodeConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence4 o = (Sequence4) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sequence5 extends hydra.ext.io.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.ext.io.shex.syntax.NodeConstraint_Sequence5 value;
    
    public Sequence5 (hydra.ext.io.shex.syntax.NodeConstraint_Sequence5 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence5)) {
        return false;
      }
      Sequence5 o = (Sequence5) other;
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
    public int compareTo(NodeConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence5 o = (Sequence5) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ListOfXsFacet extends hydra.ext.io.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> value;
    
    public ListOfXsFacet (hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListOfXsFacet)) {
        return false;
      }
      ListOfXsFacet o = (ListOfXsFacet) other;
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
    public int compareTo(NodeConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ListOfXsFacet o = (ListOfXsFacet) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
