// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public abstract class NodeConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.NodeConstraint");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE2 = new hydra.core.Name("sequence2");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE3 = new hydra.core.Name("sequence3");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE4 = new hydra.core.Name("sequence4");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE5 = new hydra.core.Name("sequence5");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_XS_FACET = new hydra.core.Name("listOfXsFacet");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence3 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence4 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence5 instance) {
      return otherwise((instance));
    }
    
    default R visit(ListOfXsFacet instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.langs.shex.syntax.NodeConstraint implements Serializable {
    public final java.util.List<hydra.langs.shex.syntax.XsFacet> value;
    
    public Sequence (java.util.List<hydra.langs.shex.syntax.XsFacet> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class Sequence2 extends hydra.langs.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.langs.shex.syntax.NodeConstraint_Sequence2 value;
    
    public Sequence2 (hydra.langs.shex.syntax.NodeConstraint_Sequence2 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) (other);
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
  
  public static final class Sequence3 extends hydra.langs.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.langs.shex.syntax.NodeConstraint_Sequence3 value;
    
    public Sequence3 (hydra.langs.shex.syntax.NodeConstraint_Sequence3 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence3)) {
        return false;
      }
      Sequence3 o = (Sequence3) (other);
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
  
  public static final class Sequence4 extends hydra.langs.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.langs.shex.syntax.NodeConstraint_Sequence4 value;
    
    public Sequence4 (hydra.langs.shex.syntax.NodeConstraint_Sequence4 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence4)) {
        return false;
      }
      Sequence4 o = (Sequence4) (other);
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
  
  public static final class Sequence5 extends hydra.langs.shex.syntax.NodeConstraint implements Serializable {
    public final hydra.langs.shex.syntax.NodeConstraint_Sequence5 value;
    
    public Sequence5 (hydra.langs.shex.syntax.NodeConstraint_Sequence5 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence5)) {
        return false;
      }
      Sequence5 o = (Sequence5) (other);
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
  
  public static final class ListOfXsFacet extends hydra.langs.shex.syntax.NodeConstraint implements Serializable {
    public final java.util.List<hydra.langs.shex.syntax.XsFacet> value;
    
    public ListOfXsFacet (java.util.List<hydra.langs.shex.syntax.XsFacet> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListOfXsFacet)) {
        return false;
      }
      ListOfXsFacet o = (ListOfXsFacet) (other);
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