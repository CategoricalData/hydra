// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSourceSpawnMethod implements Serializable, Comparable<TraversalSourceSpawnMethod> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod");
  
  public static final hydra.core.Name ADD_E = new hydra.core.Name("addE");
  
  public static final hydra.core.Name ADD_V = new hydra.core.Name("addV");
  
  public static final hydra.core.Name E = new hydra.core.Name("e");
  
  public static final hydra.core.Name V = new hydra.core.Name("v");
  
  public static final hydra.core.Name MERGE_V = new hydra.core.Name("mergeV");
  
  public static final hydra.core.Name MERGE_E = new hydra.core.Name("mergeE");
  
  public static final hydra.core.Name INJECT = new hydra.core.Name("inject");
  
  public static final hydra.core.Name IO = new hydra.core.Name("io");
  
  public static final hydra.core.Name CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name UNION = new hydra.core.Name("union");
  
  private TraversalSourceSpawnMethod () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AddE instance) ;
    
    R visit(AddV instance) ;
    
    R visit(E instance) ;
    
    R visit(V instance) ;
    
    R visit(MergeV instance) ;
    
    R visit(MergeE instance) ;
    
    R visit(Inject instance) ;
    
    R visit(Io instance) ;
    
    R visit(Call instance) ;
    
    R visit(Union instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalSourceSpawnMethod instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(AddE instance) {
      return otherwise(instance);
    }
    
    default R visit(AddV instance) {
      return otherwise(instance);
    }
    
    default R visit(E instance) {
      return otherwise(instance);
    }
    
    default R visit(V instance) {
      return otherwise(instance);
    }
    
    default R visit(MergeV instance) {
      return otherwise(instance);
    }
    
    default R visit(MergeE instance) {
      return otherwise(instance);
    }
    
    default R visit(Inject instance) {
      return otherwise(instance);
    }
    
    default R visit(Io instance) {
      return otherwise(instance);
    }
    
    default R visit(Call instance) {
      return otherwise(instance);
    }
    
    default R visit(Union instance) {
      return otherwise(instance);
    }
  }
  
  public static final class AddE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal value;
    
    public AddE (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddE)) {
        return false;
      }
      AddE o = (AddE) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AddE o = (AddE) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AddV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value;
    
    public AddV (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddV)) {
        return false;
      }
      AddV o = (AddV) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AddV o = (AddV) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class E extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public E (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof E)) {
        return false;
      }
      E o = (E) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      E o = (E) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class V extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public V (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof V)) {
        return false;
      }
      V o = (V) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      V o = (V) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MergeV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value;
    
    public MergeV (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeV)) {
        return false;
      }
      MergeV o = (MergeV) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MergeV o = (MergeV) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MergeE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value;
    
    public MergeE (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeE)) {
        return false;
      }
      MergeE o = (MergeE) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MergeE o = (MergeE) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Inject extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Inject (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inject)) {
        return false;
      }
      Inject o = (Inject) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Inject o = (Inject) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Io extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Io (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Io)) {
        return false;
      }
      Io o = (Io) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Io o = (Io) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Call extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall> value;
    
    public Call (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Call o = (Call) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Union extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value;
    
    public Union (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
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
    public int compareTo(TraversalSourceSpawnMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Union o = (Union) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
