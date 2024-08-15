// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSourceSpawnMethod implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalSourceSpawnMethod");
  
  public static final hydra.core.Name FIELD_NAME_ADD_E = new hydra.core.Name("addE");
  
  public static final hydra.core.Name FIELD_NAME_ADD_V = new hydra.core.Name("addV");
  
  public static final hydra.core.Name FIELD_NAME_E = new hydra.core.Name("e");
  
  public static final hydra.core.Name FIELD_NAME_V = new hydra.core.Name("v");
  
  public static final hydra.core.Name FIELD_NAME_MERGE_V = new hydra.core.Name("mergeV");
  
  public static final hydra.core.Name FIELD_NAME_MERGE_E = new hydra.core.Name("mergeE");
  
  public static final hydra.core.Name FIELD_NAME_INJECT = new hydra.core.Name("inject");
  
  public static final hydra.core.Name FIELD_NAME_IO = new hydra.core.Name("io");
  
  public static final hydra.core.Name FIELD_NAME_CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AddE instance) {
      return otherwise((instance));
    }
    
    default R visit(AddV instance) {
      return otherwise((instance));
    }
    
    default R visit(E instance) {
      return otherwise((instance));
    }
    
    default R visit(V instance) {
      return otherwise((instance));
    }
    
    default R visit(MergeV instance) {
      return otherwise((instance));
    }
    
    default R visit(MergeE instance) {
      return otherwise((instance));
    }
    
    default R visit(Inject instance) {
      return otherwise((instance));
    }
    
    default R visit(Io instance) {
      return otherwise((instance));
    }
    
    default R visit(Call instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AddE extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal value;
    
    public AddE (hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddE)) {
        return false;
      }
      AddE o = (AddE) (other);
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
  
  public static final class AddV extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value;
    
    public AddV (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddV)) {
        return false;
      }
      AddV o = (AddV) (other);
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
  
  public static final class E extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public E (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof E)) {
        return false;
      }
      E o = (E) (other);
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
  
  public static final class V extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public V (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof V)) {
        return false;
      }
      V o = (V) (other);
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
  
  public static final class MergeV extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value;
    
    public MergeV (hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeV)) {
        return false;
      }
      MergeV o = (MergeV) (other);
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
  
  public static final class MergeE extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value;
    
    public MergeE (hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeE)) {
        return false;
      }
      MergeE o = (MergeE) (other);
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
  
  public static final class Inject extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Inject (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inject)) {
        return false;
      }
      Inject o = (Inject) (other);
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
  
  public static final class Io extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public Io (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Io)) {
        return false;
      }
      Io o = (Io) (other);
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
  
  public static final class Call extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.ServiceCall> value;
    
    public Call (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.ServiceCall> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) (other);
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
  
  public static final class Union extends hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value;
    
    public Union (java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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