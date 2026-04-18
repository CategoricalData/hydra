// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Source of a property value
 */
public abstract class PropertySource implements Serializable, Comparable<PropertySource> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertySource");

  public static final hydra.core.Name NODE_PROPERTY = new hydra.core.Name("nodeProperty");

  public static final hydra.core.Name EDGE_PROPERTY = new hydra.core.Name("edgeProperty");

  public static final hydra.core.Name PATH_PROPERTY = new hydra.core.Name("pathProperty");

  private PropertySource () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NodeProperty instance) ;

    R visit(EdgeProperty instance) ;

    R visit(PathProperty instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertySource instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NodeProperty instance) {
      return otherwise(instance);
    }

    default R visit(EdgeProperty instance) {
      return otherwise(instance);
    }

    default R visit(PathProperty instance) {
      return otherwise(instance);
    }
  }

  public static final class NodeProperty extends com.gdblab.pathAlgebra.expressions.PropertySource implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.NodePropertyRef value;

    public NodeProperty (com.gdblab.pathAlgebra.expressions.NodePropertyRef value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NodeProperty)) {
        return false;
      }
      NodeProperty o = (NodeProperty) other;
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
    public int compareTo(PropertySource other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NodeProperty o = (NodeProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EdgeProperty extends com.gdblab.pathAlgebra.expressions.PropertySource implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.EdgePropertyRef value;

    public EdgeProperty (com.gdblab.pathAlgebra.expressions.EdgePropertyRef value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EdgeProperty)) {
        return false;
      }
      EdgeProperty o = (EdgeProperty) other;
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
    public int compareTo(PropertySource other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EdgeProperty o = (EdgeProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PathProperty extends com.gdblab.pathAlgebra.expressions.PropertySource implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.PathPropertyRef value;

    public PathProperty (com.gdblab.pathAlgebra.expressions.PathPropertyRef value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PathProperty)) {
        return false;
      }
      PathProperty o = (PathProperty) other;
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
    public int compareTo(PropertySource other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PathProperty o = (PathProperty) other;
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
