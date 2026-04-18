// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Ordering criteria corresponding to paper's τ variants
 */
public abstract class OrderByCriterion implements Serializable, Comparable<OrderByCriterion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.OrderByCriterion");

  public static final hydra.core.Name PARTITION = new hydra.core.Name("partition");

  public static final hydra.core.Name GROUP = new hydra.core.Name("group");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name PARTITION_GROUP = new hydra.core.Name("partitionGroup");

  public static final hydra.core.Name PARTITION_PATH = new hydra.core.Name("partitionPath");

  public static final hydra.core.Name GROUP_PATH = new hydra.core.Name("groupPath");

  public static final hydra.core.Name PARTITION_GROUP_PATH = new hydra.core.Name("partitionGroupPath");

  private OrderByCriterion () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Partition instance) ;

    R visit(Group instance) ;

    R visit(Path instance) ;

    R visit(PartitionGroup instance) ;

    R visit(PartitionPath instance) ;

    R visit(GroupPath instance) ;

    R visit(PartitionGroupPath instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OrderByCriterion instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Partition instance) {
      return otherwise(instance);
    }

    default R visit(Group instance) {
      return otherwise(instance);
    }

    default R visit(Path instance) {
      return otherwise(instance);
    }

    default R visit(PartitionGroup instance) {
      return otherwise(instance);
    }

    default R visit(PartitionPath instance) {
      return otherwise(instance);
    }

    default R visit(GroupPath instance) {
      return otherwise(instance);
    }

    default R visit(PartitionGroupPath instance) {
      return otherwise(instance);
    }
  }

  public static final class Partition extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public Partition () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Partition)) {
        return false;
      }
      Partition o = (Partition) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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

  public static final class Group extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public Group () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Group)) {
        return false;
      }
      Group o = (Group) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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

  public static final class Path extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public Path () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Path)) {
        return false;
      }
      Path o = (Path) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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

  public static final class PartitionGroup extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public PartitionGroup () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PartitionGroup)) {
        return false;
      }
      PartitionGroup o = (PartitionGroup) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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

  public static final class PartitionPath extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public PartitionPath () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PartitionPath)) {
        return false;
      }
      PartitionPath o = (PartitionPath) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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

  public static final class GroupPath extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public GroupPath () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GroupPath)) {
        return false;
      }
      GroupPath o = (GroupPath) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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

  public static final class PartitionGroupPath extends com.gdblab.pathAlgebra.expressions.OrderByCriterion implements Serializable {
    public PartitionGroupPath () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PartitionGroupPath)) {
        return false;
      }
      PartitionGroupPath o = (PartitionGroupPath) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderByCriterion other) {
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
