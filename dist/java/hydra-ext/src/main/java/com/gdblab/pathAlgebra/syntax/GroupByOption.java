// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public abstract class GroupByOption implements Serializable, Comparable<GroupByOption> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.GroupByOption");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public static final hydra.core.Name LENGTH = new hydra.core.Name("length");

  public static final hydra.core.Name SOURCE_TARGET = new hydra.core.Name("sourceTarget");

  public static final hydra.core.Name SOURCE_LENGTH = new hydra.core.Name("sourceLength");

  public static final hydra.core.Name TARGET_LENGTH = new hydra.core.Name("targetLength");

  public static final hydra.core.Name SOURCE_TARGET_LENGTH = new hydra.core.Name("sourceTargetLength");

  private GroupByOption () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Source instance) ;

    R visit(Target instance) ;

    R visit(Length instance) ;

    R visit(SourceTarget instance) ;

    R visit(SourceLength instance) ;

    R visit(TargetLength instance) ;

    R visit(SourceTargetLength instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GroupByOption instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Source instance) {
      return otherwise(instance);
    }

    default R visit(Target instance) {
      return otherwise(instance);
    }

    default R visit(Length instance) {
      return otherwise(instance);
    }

    default R visit(SourceTarget instance) {
      return otherwise(instance);
    }

    default R visit(SourceLength instance) {
      return otherwise(instance);
    }

    default R visit(TargetLength instance) {
      return otherwise(instance);
    }

    default R visit(SourceTargetLength instance) {
      return otherwise(instance);
    }
  }

  public static final class Source extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public Source () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Source)) {
        return false;
      }
      Source o = (Source) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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

  public static final class Target extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public Target () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Target)) {
        return false;
      }
      Target o = (Target) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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

  public static final class Length extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public Length () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Length)) {
        return false;
      }
      Length o = (Length) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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

  public static final class SourceTarget extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public SourceTarget () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SourceTarget)) {
        return false;
      }
      SourceTarget o = (SourceTarget) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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

  public static final class SourceLength extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public SourceLength () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SourceLength)) {
        return false;
      }
      SourceLength o = (SourceLength) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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

  public static final class TargetLength extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public TargetLength () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TargetLength)) {
        return false;
      }
      TargetLength o = (TargetLength) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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

  public static final class SourceTargetLength extends com.gdblab.pathAlgebra.syntax.GroupByOption implements Serializable {
    public SourceTargetLength () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SourceTargetLength)) {
        return false;
      }
      SourceTargetLength o = (SourceTargetLength) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupByOption other) {
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
