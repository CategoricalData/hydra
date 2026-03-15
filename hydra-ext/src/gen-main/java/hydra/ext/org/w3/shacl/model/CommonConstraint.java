// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * Any of a number of constraint parameters which can be applied either to node or property shapes
 */
public abstract class CommonConstraint implements Serializable, Comparable<CommonConstraint> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.shacl.model.CommonConstraint");
  
  public static final hydra.core.Name AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name CLOSED = new hydra.core.Name("closed");
  
  public static final hydra.core.Name CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name DISJOINT = new hydra.core.Name("disjoint");
  
  public static final hydra.core.Name EQUALS = new hydra.core.Name("equals");
  
  public static final hydra.core.Name HAS_VALUE = new hydra.core.Name("hasValue");
  
  public static final hydra.core.Name IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name LANGUAGE_IN = new hydra.core.Name("languageIn");
  
  public static final hydra.core.Name NODE_KIND = new hydra.core.Name("nodeKind");
  
  public static final hydra.core.Name NODE = new hydra.core.Name("node");
  
  public static final hydra.core.Name NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name MAX_EXCLUSIVE = new hydra.core.Name("maxExclusive");
  
  public static final hydra.core.Name MAX_INCLUSIVE = new hydra.core.Name("maxInclusive");
  
  public static final hydra.core.Name MAX_LENGTH = new hydra.core.Name("maxLength");
  
  public static final hydra.core.Name MIN_EXCLUSIVE = new hydra.core.Name("minExclusive");
  
  public static final hydra.core.Name MIN_INCLUSIVE = new hydra.core.Name("minInclusive");
  
  public static final hydra.core.Name MIN_LENGTH = new hydra.core.Name("minLength");
  
  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name XONE = new hydra.core.Name("xone");
  
  private CommonConstraint () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(And instance) ;
    
    R visit(Closed instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Datatype instance) ;
    
    R visit(Disjoint instance) ;
    
    R visit(Equals instance) ;
    
    R visit(HasValue instance) ;
    
    R visit(In instance) ;
    
    R visit(LanguageIn instance) ;
    
    R visit(NodeKind instance) ;
    
    R visit(Node instance) ;
    
    R visit(Not instance) ;
    
    R visit(MaxExclusive instance) ;
    
    R visit(MaxInclusive instance) ;
    
    R visit(MaxLength instance) ;
    
    R visit(MinExclusive instance) ;
    
    R visit(MinInclusive instance) ;
    
    R visit(MinLength instance) ;
    
    R visit(Pattern instance) ;
    
    R visit(Property instance) ;
    
    R visit(Or instance) ;
    
    R visit(Xone instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CommonConstraint instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(And instance) {
      return otherwise(instance);
    }
    
    default R visit(Closed instance) {
      return otherwise(instance);
    }
    
    default R visit(Class_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Datatype instance) {
      return otherwise(instance);
    }
    
    default R visit(Disjoint instance) {
      return otherwise(instance);
    }
    
    default R visit(Equals instance) {
      return otherwise(instance);
    }
    
    default R visit(HasValue instance) {
      return otherwise(instance);
    }
    
    default R visit(In instance) {
      return otherwise(instance);
    }
    
    default R visit(LanguageIn instance) {
      return otherwise(instance);
    }
    
    default R visit(NodeKind instance) {
      return otherwise(instance);
    }
    
    default R visit(Node instance) {
      return otherwise(instance);
    }
    
    default R visit(Not instance) {
      return otherwise(instance);
    }
    
    default R visit(MaxExclusive instance) {
      return otherwise(instance);
    }
    
    default R visit(MaxInclusive instance) {
      return otherwise(instance);
    }
    
    default R visit(MaxLength instance) {
      return otherwise(instance);
    }
    
    default R visit(MinExclusive instance) {
      return otherwise(instance);
    }
    
    default R visit(MinInclusive instance) {
      return otherwise(instance);
    }
    
    default R visit(MinLength instance) {
      return otherwise(instance);
    }
    
    default R visit(Pattern instance) {
      return otherwise(instance);
    }
    
    default R visit(Property instance) {
      return otherwise(instance);
    }
    
    default R visit(Or instance) {
      return otherwise(instance);
    }
    
    default R visit(Xone instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#AndConstraintComponent
   */
  public static final class And extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value;
    
    public And (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      And o = (And) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#ClosedConstraintComponent
   */
  public static final class Closed extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.shacl.model.Closed value;
    
    public Closed (hydra.ext.org.w3.shacl.model.Closed value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Closed)) {
        return false;
      }
      Closed o = (Closed) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Closed o = (Closed) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#ClassConstraintComponent
   */
  public static final class Class_ extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> value;
    
    public Class_ (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#DatatypeConstraintComponent
   */
  public static final class Datatype extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Iri value;
    
    public Datatype (hydra.ext.org.w3.rdf.syntax.Iri value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datatype)) {
        return false;
      }
      Datatype o = (Datatype) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Datatype o = (Datatype) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#DisjointConstraintComponent
   */
  public static final class Disjoint extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> value;
    
    public Disjoint (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjoint)) {
        return false;
      }
      Disjoint o = (Disjoint) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Disjoint o = (Disjoint) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#EqualsConstraintComponent
   */
  public static final class Equals extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> value;
    
    public Equals (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equals)) {
        return false;
      }
      Equals o = (Equals) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Equals o = (Equals) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Specifies the condition that at least one value node is equal to the given RDF term. See https://www.w3.org/TR/shacl/#HasValueConstraintComponent
   */
  public static final class HasValue extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Node> value;
    
    public HasValue (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Node> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasValue)) {
        return false;
      }
      HasValue o = (HasValue) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasValue o = (HasValue) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Specifies the condition that each value node is a member of a provided SHACL list. See https://www.w3.org/TR/shacl/#InConstraintComponent
   */
  public static final class In extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Node> value;
    
    public In (hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Node> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      In o = (In) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#LanguageInConstraintComponent
   */
  public static final class LanguageIn extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.LanguageTag> value;
    
    public LanguageIn (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.LanguageTag> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LanguageIn)) {
        return false;
      }
      LanguageIn o = (LanguageIn) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LanguageIn o = (LanguageIn) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#NodeKindConstraintComponent
   */
  public static final class NodeKind extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.shacl.model.NodeKind value;
    
    public NodeKind (hydra.ext.org.w3.shacl.model.NodeKind value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NodeKind)) {
        return false;
      }
      NodeKind o = (NodeKind) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NodeKind o = (NodeKind) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#NodeConstraintComponent
   */
  public static final class Node extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.NodeShape>> value;
    
    public Node (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.NodeShape>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Node o = (Node) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#NotConstraintComponent
   */
  public static final class Not extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value;
    
    public Not (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent
   */
  public static final class MaxExclusive extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Literal value;
    
    public MaxExclusive (hydra.ext.org.w3.rdf.syntax.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxExclusive)) {
        return false;
      }
      MaxExclusive o = (MaxExclusive) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MaxExclusive o = (MaxExclusive) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent
   */
  public static final class MaxInclusive extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Literal value;
    
    public MaxInclusive (hydra.ext.org.w3.rdf.syntax.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxInclusive)) {
        return false;
      }
      MaxInclusive o = (MaxInclusive) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MaxInclusive o = (MaxInclusive) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent
   */
  public static final class MaxLength extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final java.math.BigInteger value;
    
    public MaxLength (java.math.BigInteger value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxLength)) {
        return false;
      }
      MaxLength o = (MaxLength) other;
      return this.value.compareTo(o.value) == 0;
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MaxLength o = (MaxLength) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent
   */
  public static final class MinExclusive extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Literal value;
    
    public MinExclusive (hydra.ext.org.w3.rdf.syntax.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinExclusive)) {
        return false;
      }
      MinExclusive o = (MinExclusive) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MinExclusive o = (MinExclusive) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent
   */
  public static final class MinInclusive extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Literal value;
    
    public MinInclusive (hydra.ext.org.w3.rdf.syntax.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinInclusive)) {
        return false;
      }
      MinInclusive o = (MinInclusive) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MinInclusive o = (MinInclusive) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#MinLengthConstraintComponent
   */
  public static final class MinLength extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final java.math.BigInteger value;
    
    public MinLength (java.math.BigInteger value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinLength)) {
        return false;
      }
      MinLength o = (MinLength) other;
      return this.value.compareTo(o.value) == 0;
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MinLength o = (MinLength) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#PatternConstraintComponent
   */
  public static final class Pattern extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.ext.org.w3.shacl.model.Pattern value;
    
    public Pattern (hydra.ext.org.w3.shacl.model.Pattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pattern o = (Pattern) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#PropertyConstraintComponent
   */
  public static final class Property extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>> value;
    
    public Property (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#OrConstraintComponent
   */
  public static final class Or extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value;
    
    public Or (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Or o = (Or) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#XoneConstraintComponent
   */
  public static final class Xone extends hydra.ext.org.w3.shacl.model.CommonConstraint implements Serializable {
    public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value;
    
    public Xone (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xone)) {
        return false;
      }
      Xone o = (Xone) other;
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
    public int compareTo(CommonConstraint other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Xone o = (Xone) other;
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
