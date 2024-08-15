// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shacl.model;

import java.io.Serializable;

/**
 * Any of a number of constraint parameters which can be applied either to node or property shapes
 */
public abstract class CommonConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shacl/model.CommonConstraint");
  
  public static final hydra.core.Name FIELD_NAME_AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name FIELD_NAME_CLOSED = new hydra.core.Name("closed");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name FIELD_NAME_DISJOINT = new hydra.core.Name("disjoint");
  
  public static final hydra.core.Name FIELD_NAME_EQUALS = new hydra.core.Name("equals");
  
  public static final hydra.core.Name FIELD_NAME_HAS_VALUE = new hydra.core.Name("hasValue");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_LANGUAGE_IN = new hydra.core.Name("languageIn");
  
  public static final hydra.core.Name FIELD_NAME_NODE_KIND = new hydra.core.Name("nodeKind");
  
  public static final hydra.core.Name FIELD_NAME_NODE = new hydra.core.Name("node");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_MAX_EXCLUSIVE = new hydra.core.Name("maxExclusive");
  
  public static final hydra.core.Name FIELD_NAME_MAX_INCLUSIVE = new hydra.core.Name("maxInclusive");
  
  public static final hydra.core.Name FIELD_NAME_MAX_LENGTH = new hydra.core.Name("maxLength");
  
  public static final hydra.core.Name FIELD_NAME_MIN_EXCLUSIVE = new hydra.core.Name("minExclusive");
  
  public static final hydra.core.Name FIELD_NAME_MIN_INCLUSIVE = new hydra.core.Name("minInclusive");
  
  public static final hydra.core.Name FIELD_NAME_MIN_LENGTH = new hydra.core.Name("minLength");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name FIELD_NAME_XONE = new hydra.core.Name("xone");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
    
    default R visit(Closed instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Datatype instance) {
      return otherwise((instance));
    }
    
    default R visit(Disjoint instance) {
      return otherwise((instance));
    }
    
    default R visit(Equals instance) {
      return otherwise((instance));
    }
    
    default R visit(HasValue instance) {
      return otherwise((instance));
    }
    
    default R visit(In instance) {
      return otherwise((instance));
    }
    
    default R visit(LanguageIn instance) {
      return otherwise((instance));
    }
    
    default R visit(NodeKind instance) {
      return otherwise((instance));
    }
    
    default R visit(Node instance) {
      return otherwise((instance));
    }
    
    default R visit(Not instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxExclusive instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxInclusive instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxLength instance) {
      return otherwise((instance));
    }
    
    default R visit(MinExclusive instance) {
      return otherwise((instance));
    }
    
    default R visit(MinInclusive instance) {
      return otherwise((instance));
    }
    
    default R visit(MinLength instance) {
      return otherwise((instance));
    }
    
    default R visit(Pattern instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
    
    default R visit(Xone instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#AndConstraintComponent
   */
  public static final class And extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value;
    
    public And (java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#ClosedConstraintComponent
   */
  public static final class Closed extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.shacl.model.Closed value;
    
    public Closed (hydra.langs.shacl.model.Closed value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Closed)) {
        return false;
      }
      Closed o = (Closed) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#ClassConstraintComponent
   */
  public static final class Class_ extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.rdf.syntax.RdfsClass> value;
    
    public Class_ (java.util.Set<hydra.langs.rdf.syntax.RdfsClass> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#DatatypeConstraintComponent
   */
  public static final class Datatype extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.rdf.syntax.Iri value;
    
    public Datatype (hydra.langs.rdf.syntax.Iri value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datatype)) {
        return false;
      }
      Datatype o = (Datatype) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#DisjointConstraintComponent
   */
  public static final class Disjoint extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.rdf.syntax.Property> value;
    
    public Disjoint (java.util.Set<hydra.langs.rdf.syntax.Property> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjoint)) {
        return false;
      }
      Disjoint o = (Disjoint) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#EqualsConstraintComponent
   */
  public static final class Equals extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.rdf.syntax.Property> value;
    
    public Equals (java.util.Set<hydra.langs.rdf.syntax.Property> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equals)) {
        return false;
      }
      Equals o = (Equals) (other);
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
  
  /**
   * Specifies the condition that at least one value node is equal to the given RDF term. See https://www.w3.org/TR/shacl/#HasValueConstraintComponent
   */
  public static final class HasValue extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.rdf.syntax.Node> value;
    
    public HasValue (java.util.Set<hydra.langs.rdf.syntax.Node> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasValue)) {
        return false;
      }
      HasValue o = (HasValue) (other);
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
  
  /**
   * Specifies the condition that each value node is a member of a provided SHACL list. See https://www.w3.org/TR/shacl/#InConstraintComponent
   */
  public static final class In extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.List<hydra.langs.rdf.syntax.Node> value;
    
    public In (java.util.List<hydra.langs.rdf.syntax.Node> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#LanguageInConstraintComponent
   */
  public static final class LanguageIn extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.rdf.syntax.LanguageTag> value;
    
    public LanguageIn (java.util.Set<hydra.langs.rdf.syntax.LanguageTag> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LanguageIn)) {
        return false;
      }
      LanguageIn o = (LanguageIn) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#NodeKindConstraintComponent
   */
  public static final class NodeKind extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.shacl.model.NodeKind value;
    
    public NodeKind (hydra.langs.shacl.model.NodeKind value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NodeKind)) {
        return false;
      }
      NodeKind o = (NodeKind) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#NodeConstraintComponent
   */
  public static final class Node extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.NodeShape>> value;
    
    public Node (java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.NodeShape>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#NotConstraintComponent
   */
  public static final class Not extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value;
    
    public Not (java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent
   */
  public static final class MaxExclusive extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.rdf.syntax.Literal value;
    
    public MaxExclusive (hydra.langs.rdf.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxExclusive)) {
        return false;
      }
      MaxExclusive o = (MaxExclusive) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent
   */
  public static final class MaxInclusive extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.rdf.syntax.Literal value;
    
    public MaxInclusive (hydra.langs.rdf.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxInclusive)) {
        return false;
      }
      MaxInclusive o = (MaxInclusive) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent
   */
  public static final class MaxLength extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.math.BigInteger value;
    
    public MaxLength (java.math.BigInteger value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxLength)) {
        return false;
      }
      MaxLength o = (MaxLength) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent
   */
  public static final class MinExclusive extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.rdf.syntax.Literal value;
    
    public MinExclusive (hydra.langs.rdf.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinExclusive)) {
        return false;
      }
      MinExclusive o = (MinExclusive) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent
   */
  public static final class MinInclusive extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.rdf.syntax.Literal value;
    
    public MinInclusive (hydra.langs.rdf.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinInclusive)) {
        return false;
      }
      MinInclusive o = (MinInclusive) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#MinLengthConstraintComponent
   */
  public static final class MinLength extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.math.BigInteger value;
    
    public MinLength (java.math.BigInteger value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinLength)) {
        return false;
      }
      MinLength o = (MinLength) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#PatternConstraintComponent
   */
  public static final class Pattern extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final hydra.langs.shacl.model.Pattern value;
    
    public Pattern (hydra.langs.shacl.model.Pattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#PropertyConstraintComponent
   */
  public static final class Property extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.PropertyShape>> value;
    
    public Property (java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.PropertyShape>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#OrConstraintComponent
   */
  public static final class Or extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value;
    
    public Or (java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#XoneConstraintComponent
   */
  public static final class Xone extends hydra.langs.shacl.model.CommonConstraint implements Serializable {
    public final java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value;
    
    public Xone (java.util.Set<hydra.langs.shacl.model.Reference<hydra.langs.shacl.model.Shape>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xone)) {
        return false;
      }
      Xone o = (Xone) (other);
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