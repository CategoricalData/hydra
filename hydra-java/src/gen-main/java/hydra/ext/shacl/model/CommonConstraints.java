package hydra.ext.shacl.model;

/**
 * Any of a number of constraint parameters which can be applied either to node or property shapes
 */
public class CommonConstraints {
  /**
   * See https://www.w3.org/TR/shacl/#AndConstraintComponent
   */
  public final java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> and;
  
  /**
   * See https://www.w3.org/TR/shacl/#ClosedConstraintComponent
   */
  public final java.util.Optional<hydra.ext.shacl.model.Closed> closed;
  
  /**
   * See https://www.w3.org/TR/shacl/#ClassConstraintComponent
   */
  public final java.util.Set<hydra.ext.rdf.syntax.RdfsClass> class_;
  
  /**
   * See https://www.w3.org/TR/shacl/#DatatypeConstraintComponent
   */
  public final java.util.Optional<hydra.ext.rdf.syntax.Iri> datatype;
  
  /**
   * See https://www.w3.org/TR/shacl/#DisjointConstraintComponent
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Property> disjoint;
  
  /**
   * See https://www.w3.org/TR/shacl/#EqualsConstraintComponent
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Property> equals;
  
  /**
   * Specifies the condition that at least one value node is equal to the given RDF term. See https://www.w3.org/TR/shacl/#HasValueConstraintComponent
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Node> hasValue;
  
  /**
   * Specifies the condition that each value node is a member of a provided SHACL list. See https://www.w3.org/TR/shacl/#InConstraintComponent
   */
  public final java.util.Optional<java.util.List<hydra.ext.rdf.syntax.Node>> in;
  
  /**
   * See https://www.w3.org/TR/shacl/#LanguageInConstraintComponent
   */
  public final java.util.Optional<java.util.List<hydra.ext.rdf.syntax.LanguageTag>> languageIn;
  
  /**
   * See https://www.w3.org/TR/shacl/#NodeKindConstraintComponent
   */
  public final java.util.Optional<hydra.ext.shacl.model.NodeKind> nodeKind;
  
  /**
   * See https://www.w3.org/TR/shacl/#NodeConstraintComponent
   */
  public final java.util.Set<hydra.ext.shacl.model.NodeShape> node;
  
  /**
   * See https://www.w3.org/TR/shacl/#NotConstraintComponent
   */
  public final java.util.Set<hydra.ext.shacl.model.Shape> not;
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent
   */
  public final java.util.Optional<hydra.ext.rdf.syntax.Literal> maxExclusive;
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent
   */
  public final java.util.Optional<hydra.ext.rdf.syntax.Literal> maxInclusive;
  
  /**
   * See https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent
   */
  public final java.util.Optional<java.math.BigInteger> maxLength;
  
  /**
   * See https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent
   */
  public final java.util.Optional<hydra.ext.rdf.syntax.Literal> minExclusive;
  
  /**
   * See https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent
   */
  public final java.util.Optional<hydra.ext.rdf.syntax.Literal> minInclusive;
  
  /**
   * See https://www.w3.org/TR/shacl/#MinLengthConstraintComponent
   */
  public final java.util.Optional<java.math.BigInteger> minLength;
  
  /**
   * See https://www.w3.org/TR/shacl/#PatternConstraintComponent
   */
  public final java.util.Optional<hydra.ext.shacl.model.Pattern> pattern;
  
  /**
   * See https://www.w3.org/TR/shacl/#PropertyConstraintComponent
   */
  public final java.util.Set<hydra.ext.shacl.model.PropertyShape> property;
  
  /**
   * See https://www.w3.org/TR/shacl/#OrConstraintComponent
   */
  public final java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> or;
  
  /**
   * See https://www.w3.org/TR/shacl/#XoneConstraintComponent
   */
  public final java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> xone;
  
  public CommonConstraints (java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> and, java.util.Optional<hydra.ext.shacl.model.Closed> closed, java.util.Set<hydra.ext.rdf.syntax.RdfsClass> class_, java.util.Optional<hydra.ext.rdf.syntax.Iri> datatype, java.util.Set<hydra.ext.rdf.syntax.Property> disjoint, java.util.Set<hydra.ext.rdf.syntax.Property> equals, java.util.Set<hydra.ext.rdf.syntax.Node> hasValue, java.util.Optional<java.util.List<hydra.ext.rdf.syntax.Node>> in, java.util.Optional<java.util.List<hydra.ext.rdf.syntax.LanguageTag>> languageIn, java.util.Optional<hydra.ext.shacl.model.NodeKind> nodeKind, java.util.Set<hydra.ext.shacl.model.NodeShape> node, java.util.Set<hydra.ext.shacl.model.Shape> not, java.util.Optional<hydra.ext.rdf.syntax.Literal> maxExclusive, java.util.Optional<hydra.ext.rdf.syntax.Literal> maxInclusive, java.util.Optional<java.math.BigInteger> maxLength, java.util.Optional<hydra.ext.rdf.syntax.Literal> minExclusive, java.util.Optional<hydra.ext.rdf.syntax.Literal> minInclusive, java.util.Optional<java.math.BigInteger> minLength, java.util.Optional<hydra.ext.shacl.model.Pattern> pattern, java.util.Set<hydra.ext.shacl.model.PropertyShape> property, java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> or, java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> xone) {
    this.and = and;
    this.closed = closed;
    this.class_ = class_;
    this.datatype = datatype;
    this.disjoint = disjoint;
    this.equals = equals;
    this.hasValue = hasValue;
    this.in = in;
    this.languageIn = languageIn;
    this.nodeKind = nodeKind;
    this.node = node;
    this.not = not;
    this.maxExclusive = maxExclusive;
    this.maxInclusive = maxInclusive;
    this.maxLength = maxLength;
    this.minExclusive = minExclusive;
    this.minInclusive = minInclusive;
    this.minLength = minLength;
    this.pattern = pattern;
    this.property = property;
    this.or = or;
    this.xone = xone;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CommonConstraints)) {
      return false;
    }
    CommonConstraints o = (CommonConstraints) (other);
    return and.equals(o.and) && closed.equals(o.closed) && class_.equals(o.class_) && datatype.equals(o.datatype) && disjoint.equals(o.disjoint) && equals.equals(o.equals) && hasValue.equals(o.hasValue) && in.equals(o.in) && languageIn.equals(o.languageIn) && nodeKind.equals(o.nodeKind) && node.equals(o.node) && not.equals(o.not) && maxExclusive.equals(o.maxExclusive) && maxInclusive.equals(o.maxInclusive) && maxLength.equals(o.maxLength) && minExclusive.equals(o.minExclusive) && minInclusive.equals(o.minInclusive) && minLength.equals(o.minLength) && pattern.equals(o.pattern) && property.equals(o.property) && or.equals(o.or) && xone.equals(o.xone);
  }
  
  @Override
  public int hashCode() {
    return 2 * and.hashCode() + 3 * closed.hashCode() + 5 * class_.hashCode() + 7 * datatype.hashCode() + 11 * disjoint.hashCode() + 13 * equals.hashCode() + 17 * hasValue.hashCode() + 19 * in.hashCode() + 23 * languageIn.hashCode() + 29 * nodeKind.hashCode() + 31 * node.hashCode() + 37 * not.hashCode() + 41 * maxExclusive.hashCode() + 43 * maxInclusive.hashCode() + 47 * maxLength.hashCode() + 53 * minExclusive.hashCode() + 59 * minInclusive.hashCode() + 61 * minLength.hashCode() + 67 * pattern.hashCode() + 71 * property.hashCode() + 2 * or.hashCode() + 3 * xone.hashCode();
  }
  
  public CommonConstraints withAnd(java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> and) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withClosed(java.util.Optional<hydra.ext.shacl.model.Closed> closed) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withClass(java.util.Set<hydra.ext.rdf.syntax.RdfsClass> class_) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withDatatype(java.util.Optional<hydra.ext.rdf.syntax.Iri> datatype) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withDisjoint(java.util.Set<hydra.ext.rdf.syntax.Property> disjoint) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withEquals(java.util.Set<hydra.ext.rdf.syntax.Property> equals) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withHasValue(java.util.Set<hydra.ext.rdf.syntax.Node> hasValue) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withIn(java.util.Optional<java.util.List<hydra.ext.rdf.syntax.Node>> in) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withLanguageIn(java.util.Optional<java.util.List<hydra.ext.rdf.syntax.LanguageTag>> languageIn) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withNodeKind(java.util.Optional<hydra.ext.shacl.model.NodeKind> nodeKind) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withNode(java.util.Set<hydra.ext.shacl.model.NodeShape> node) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withNot(java.util.Set<hydra.ext.shacl.model.Shape> not) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withMaxExclusive(java.util.Optional<hydra.ext.rdf.syntax.Literal> maxExclusive) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withMaxInclusive(java.util.Optional<hydra.ext.rdf.syntax.Literal> maxInclusive) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withMaxLength(java.util.Optional<java.math.BigInteger> maxLength) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withMinExclusive(java.util.Optional<hydra.ext.rdf.syntax.Literal> minExclusive) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withMinInclusive(java.util.Optional<hydra.ext.rdf.syntax.Literal> minInclusive) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withMinLength(java.util.Optional<java.math.BigInteger> minLength) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withPattern(java.util.Optional<hydra.ext.shacl.model.Pattern> pattern) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withProperty(java.util.Set<hydra.ext.shacl.model.PropertyShape> property) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withOr(java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> or) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
  
  public CommonConstraints withXone(java.util.Optional<java.util.List<hydra.ext.shacl.model.Shape>> xone) {
    return new CommonConstraints(and, closed, class_, datatype, disjoint, equals, hasValue, in, languageIn, nodeKind, node, not, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, property, or, xone);
  }
}