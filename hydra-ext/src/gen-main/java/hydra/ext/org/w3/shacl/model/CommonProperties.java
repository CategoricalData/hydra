// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * Common constraint parameters and other properties for SHACL shapes
 */
public class CommonProperties implements Serializable, Comparable<CommonProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.shacl.model.CommonProperties");
  
  public static final hydra.core.Name CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name DEACTIVATED = new hydra.core.Name("deactivated");
  
  public static final hydra.core.Name MESSAGE = new hydra.core.Name("message");
  
  public static final hydra.core.Name SEVERITY = new hydra.core.Name("severity");
  
  public static final hydra.core.Name TARGET_CLASS = new hydra.core.Name("targetClass");
  
  public static final hydra.core.Name TARGET_NODE = new hydra.core.Name("targetNode");
  
  public static final hydra.core.Name TARGET_OBJECTS_OF = new hydra.core.Name("targetObjectsOf");
  
  public static final hydra.core.Name TARGET_SUBJECTS_OF = new hydra.core.Name("targetSubjectsOf");
  
  /**
   * Common constraint parameters attached to this shape
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.CommonConstraint> constraints;
  
  /**
   * See https://www.w3.org/TR/shacl/#deactivated
   */
  public final hydra.util.Maybe<Boolean> deactivated;
  
  /**
   * See https://www.w3.org/TR/shacl/#message
   */
  public final hydra.ext.org.w3.rdf.syntax.LangStrings message;
  
  /**
   * See https://www.w3.org/TR/shacl/#severity
   */
  public final hydra.ext.org.w3.shacl.model.Severity severity;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetClass
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> targetClass;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetNode
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.IriOrLiteral> targetNode;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetObjectsOf
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> targetObjectsOf;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetSubjectsOf
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> targetSubjectsOf;
  
  public CommonProperties (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.CommonConstraint> constraints, hydra.util.Maybe<Boolean> deactivated, hydra.ext.org.w3.rdf.syntax.LangStrings message, hydra.ext.org.w3.shacl.model.Severity severity, hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> targetClass, hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.IriOrLiteral> targetNode, hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> targetObjectsOf, hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> targetSubjectsOf) {
    this.constraints = constraints;
    this.deactivated = deactivated;
    this.message = message;
    this.severity = severity;
    this.targetClass = targetClass;
    this.targetNode = targetNode;
    this.targetObjectsOf = targetObjectsOf;
    this.targetSubjectsOf = targetSubjectsOf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CommonProperties)) {
      return false;
    }
    CommonProperties o = (CommonProperties) other;
    return java.util.Objects.equals(
      this.constraints,
      o.constraints) && java.util.Objects.equals(
      this.deactivated,
      o.deactivated) && java.util.Objects.equals(
      this.message,
      o.message) && java.util.Objects.equals(
      this.severity,
      o.severity) && java.util.Objects.equals(
      this.targetClass,
      o.targetClass) && java.util.Objects.equals(
      this.targetNode,
      o.targetNode) && java.util.Objects.equals(
      this.targetObjectsOf,
      o.targetObjectsOf) && java.util.Objects.equals(
      this.targetSubjectsOf,
      o.targetSubjectsOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(constraints) + 3 * java.util.Objects.hashCode(deactivated) + 5 * java.util.Objects.hashCode(message) + 7 * java.util.Objects.hashCode(severity) + 11 * java.util.Objects.hashCode(targetClass) + 13 * java.util.Objects.hashCode(targetNode) + 17 * java.util.Objects.hashCode(targetObjectsOf) + 19 * java.util.Objects.hashCode(targetSubjectsOf);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CommonProperties other) {
    int cmp = 0;
    cmp = ((Comparable) constraints).compareTo(other.constraints);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) deactivated).compareTo(other.deactivated);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) message).compareTo(other.message);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) severity).compareTo(other.severity);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) targetClass).compareTo(other.targetClass);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) targetNode).compareTo(other.targetNode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) targetObjectsOf).compareTo(other.targetObjectsOf);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) targetSubjectsOf).compareTo(other.targetSubjectsOf);
  }
  
  public CommonProperties withConstraints(hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.CommonConstraint> constraints) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withDeactivated(hydra.util.Maybe<Boolean> deactivated) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withMessage(hydra.ext.org.w3.rdf.syntax.LangStrings message) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withSeverity(hydra.ext.org.w3.shacl.model.Severity severity) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetClass(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> targetClass) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetNode(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.IriOrLiteral> targetNode) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetObjectsOf(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> targetObjectsOf) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetSubjectsOf(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> targetSubjectsOf) {
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
}
