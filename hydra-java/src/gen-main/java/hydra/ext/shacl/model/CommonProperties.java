// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shacl.model;

import java.io.Serializable;

/**
 * Common constraint parameters and other properties for SHACL shapes
 */
public class CommonProperties implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shacl/model.CommonProperties");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_DEACTIVATED = new hydra.core.Name("deactivated");
  
  public static final hydra.core.Name FIELD_NAME_MESSAGE = new hydra.core.Name("message");
  
  public static final hydra.core.Name FIELD_NAME_SEVERITY = new hydra.core.Name("severity");
  
  public static final hydra.core.Name FIELD_NAME_TARGET_CLASS = new hydra.core.Name("targetClass");
  
  public static final hydra.core.Name FIELD_NAME_TARGET_NODE = new hydra.core.Name("targetNode");
  
  public static final hydra.core.Name FIELD_NAME_TARGET_OBJECTS_OF = new hydra.core.Name("targetObjectsOf");
  
  public static final hydra.core.Name FIELD_NAME_TARGET_SUBJECTS_OF = new hydra.core.Name("targetSubjectsOf");
  
  /**
   * Common constraint parameters attached to this shape
   */
  public final java.util.Set<hydra.ext.shacl.model.CommonConstraint> constraints;
  
  /**
   * See https://www.w3.org/TR/shacl/#deactivated
   */
  public final hydra.util.Opt<Boolean> deactivated;
  
  /**
   * See https://www.w3.org/TR/shacl/#message
   */
  public final hydra.ext.rdf.syntax.LangStrings message;
  
  /**
   * See https://www.w3.org/TR/shacl/#severity
   */
  public final hydra.ext.shacl.model.Severity severity;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetClass
   */
  public final java.util.Set<hydra.ext.rdf.syntax.RdfsClass> targetClass;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetNode
   */
  public final java.util.Set<hydra.ext.rdf.syntax.IriOrLiteral> targetNode;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetObjectsOf
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Property> targetObjectsOf;
  
  /**
   * See https://www.w3.org/TR/shacl/#targetSubjectsOf
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Property> targetSubjectsOf;
  
  public CommonProperties (java.util.Set<hydra.ext.shacl.model.CommonConstraint> constraints, hydra.util.Opt<Boolean> deactivated, hydra.ext.rdf.syntax.LangStrings message, hydra.ext.shacl.model.Severity severity, java.util.Set<hydra.ext.rdf.syntax.RdfsClass> targetClass, java.util.Set<hydra.ext.rdf.syntax.IriOrLiteral> targetNode, java.util.Set<hydra.ext.rdf.syntax.Property> targetObjectsOf, java.util.Set<hydra.ext.rdf.syntax.Property> targetSubjectsOf) {
    java.util.Objects.requireNonNull((constraints));
    java.util.Objects.requireNonNull((deactivated));
    java.util.Objects.requireNonNull((message));
    java.util.Objects.requireNonNull((severity));
    java.util.Objects.requireNonNull((targetClass));
    java.util.Objects.requireNonNull((targetNode));
    java.util.Objects.requireNonNull((targetObjectsOf));
    java.util.Objects.requireNonNull((targetSubjectsOf));
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
    CommonProperties o = (CommonProperties) (other);
    return constraints.equals(o.constraints) && deactivated.equals(o.deactivated) && message.equals(o.message) && severity.equals(o.severity) && targetClass.equals(o.targetClass) && targetNode.equals(o.targetNode) && targetObjectsOf.equals(o.targetObjectsOf) && targetSubjectsOf.equals(o.targetSubjectsOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * constraints.hashCode() + 3 * deactivated.hashCode() + 5 * message.hashCode() + 7 * severity.hashCode() + 11 * targetClass.hashCode() + 13 * targetNode.hashCode() + 17 * targetObjectsOf.hashCode() + 19 * targetSubjectsOf.hashCode();
  }
  
  public CommonProperties withConstraints(java.util.Set<hydra.ext.shacl.model.CommonConstraint> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withDeactivated(hydra.util.Opt<Boolean> deactivated) {
    java.util.Objects.requireNonNull((deactivated));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withMessage(hydra.ext.rdf.syntax.LangStrings message) {
    java.util.Objects.requireNonNull((message));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withSeverity(hydra.ext.shacl.model.Severity severity) {
    java.util.Objects.requireNonNull((severity));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetClass(java.util.Set<hydra.ext.rdf.syntax.RdfsClass> targetClass) {
    java.util.Objects.requireNonNull((targetClass));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetNode(java.util.Set<hydra.ext.rdf.syntax.IriOrLiteral> targetNode) {
    java.util.Objects.requireNonNull((targetNode));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetObjectsOf(java.util.Set<hydra.ext.rdf.syntax.Property> targetObjectsOf) {
    java.util.Objects.requireNonNull((targetObjectsOf));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
  
  public CommonProperties withTargetSubjectsOf(java.util.Set<hydra.ext.rdf.syntax.Property> targetSubjectsOf) {
    java.util.Objects.requireNonNull((targetSubjectsOf));
    return new CommonProperties(constraints, deactivated, message, severity, targetClass, targetNode, targetObjectsOf, targetSubjectsOf);
  }
}
