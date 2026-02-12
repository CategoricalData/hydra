// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ClassType implements Serializable, Comparable<ClassType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassType");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.ClassTypeQualifier qualifier;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> arguments;
  
  public ClassType (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.ClassTypeQualifier qualifier, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.TypeArgument> arguments) {
    this.annotations = annotations;
    this.qualifier = qualifier;
    this.identifier = identifier;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassType)) {
      return false;
    }
    ClassType o = (ClassType) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.qualifier,
      o.qualifier) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(qualifier) + 5 * java.util.Objects.hashCode(identifier) + 7 * java.util.Objects.hashCode(arguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassType other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) qualifier).compareTo(other.qualifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      arguments.hashCode(),
      other.arguments.hashCode());
  }
  
  public ClassType withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withQualifier(hydra.ext.java.syntax.ClassTypeQualifier qualifier) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> arguments) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
}
