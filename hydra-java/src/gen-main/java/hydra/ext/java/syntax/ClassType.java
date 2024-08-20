// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ClassType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ClassType");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.ClassTypeQualifier qualifier;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> arguments;
  
  public ClassType (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.ClassTypeQualifier qualifier, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.TypeArgument> arguments) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((qualifier));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((arguments));
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
    ClassType o = (ClassType) (other);
    return annotations.equals(o.annotations) && qualifier.equals(o.qualifier) && identifier.equals(o.identifier) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * qualifier.hashCode() + 5 * identifier.hashCode() + 7 * arguments.hashCode();
  }
  
  public ClassType withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withQualifier(hydra.ext.java.syntax.ClassTypeQualifier qualifier) {
    java.util.Objects.requireNonNull((qualifier));
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
}
