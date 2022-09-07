package hydra.ext.java.syntax;

public class ClassType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ClassType");
  
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
    ClassType o = (ClassType) (other);
    return annotations.equals(o.annotations) && qualifier.equals(o.qualifier) && identifier.equals(o.identifier) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * qualifier.hashCode() + 5 * identifier.hashCode() + 7 * arguments.hashCode();
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