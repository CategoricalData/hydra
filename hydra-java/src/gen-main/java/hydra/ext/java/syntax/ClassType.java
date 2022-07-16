package hydra.ext.java.syntax;

public class ClassType {
  public final java.util.List<Annotation> annotations;
  
  public final ClassTypeQualifier qualifier;
  
  public final TypeIdentifier identifier;
  
  public final java.util.List<TypeArgument> arguments;
  
  public ClassType (java.util.List<Annotation> annotations, ClassTypeQualifier qualifier, TypeIdentifier identifier, java.util.List<TypeArgument> arguments) {
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
  
  public ClassType withAnnotations(java.util.List<Annotation> annotations) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withQualifier(ClassTypeQualifier qualifier) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withIdentifier(TypeIdentifier identifier) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
  
  public ClassType withArguments(java.util.List<TypeArgument> arguments) {
    return new ClassType(annotations, qualifier, identifier, arguments);
  }
}