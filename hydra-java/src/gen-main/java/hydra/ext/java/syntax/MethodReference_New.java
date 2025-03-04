// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_New implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_New");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_TYPE = new hydra.core.Name("classType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.java.syntax.ClassType classType;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public MethodReference_New (hydra.ext.java.syntax.ClassType classType, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((classType));
    java.util.Objects.requireNonNull((typeArguments));
    this.classType = classType;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_New)) {
      return false;
    }
    MethodReference_New o = (MethodReference_New) (other);
    return classType.equals(o.classType) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * classType.hashCode() + 3 * typeArguments.hashCode();
  }
  
  public MethodReference_New withClassType(hydra.ext.java.syntax.ClassType classType) {
    java.util.Objects.requireNonNull((classType));
    return new MethodReference_New(classType, typeArguments);
  }
  
  public MethodReference_New withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new MethodReference_New(classType, typeArguments);
  }
}