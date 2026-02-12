// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_New implements Serializable, Comparable<MethodReference_New> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_New");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_TYPE = new hydra.core.Name("classType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.java.syntax.ClassType classType;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public MethodReference_New (hydra.ext.java.syntax.ClassType classType, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    this.classType = classType;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_New)) {
      return false;
    }
    MethodReference_New o = (MethodReference_New) other;
    return java.util.Objects.equals(
      this.classType,
      o.classType) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(classType) + 3 * java.util.Objects.hashCode(typeArguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodReference_New other) {
    int cmp = 0;
    cmp = ((Comparable) classType).compareTo(other.classType);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
  }
  
  public MethodReference_New withClassType(hydra.ext.java.syntax.ClassType classType) {
    return new MethodReference_New(classType, typeArguments);
  }
  
  public MethodReference_New withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodReference_New(classType, typeArguments);
  }
}
