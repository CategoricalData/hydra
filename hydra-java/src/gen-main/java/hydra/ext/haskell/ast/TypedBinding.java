// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A binding with its type signature
 */
public class TypedBinding implements Serializable, Comparable<TypedBinding> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypedBinding");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_SIGNATURE = new hydra.core.Name("typeSignature");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_BINDING = new hydra.core.Name("valueBinding");
  
  /**
   * The type signature
   */
  public final hydra.ext.haskell.ast.TypeSignature typeSignature;
  
  /**
   * The value binding
   */
  public final hydra.ext.haskell.ast.ValueBinding valueBinding;
  
  public TypedBinding (hydra.ext.haskell.ast.TypeSignature typeSignature, hydra.ext.haskell.ast.ValueBinding valueBinding) {
    this.typeSignature = typeSignature;
    this.valueBinding = valueBinding;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedBinding)) {
      return false;
    }
    TypedBinding o = (TypedBinding) other;
    return java.util.Objects.equals(
      this.typeSignature,
      o.typeSignature) && java.util.Objects.equals(
      this.valueBinding,
      o.valueBinding);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeSignature) + 3 * java.util.Objects.hashCode(valueBinding);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypedBinding other) {
    int cmp = 0;
    cmp = ((Comparable) typeSignature).compareTo(other.typeSignature);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) valueBinding).compareTo(other.valueBinding);
  }
  
  public TypedBinding withTypeSignature(hydra.ext.haskell.ast.TypeSignature typeSignature) {
    return new TypedBinding(typeSignature, valueBinding);
  }
  
  public TypedBinding withValueBinding(hydra.ext.haskell.ast.ValueBinding valueBinding) {
    return new TypedBinding(typeSignature, valueBinding);
  }
}
