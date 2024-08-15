// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

public class TypedBinding implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.TypedBinding");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_SIGNATURE = new hydra.core.Name("typeSignature");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_BINDING = new hydra.core.Name("valueBinding");
  
  public final hydra.langs.haskell.ast.TypeSignature typeSignature;
  
  public final hydra.langs.haskell.ast.ValueBinding valueBinding;
  
  public TypedBinding (hydra.langs.haskell.ast.TypeSignature typeSignature, hydra.langs.haskell.ast.ValueBinding valueBinding) {
    java.util.Objects.requireNonNull((typeSignature));
    java.util.Objects.requireNonNull((valueBinding));
    this.typeSignature = typeSignature;
    this.valueBinding = valueBinding;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedBinding)) {
      return false;
    }
    TypedBinding o = (TypedBinding) (other);
    return typeSignature.equals(o.typeSignature) && valueBinding.equals(o.valueBinding);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeSignature.hashCode() + 3 * valueBinding.hashCode();
  }
  
  public TypedBinding withTypeSignature(hydra.langs.haskell.ast.TypeSignature typeSignature) {
    java.util.Objects.requireNonNull((typeSignature));
    return new TypedBinding(typeSignature, valueBinding);
  }
  
  public TypedBinding withValueBinding(hydra.langs.haskell.ast.ValueBinding valueBinding) {
    java.util.Objects.requireNonNull((valueBinding));
    return new TypedBinding(typeSignature, valueBinding);
  }
}