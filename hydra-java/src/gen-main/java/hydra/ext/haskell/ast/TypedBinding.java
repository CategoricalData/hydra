package hydra.ext.haskell.ast;

public class TypedBinding {
  public final TypeSignature typeSignature;
  
  public final ValueBinding valueBinding;
  
  public TypedBinding (TypeSignature typeSignature, ValueBinding valueBinding) {
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
  
  public TypedBinding withTypeSignature(TypeSignature typeSignature) {
    return new TypedBinding(typeSignature, valueBinding);
  }
  
  public TypedBinding withValueBinding(ValueBinding valueBinding) {
    return new TypedBinding(typeSignature, valueBinding);
  }
}