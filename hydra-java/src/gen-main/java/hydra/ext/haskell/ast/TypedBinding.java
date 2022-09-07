package hydra.ext.haskell.ast;

public class TypedBinding {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.TypedBinding");
  
  public final hydra.ext.haskell.ast.TypeSignature typeSignature;
  
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
    TypedBinding o = (TypedBinding) (other);
    return typeSignature.equals(o.typeSignature) && valueBinding.equals(o.valueBinding);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeSignature.hashCode() + 3 * valueBinding.hashCode();
  }
  
  public TypedBinding withTypeSignature(hydra.ext.haskell.ast.TypeSignature typeSignature) {
    return new TypedBinding(typeSignature, valueBinding);
  }
  
  public TypedBinding withValueBinding(hydra.ext.haskell.ast.ValueBinding valueBinding) {
    return new TypedBinding(typeSignature, valueBinding);
  }
}