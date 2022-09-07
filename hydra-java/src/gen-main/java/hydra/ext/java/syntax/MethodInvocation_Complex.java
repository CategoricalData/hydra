package hydra.ext.java.syntax;

public class MethodInvocation_Complex {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.MethodInvocation.Complex");
  
  public final hydra.ext.java.syntax.MethodInvocation_Variant variant;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodInvocation_Complex (hydra.ext.java.syntax.MethodInvocation_Variant variant, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    this.variant = variant;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodInvocation_Complex)) {
      return false;
    }
    MethodInvocation_Complex o = (MethodInvocation_Complex) (other);
    return variant.equals(o.variant) && typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * variant.hashCode() + 3 * typeArguments.hashCode() + 5 * identifier.hashCode();
  }
  
  public MethodInvocation_Complex withVariant(hydra.ext.java.syntax.MethodInvocation_Variant variant) {
    return new MethodInvocation_Complex(variant, typeArguments, identifier);
  }
  
  public MethodInvocation_Complex withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodInvocation_Complex(variant, typeArguments, identifier);
  }
  
  public MethodInvocation_Complex withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodInvocation_Complex(variant, typeArguments, identifier);
  }
}