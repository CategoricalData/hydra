// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class VariantTypeParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.VariantTypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_VARIANCE = new hydra.core.Name("variance");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.VarianceAnnotation> variance;
  
  public final hydra.ext.csharp.syntax.TypeParameter parameter;
  
  public VariantTypeParameter (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.util.Opt<hydra.ext.csharp.syntax.VarianceAnnotation> variance, hydra.ext.csharp.syntax.TypeParameter parameter) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((variance));
    java.util.Objects.requireNonNull((parameter));
    this.attributes = attributes;
    this.variance = variance;
    this.parameter = parameter;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariantTypeParameter)) {
      return false;
    }
    VariantTypeParameter o = (VariantTypeParameter) (other);
    return attributes.equals(o.attributes) && variance.equals(o.variance) && parameter.equals(o.parameter);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * variance.hashCode() + 5 * parameter.hashCode();
  }
  
  public VariantTypeParameter withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new VariantTypeParameter(attributes, variance, parameter);
  }
  
  public VariantTypeParameter withVariance(hydra.util.Opt<hydra.ext.csharp.syntax.VarianceAnnotation> variance) {
    java.util.Objects.requireNonNull((variance));
    return new VariantTypeParameter(attributes, variance, parameter);
  }
  
  public VariantTypeParameter withParameter(hydra.ext.csharp.syntax.TypeParameter parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new VariantTypeParameter(attributes, variance, parameter);
  }
}