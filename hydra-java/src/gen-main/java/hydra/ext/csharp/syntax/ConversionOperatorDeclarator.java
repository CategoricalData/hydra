// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ConversionOperatorDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConversionOperatorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_KIND = new hydra.core.Name("kind");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public final hydra.ext.csharp.syntax.ConversionKind kind;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.FixedParameter parameter;
  
  public ConversionOperatorDeclarator (hydra.ext.csharp.syntax.ConversionKind kind, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.FixedParameter parameter) {
    java.util.Objects.requireNonNull((kind));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((parameter));
    this.kind = kind;
    this.type = type;
    this.parameter = parameter;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConversionOperatorDeclarator)) {
      return false;
    }
    ConversionOperatorDeclarator o = (ConversionOperatorDeclarator) (other);
    return kind.equals(o.kind) && type.equals(o.type) && parameter.equals(o.parameter);
  }
  
  @Override
  public int hashCode() {
    return 2 * kind.hashCode() + 3 * type.hashCode() + 5 * parameter.hashCode();
  }
  
  public ConversionOperatorDeclarator withKind(hydra.ext.csharp.syntax.ConversionKind kind) {
    java.util.Objects.requireNonNull((kind));
    return new ConversionOperatorDeclarator(kind, type, parameter);
  }
  
  public ConversionOperatorDeclarator withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ConversionOperatorDeclarator(kind, type, parameter);
  }
  
  public ConversionOperatorDeclarator withParameter(hydra.ext.csharp.syntax.FixedParameter parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new ConversionOperatorDeclarator(kind, type, parameter);
  }
}