// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class VerbatimInterpolation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.VerbatimInterpolation");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_WIDTH = new hydra.core.Name("width");
  
  public static final hydra.core.Name FIELD_NAME_FORMAT = new hydra.core.Name("format");
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> width;
  
  public final hydra.util.Opt<String> format;
  
  public VerbatimInterpolation (hydra.ext.csharp.syntax.Expression expression, hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> width, hydra.util.Opt<String> format) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((width));
    java.util.Objects.requireNonNull((format));
    this.expression = expression;
    this.width = width;
    this.format = format;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VerbatimInterpolation)) {
      return false;
    }
    VerbatimInterpolation o = (VerbatimInterpolation) (other);
    return expression.equals(o.expression) && width.equals(o.width) && format.equals(o.format);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * width.hashCode() + 5 * format.hashCode();
  }
  
  public VerbatimInterpolation withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new VerbatimInterpolation(expression, width, format);
  }
  
  public VerbatimInterpolation withWidth(hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> width) {
    java.util.Objects.requireNonNull((width));
    return new VerbatimInterpolation(expression, width, format);
  }
  
  public VerbatimInterpolation withFormat(hydra.util.Opt<String> format) {
    java.util.Objects.requireNonNull((format));
    return new VerbatimInterpolation(expression, width, format);
  }
}