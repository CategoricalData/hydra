// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RegularInterpolation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RegularInterpolation");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_WIDTH = new hydra.core.Name("width");
  
  public static final hydra.core.Name FIELD_NAME_FORMAT = new hydra.core.Name("format");
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Expression> width;
  
  public final hydra.util.Opt<String> format;
  
  public RegularInterpolation (hydra.ext.csharp.syntax.Expression expression, hydra.util.Opt<hydra.ext.csharp.syntax.Expression> width, hydra.util.Opt<String> format) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((width));
    java.util.Objects.requireNonNull((format));
    this.expression = expression;
    this.width = width;
    this.format = format;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RegularInterpolation)) {
      return false;
    }
    RegularInterpolation o = (RegularInterpolation) (other);
    return expression.equals(o.expression) && width.equals(o.width) && format.equals(o.format);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * width.hashCode() + 5 * format.hashCode();
  }
  
  public RegularInterpolation withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new RegularInterpolation(expression, width, format);
  }
  
  public RegularInterpolation withWidth(hydra.util.Opt<hydra.ext.csharp.syntax.Expression> width) {
    java.util.Objects.requireNonNull((width));
    return new RegularInterpolation(expression, width, format);
  }
  
  public RegularInterpolation withFormat(hydra.util.Opt<String> format) {
    java.util.Objects.requireNonNull((format));
    return new RegularInterpolation(expression, width, format);
  }
}