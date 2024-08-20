// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodHeader implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.MethodHeader");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_RESULT = new hydra.core.Name("result");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATOR = new hydra.core.Name("declarator");
  
  public static final hydra.core.Name FIELD_NAME_THROWS = new hydra.core.Name("throws");
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.ext.java.syntax.Result result;
  
  public final hydra.ext.java.syntax.MethodDeclarator declarator;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Throws> throws_;
  
  public MethodHeader (java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, hydra.ext.java.syntax.Result result, hydra.ext.java.syntax.MethodDeclarator declarator, hydra.util.Opt<hydra.ext.java.syntax.Throws> throws_) {
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((result));
    java.util.Objects.requireNonNull((declarator));
    java.util.Objects.requireNonNull((throws_));
    this.parameters = parameters;
    this.result = result;
    this.declarator = declarator;
    this.throws_ = throws_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodHeader)) {
      return false;
    }
    MethodHeader o = (MethodHeader) (other);
    return parameters.equals(o.parameters) && result.equals(o.result) && declarator.equals(o.declarator) && throws_.equals(o.throws_);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode() + 3 * result.hashCode() + 5 * declarator.hashCode() + 7 * throws_.hashCode();
  }
  
  public MethodHeader withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withResult(hydra.ext.java.syntax.Result result) {
    java.util.Objects.requireNonNull((result));
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withDeclarator(hydra.ext.java.syntax.MethodDeclarator declarator) {
    java.util.Objects.requireNonNull((declarator));
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withThrows(hydra.util.Opt<hydra.ext.java.syntax.Throws> throws_) {
    java.util.Objects.requireNonNull((throws_));
    return new MethodHeader(parameters, result, declarator, throws_);
  }
}
