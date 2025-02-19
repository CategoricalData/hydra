// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class FunctionType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.FunctionType");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  public static final hydra.core.Name FIELD_NAME_CODOMAIN = new hydra.core.Name("codomain");
  
  public final hydra.ext.haskell.ast.Type domain;
  
  public final hydra.ext.haskell.ast.Type codomain;
  
  public FunctionType (hydra.ext.haskell.ast.Type domain, hydra.ext.haskell.ast.Type codomain) {
    java.util.Objects.requireNonNull((domain));
    java.util.Objects.requireNonNull((codomain));
    this.domain = domain;
    this.codomain = codomain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionType)) {
      return false;
    }
    FunctionType o = (FunctionType) (other);
    return domain.equals(o.domain) && codomain.equals(o.codomain);
  }
  
  @Override
  public int hashCode() {
    return 2 * domain.hashCode() + 3 * codomain.hashCode();
  }
  
  public FunctionType withDomain(hydra.ext.haskell.ast.Type domain) {
    java.util.Objects.requireNonNull((domain));
    return new FunctionType(domain, codomain);
  }
  
  public FunctionType withCodomain(hydra.ext.haskell.ast.Type codomain) {
    java.util.Objects.requireNonNull((codomain));
    return new FunctionType(domain, codomain);
  }
}