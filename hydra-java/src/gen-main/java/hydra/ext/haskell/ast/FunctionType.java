// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A function type
 */
public class FunctionType implements Serializable, Comparable<FunctionType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.FunctionType");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  public static final hydra.core.Name FIELD_NAME_CODOMAIN = new hydra.core.Name("codomain");
  
  /**
   * The domain type
   */
  public final hydra.ext.haskell.ast.Type domain;
  
  /**
   * The codomain type
   */
  public final hydra.ext.haskell.ast.Type codomain;
  
  public FunctionType (hydra.ext.haskell.ast.Type domain, hydra.ext.haskell.ast.Type codomain) {
    this.domain = domain;
    this.codomain = codomain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionType)) {
      return false;
    }
    FunctionType o = (FunctionType) other;
    return java.util.Objects.equals(
      this.domain,
      o.domain) && java.util.Objects.equals(
      this.codomain,
      o.codomain);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(domain) + 3 * java.util.Objects.hashCode(codomain);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionType other) {
    int cmp = 0;
    cmp = ((Comparable) domain).compareTo(other.domain);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) codomain).compareTo(other.codomain);
  }
  
  public FunctionType withDomain(hydra.ext.haskell.ast.Type domain) {
    return new FunctionType(domain, codomain);
  }
  
  public FunctionType withCodomain(hydra.ext.haskell.ast.Type codomain) {
    return new FunctionType(domain, codomain);
  }
}
