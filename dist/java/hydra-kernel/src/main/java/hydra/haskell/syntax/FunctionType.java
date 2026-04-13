// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A function type
 */
public class FunctionType implements Serializable, Comparable<FunctionType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.FunctionType");

  public static final hydra.core.Name DOMAIN = new hydra.core.Name("domain");

  public static final hydra.core.Name CODOMAIN = new hydra.core.Name("codomain");

  /**
   * The domain type
   */
  public final hydra.haskell.syntax.Type domain;

  /**
   * The codomain type
   */
  public final hydra.haskell.syntax.Type codomain;

  public FunctionType (hydra.haskell.syntax.Type domain, hydra.haskell.syntax.Type codomain) {
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
    cmp = hydra.util.Comparing.compare(
      domain,
      other.domain);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      codomain,
      other.codomain);
  }

  public FunctionType withDomain(hydra.haskell.syntax.Type domain) {
    return new FunctionType(domain, codomain);
  }

  public FunctionType withCodomain(hydra.haskell.syntax.Type codomain) {
    return new FunctionType(domain, codomain);
  }
}
