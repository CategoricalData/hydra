// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A structured representation of a function term's components, replacing ad-hoc tuples. This captures all the information extracted from peeling lambdas, type lambdas, lets, and type applications from a term.
 */
public class FunctionStructure<Env> implements Serializable, Comparable<FunctionStructure<Env>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.typing.FunctionStructure");
  
  public static final hydra.core.Name TYPE_PARAMS = new hydra.core.Name("typeParams");
  
  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name DOMAINS = new hydra.core.Name("domains");
  
  public static final hydra.core.Name CODOMAIN = new hydra.core.Name("codomain");
  
  public static final hydra.core.Name ENVIRONMENT = new hydra.core.Name("environment");
  
  /**
   * Type parameters (from type lambdas)
   */
  public final hydra.util.ConsList<hydra.core.Name> typeParams;
  
  /**
   * Value parameters (from lambdas)
   */
  public final hydra.util.ConsList<hydra.core.Name> params;
  
  /**
   * Let bindings accumulated from the term
   */
  public final hydra.util.ConsList<hydra.core.Binding> bindings;
  
  /**
   * The body term after removing all lambdas, lets, etc.
   */
  public final hydra.core.Term body;
  
  /**
   * Domain types of the value parameters
   */
  public final hydra.util.ConsList<hydra.core.Type> domains;
  
  /**
   * The return type of the function (if type inference succeeded)
   */
  public final hydra.util.Maybe<hydra.core.Type> codomain;
  
  /**
   * Updated environment after processing all bindings
   */
  public final Env environment;
  
  public FunctionStructure (hydra.util.ConsList<hydra.core.Name> typeParams, hydra.util.ConsList<hydra.core.Name> params, hydra.util.ConsList<hydra.core.Binding> bindings, hydra.core.Term body, hydra.util.ConsList<hydra.core.Type> domains, hydra.util.Maybe<hydra.core.Type> codomain, Env environment) {
    this.typeParams = typeParams;
    this.params = params;
    this.bindings = bindings;
    this.body = body;
    this.domains = domains;
    this.codomain = codomain;
    this.environment = environment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionStructure)) {
      return false;
    }
    FunctionStructure o = (FunctionStructure) other;
    return java.util.Objects.equals(
      this.typeParams,
      o.typeParams) && java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.domains,
      o.domains) && java.util.Objects.equals(
      this.codomain,
      o.codomain) && java.util.Objects.equals(
      this.environment,
      o.environment);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeParams) + 3 * java.util.Objects.hashCode(params) + 5 * java.util.Objects.hashCode(bindings) + 7 * java.util.Objects.hashCode(body) + 11 * java.util.Objects.hashCode(domains) + 13 * java.util.Objects.hashCode(codomain) + 17 * java.util.Objects.hashCode(environment);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionStructure other) {
    int cmp = 0;
    cmp = Integer.compare(
      typeParams.hashCode(),
      other.typeParams.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      params.hashCode(),
      other.params.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      bindings.hashCode(),
      other.bindings.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      domains.hashCode(),
      other.domains.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      codomain.hashCode(),
      other.codomain.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) environment).compareTo(other.environment);
  }
  
  public FunctionStructure withTypeParams(hydra.util.ConsList<hydra.core.Name> typeParams) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
  
  public FunctionStructure withParams(hydra.util.ConsList<hydra.core.Name> params) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
  
  public FunctionStructure withBindings(hydra.util.ConsList<hydra.core.Binding> bindings) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
  
  public FunctionStructure withBody(hydra.core.Term body) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
  
  public FunctionStructure withDomains(hydra.util.ConsList<hydra.core.Type> domains) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
  
  public FunctionStructure withCodomain(hydra.util.Maybe<hydra.core.Type> codomain) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
  
  public FunctionStructure withEnvironment(Env environment) {
    return new FunctionStructure(typeParams, params, bindings, body, domains, codomain, environment);
  }
}
