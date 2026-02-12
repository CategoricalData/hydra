// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.helpers;

import java.io.Serializable;

/**
 * Aliases and context for Java code generation
 */
public class Aliases implements Serializable, Comparable<Aliases> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.helpers.Aliases");
  
  public static final hydra.core.Name FIELD_NAME_CURRENT_NAMESPACE = new hydra.core.Name("currentNamespace");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGES = new hydra.core.Name("packages");
  
  public static final hydra.core.Name FIELD_NAME_BRANCH_VARS = new hydra.core.Name("branchVars");
  
  public static final hydra.core.Name FIELD_NAME_RECURSIVE_VARS = new hydra.core.Name("recursiveVars");
  
  public static final hydra.core.Name FIELD_NAME_IN_SCOPE_TYPE_PARAMS = new hydra.core.Name("inScopeTypeParams");
  
  public static final hydra.core.Name FIELD_NAME_POLYMORPHIC_LOCALS = new hydra.core.Name("polymorphicLocals");
  
  public static final hydra.core.Name FIELD_NAME_IN_SCOPE_JAVA_VARS = new hydra.core.Name("inScopeJavaVars");
  
  public static final hydra.core.Name FIELD_NAME_VAR_RENAMES = new hydra.core.Name("varRenames");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA_VARS = new hydra.core.Name("lambdaVars");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_VAR_SUBST = new hydra.core.Name("typeVarSubst");
  
  public static final hydra.core.Name FIELD_NAME_TRUSTED_TYPE_VARS = new hydra.core.Name("trustedTypeVars");
  
  public static final hydra.core.Name FIELD_NAME_METHOD_CODOMAIN = new hydra.core.Name("methodCodomain");
  
  public static final hydra.core.Name FIELD_NAME_THUNKED_VARS = new hydra.core.Name("thunkedVars");
  
  /**
   * Current module namespace context
   */
  public final hydra.module.Namespace currentNamespace;
  
  /**
   * Maps namespaces to Java package names
   */
  public final java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName> packages;
  
  /**
   * Variables bound in pattern matching branches
   */
  public final java.util.Set<hydra.core.Name> branchVars;
  
  /**
   * Variables that are self-recursive
   */
  public final java.util.Set<hydra.core.Name> recursiveVars;
  
  /**
   * Type parameters that are in scope (from method-level type parameters)
   */
  public final java.util.Set<hydra.core.Name> inScopeTypeParams;
  
  /**
   * Local variables that have polymorphic types (declared with raw types)
   */
  public final java.util.Set<hydra.core.Name> polymorphicLocals;
  
  /**
   * All in-scope Java variable names (for avoiding lambda parameter shadowing)
   */
  public final java.util.Set<hydra.core.Name> inScopeJavaVars;
  
  /**
   * Variable renames for avoiding shadowing (maps Hydra name to Java name)
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Name> varRenames;
  
  /**
   * Lambda-bound variables (including hoisted captures with qualified names)
   */
  public final java.util.Set<hydra.core.Name> lambdaVars;
  
  /**
   * Type variable substitution: maps fresh inference variable names to canonical scheme variable names
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Name> typeVarSubst;
  
  /**
   * Type variables that actually appear in the method's formal parameter types
   */
  public final java.util.Set<hydra.core.Name> trustedTypeVars;
  
  /**
   * The enclosing method's codomain (return type), used for casting pair expressions
   */
  public final hydra.util.Maybe<hydra.core.Type> methodCodomain;
  
  /**
   * Variables that have been thunked (wrapped in Supplier) for lazy evaluation
   */
  public final java.util.Set<hydra.core.Name> thunkedVars;
  
  public Aliases (hydra.module.Namespace currentNamespace, java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName> packages, java.util.Set<hydra.core.Name> branchVars, java.util.Set<hydra.core.Name> recursiveVars, java.util.Set<hydra.core.Name> inScopeTypeParams, java.util.Set<hydra.core.Name> polymorphicLocals, java.util.Set<hydra.core.Name> inScopeJavaVars, java.util.Map<hydra.core.Name, hydra.core.Name> varRenames, java.util.Set<hydra.core.Name> lambdaVars, java.util.Map<hydra.core.Name, hydra.core.Name> typeVarSubst, java.util.Set<hydra.core.Name> trustedTypeVars, hydra.util.Maybe<hydra.core.Type> methodCodomain, java.util.Set<hydra.core.Name> thunkedVars) {
    this.currentNamespace = currentNamespace;
    this.packages = packages;
    this.branchVars = branchVars;
    this.recursiveVars = recursiveVars;
    this.inScopeTypeParams = inScopeTypeParams;
    this.polymorphicLocals = polymorphicLocals;
    this.inScopeJavaVars = inScopeJavaVars;
    this.varRenames = varRenames;
    this.lambdaVars = lambdaVars;
    this.typeVarSubst = typeVarSubst;
    this.trustedTypeVars = trustedTypeVars;
    this.methodCodomain = methodCodomain;
    this.thunkedVars = thunkedVars;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Aliases)) {
      return false;
    }
    Aliases o = (Aliases) other;
    return java.util.Objects.equals(
      this.currentNamespace,
      o.currentNamespace) && java.util.Objects.equals(
      this.packages,
      o.packages) && java.util.Objects.equals(
      this.branchVars,
      o.branchVars) && java.util.Objects.equals(
      this.recursiveVars,
      o.recursiveVars) && java.util.Objects.equals(
      this.inScopeTypeParams,
      o.inScopeTypeParams) && java.util.Objects.equals(
      this.polymorphicLocals,
      o.polymorphicLocals) && java.util.Objects.equals(
      this.inScopeJavaVars,
      o.inScopeJavaVars) && java.util.Objects.equals(
      this.varRenames,
      o.varRenames) && java.util.Objects.equals(
      this.lambdaVars,
      o.lambdaVars) && java.util.Objects.equals(
      this.typeVarSubst,
      o.typeVarSubst) && java.util.Objects.equals(
      this.trustedTypeVars,
      o.trustedTypeVars) && java.util.Objects.equals(
      this.methodCodomain,
      o.methodCodomain) && java.util.Objects.equals(
      this.thunkedVars,
      o.thunkedVars);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(currentNamespace) + 3 * java.util.Objects.hashCode(packages) + 5 * java.util.Objects.hashCode(branchVars) + 7 * java.util.Objects.hashCode(recursiveVars) + 11 * java.util.Objects.hashCode(inScopeTypeParams) + 13 * java.util.Objects.hashCode(polymorphicLocals) + 17 * java.util.Objects.hashCode(inScopeJavaVars) + 19 * java.util.Objects.hashCode(varRenames) + 23 * java.util.Objects.hashCode(lambdaVars) + 29 * java.util.Objects.hashCode(typeVarSubst) + 31 * java.util.Objects.hashCode(trustedTypeVars) + 37 * java.util.Objects.hashCode(methodCodomain) + 41 * java.util.Objects.hashCode(thunkedVars);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Aliases other) {
    int cmp = 0;
    cmp = ((Comparable) currentNamespace).compareTo(other.currentNamespace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      packages.hashCode(),
      other.packages.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      branchVars.hashCode(),
      other.branchVars.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      recursiveVars.hashCode(),
      other.recursiveVars.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      inScopeTypeParams.hashCode(),
      other.inScopeTypeParams.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      polymorphicLocals.hashCode(),
      other.polymorphicLocals.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      inScopeJavaVars.hashCode(),
      other.inScopeJavaVars.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      varRenames.hashCode(),
      other.varRenames.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      lambdaVars.hashCode(),
      other.lambdaVars.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeVarSubst.hashCode(),
      other.typeVarSubst.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      trustedTypeVars.hashCode(),
      other.trustedTypeVars.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      methodCodomain.hashCode(),
      other.methodCodomain.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      thunkedVars.hashCode(),
      other.thunkedVars.hashCode());
  }
  
  public Aliases withCurrentNamespace(hydra.module.Namespace currentNamespace) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withPackages(java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName> packages) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withBranchVars(java.util.Set<hydra.core.Name> branchVars) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withRecursiveVars(java.util.Set<hydra.core.Name> recursiveVars) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withInScopeTypeParams(java.util.Set<hydra.core.Name> inScopeTypeParams) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withPolymorphicLocals(java.util.Set<hydra.core.Name> polymorphicLocals) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withInScopeJavaVars(java.util.Set<hydra.core.Name> inScopeJavaVars) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withVarRenames(java.util.Map<hydra.core.Name, hydra.core.Name> varRenames) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withLambdaVars(java.util.Set<hydra.core.Name> lambdaVars) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withTypeVarSubst(java.util.Map<hydra.core.Name, hydra.core.Name> typeVarSubst) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withTrustedTypeVars(java.util.Set<hydra.core.Name> trustedTypeVars) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withMethodCodomain(hydra.util.Maybe<hydra.core.Type> methodCodomain) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
  
  public Aliases withThunkedVars(java.util.Set<hydra.core.Name> thunkedVars) {
    return new Aliases(currentNamespace, packages, branchVars, recursiveVars, inScopeTypeParams, polymorphicLocals, inScopeJavaVars, varRenames, lambdaVars, typeVarSubst, trustedTypeVars, methodCodomain, thunkedVars);
  }
}
