// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * A union type variant name which, when capitalized and concatenated with its type name, conflicts with another type definition name. For example, a union type Foo with a variant bar produces FooBar, which conflicts with an existing type definition FooBar. This is currently a problem only for the Haskell target.
 */
public class ConflictingVariantNameError implements Serializable, Comparable<ConflictingVariantNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name VARIANT_NAME = new hydra.core.Name("variantName");

  public static final hydra.core.Name CONFLICTING_NAME = new hydra.core.Name("conflictingName");

  /**
   * The namespace of the module containing the conflict
   */
  public final hydra.module.Namespace namespace;

  /**
   * The name of the union type
   */
  public final hydra.core.Name typeName;

  /**
   * The name of the variant field causing the conflict
   */
  public final hydra.core.Name variantName;

  /**
   * The name of the other type definition that conflicts with the generated constructor name
   */
  public final hydra.core.Name conflictingName;

  public ConflictingVariantNameError (hydra.module.Namespace namespace, hydra.core.Name typeName, hydra.core.Name variantName, hydra.core.Name conflictingName) {
    this.namespace = namespace;
    this.typeName = typeName;
    this.variantName = variantName;
    this.conflictingName = conflictingName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConflictingVariantNameError)) {
      return false;
    }
    ConflictingVariantNameError o = (ConflictingVariantNameError) other;
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.variantName,
      o.variantName) && java.util.Objects.equals(
      this.conflictingName,
      o.conflictingName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(typeName) + 5 * java.util.Objects.hashCode(variantName) + 7 * java.util.Objects.hashCode(conflictingName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConflictingVariantNameError other) {
    int cmp = 0;
    cmp = ((Comparable) namespace).compareTo(other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) typeName).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) variantName).compareTo(other.variantName);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) conflictingName).compareTo(other.conflictingName);
  }

  public ConflictingVariantNameError withNamespace(hydra.module.Namespace namespace) {
    return new ConflictingVariantNameError(namespace, typeName, variantName, conflictingName);
  }

  public ConflictingVariantNameError withTypeName(hydra.core.Name typeName) {
    return new ConflictingVariantNameError(namespace, typeName, variantName, conflictingName);
  }

  public ConflictingVariantNameError withVariantName(hydra.core.Name variantName) {
    return new ConflictingVariantNameError(namespace, typeName, variantName, conflictingName);
  }

  public ConflictingVariantNameError withConflictingName(hydra.core.Name conflictingName) {
    return new ConflictingVariantNameError(namespace, typeName, variantName, conflictingName);
  }
}
