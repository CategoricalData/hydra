// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * An ordinary (positional) data constructor
 */
public class OrdinaryConstructor implements Serializable, Comparable<OrdinaryConstructor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  /**
   * The name of the constructor
   */
  public final hydra.haskell.syntax.Name name;

  /**
   * The types of the positional fields
   */
  public final java.util.List<hydra.haskell.syntax.Type> fields;

  public OrdinaryConstructor (hydra.haskell.syntax.Name name, java.util.List<hydra.haskell.syntax.Type> fields) {
    this.name = name;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrdinaryConstructor)) {
      return false;
    }
    OrdinaryConstructor o = (OrdinaryConstructor) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(fields);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrdinaryConstructor other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      fields,
      other.fields);
  }

  public OrdinaryConstructor withName(hydra.haskell.syntax.Name name) {
    return new OrdinaryConstructor(name, fields);
  }

  public OrdinaryConstructor withFields(java.util.List<hydra.haskell.syntax.Type> fields) {
    return new OrdinaryConstructor(name, fields);
  }
}
