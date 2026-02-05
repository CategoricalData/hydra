// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * An abstract specification of the domain represented by a column in a relation; a role
 */
public class ColumnSchema<T> implements Serializable, Comparable<ColumnSchema<T>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.ColumnSchema");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  /**
   * A unique name for the column
   */
  public final hydra.relational.ColumnName name;
  
  /**
   * The domain (type) of the column
   */
  public final T domain;
  
  public ColumnSchema (hydra.relational.ColumnName name, T domain) {
    this.name = name;
    this.domain = domain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnSchema)) {
      return false;
    }
    ColumnSchema o = (ColumnSchema) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.domain,
      o.domain);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(domain);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ColumnSchema other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (domain)).compareTo(other.domain);
  }
  
  public ColumnSchema withName(hydra.relational.ColumnName name) {
    return new ColumnSchema(name, domain);
  }
  
  public ColumnSchema withDomain(T domain) {
    return new ColumnSchema(name, domain);
  }
}
