// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

/**
 * An abstract specification of the domain represented by a column in a relation; a role
 */
public class ColumnSchema<T> {
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
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((domain));
    this.name = name;
    this.domain = domain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnSchema)) {
      return false;
    }
    ColumnSchema o = (ColumnSchema) (other);
    return name.equals(o.name) && domain.equals(o.domain);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * domain.hashCode();
  }
  
  public ColumnSchema withName(hydra.relational.ColumnName name) {
    java.util.Objects.requireNonNull((name));
    return new ColumnSchema(name, domain);
  }
  
  public ColumnSchema withDomain(T domain) {
    java.util.Objects.requireNonNull((domain));
    return new ColumnSchema(name, domain);
  }
}
