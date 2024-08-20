// Note: this is an automatically generated file. Do not edit.

package hydra.ext.relationalModel;

import java.io.Serializable;

/**
 * An abstract specification of the domain represented by a column in a relation; a role
 */
public class ColumnSchema<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/relationalModel.ColumnSchema");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  public static final hydra.core.Name FIELD_NAME_IS_PRIMARY_KEY = new hydra.core.Name("isPrimaryKey");
  
  /**
   * A unique name for the column
   */
  public final hydra.ext.relationalModel.ColumnName name;
  
  /**
   * The domain (type) of the column
   */
  public final T domain;
  
  /**
   * Whether this column represents the primary key of its relation
   */
  public final Boolean isPrimaryKey;
  
  public ColumnSchema (hydra.ext.relationalModel.ColumnName name, T domain, Boolean isPrimaryKey) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((domain));
    java.util.Objects.requireNonNull((isPrimaryKey));
    this.name = name;
    this.domain = domain;
    this.isPrimaryKey = isPrimaryKey;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnSchema)) {
      return false;
    }
    ColumnSchema o = (ColumnSchema) (other);
    return name.equals(o.name) && domain.equals(o.domain) && isPrimaryKey.equals(o.isPrimaryKey);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * domain.hashCode() + 5 * isPrimaryKey.hashCode();
  }
  
  public ColumnSchema withName(hydra.ext.relationalModel.ColumnName name) {
    java.util.Objects.requireNonNull((name));
    return new ColumnSchema(name, domain, isPrimaryKey);
  }
  
  public ColumnSchema withDomain(T domain) {
    java.util.Objects.requireNonNull((domain));
    return new ColumnSchema(name, domain, isPrimaryKey);
  }
  
  public ColumnSchema withIsPrimaryKey(Boolean isPrimaryKey) {
    java.util.Objects.requireNonNull((isPrimaryKey));
    return new ColumnSchema(name, domain, isPrimaryKey);
  }
}
