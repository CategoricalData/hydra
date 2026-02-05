// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A simple table as in a CSV file, having an optional header row and any number of data rows
 */
public class Table<V> implements Serializable, Comparable<Table<V>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.Table");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_DATA = new hydra.core.Name("data");
  
  /**
   * The optional header row of the table. If present, the header must have the same number of cells as each data row.
   */
  public final hydra.util.Maybe<hydra.tabular.HeaderRow> header;
  
  /**
   * The data rows of the table. Each row must have the same number of cells.
   */
  public final java.util.List<hydra.tabular.DataRow<V>> data;
  
  public Table (hydra.util.Maybe<hydra.tabular.HeaderRow> header, java.util.List<hydra.tabular.DataRow<V>> data) {
    this.header = header;
    this.data = data;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Table)) {
      return false;
    }
    Table o = (Table) (other);
    return java.util.Objects.equals(
      this.header,
      o.header) && java.util.Objects.equals(
      this.data,
      o.data);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(header) + 3 * java.util.Objects.hashCode(data);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Table other) {
    int cmp = 0;
    cmp = Integer.compare(
      header.hashCode(),
      other.header.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      data.hashCode(),
      other.data.hashCode());
  }
  
  public Table withHeader(hydra.util.Maybe<hydra.tabular.HeaderRow> header) {
    return new Table(header, data);
  }
  
  public Table withData(java.util.List<hydra.tabular.DataRow<V>> data) {
    return new Table(header, data);
  }
}
