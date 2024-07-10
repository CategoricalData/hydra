// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tabular;

import java.io.Serializable;

/**
 * A simple table as in a CSV file, having an optional header row and any number of data rows
 */
public class Table<V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tabular.Table");
  
  /**
   * The optional header row of the table. If present, the header must have the same number of cells as each data row.
   */
  public final java.util.Optional<hydra.langs.tabular.HeaderRow> header;
  
  /**
   * The data rows of the table. Each row must have the same number of cells.
   */
  public final java.util.List<hydra.langs.tabular.DataRow<V>> data;
  
  public Table (java.util.Optional<hydra.langs.tabular.HeaderRow> header, java.util.List<hydra.langs.tabular.DataRow<V>> data) {
    if (header == null) {
      throw new IllegalArgumentException("null value for 'header' argument");
    }
    if (data == null) {
      throw new IllegalArgumentException("null value for 'data' argument");
    }
    this.header = header;
    this.data = data;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Table)) {
      return false;
    }
    Table o = (Table) (other);
    return header.equals(o.header) && data.equals(o.data);
  }
  
  @Override
  public int hashCode() {
    return 2 * header.hashCode() + 3 * data.hashCode();
  }
  
  public Table withHeader(java.util.Optional<hydra.langs.tabular.HeaderRow> header) {
    if (header == null) {
      throw new IllegalArgumentException("null value for 'header' argument");
    }
    return new Table(header, data);
  }
  
  public Table withData(java.util.List<hydra.langs.tabular.DataRow<V>> data) {
    if (data == null) {
      throw new IllegalArgumentException("null value for 'data' argument");
    }
    return new Table(header, data);
  }
}