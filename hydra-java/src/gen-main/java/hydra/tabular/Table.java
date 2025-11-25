// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

/**
 * A simple table as in a CSV file, having an optional header row and any number of data rows
 */
public class Table<V> {
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
  public final java.util.List<hydra.tabular.DataRow<Object>> data;
  
  public Table (hydra.util.Maybe<hydra.tabular.HeaderRow> header, java.util.List<hydra.tabular.DataRow<Object>> data) {
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((data));
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
  
  public Table withHeader(hydra.util.Maybe<hydra.tabular.HeaderRow> header) {
    java.util.Objects.requireNonNull((header));
    return new Table(header, data);
  }
  
  public Table withData(java.util.List<hydra.tabular.DataRow<Object>> data) {
    java.util.Objects.requireNonNull((data));
    return new Table(header, data);
  }
}
