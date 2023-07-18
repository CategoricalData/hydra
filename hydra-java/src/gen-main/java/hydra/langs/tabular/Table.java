package hydra.langs.tabular;

public class Table {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tabular.Table");
  
  public final java.util.Optional<hydra.langs.tabular.HeaderRow> header;
  
  public final java.util.List<hydra.langs.tabular.DataRow> data;
  
  public Table (java.util.Optional<hydra.langs.tabular.HeaderRow> header, java.util.List<hydra.langs.tabular.DataRow> data) {
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
    return new Table(header, data);
  }
  
  public Table withData(java.util.List<hydra.langs.tabular.DataRow> data) {
    return new Table(header, data);
  }
}