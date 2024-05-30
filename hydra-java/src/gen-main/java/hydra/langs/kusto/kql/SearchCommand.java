package hydra.langs.kusto.kql;

import java.io.Serializable;

/**
 * Search across all datasets and columns or, if provided, specific datasets and/or columns
 */
public class SearchCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.SearchCommand");
  
  public final java.util.List<hydra.langs.kusto.kql.TableName> datasets;
  
  public final hydra.langs.kusto.kql.Expression pattern;
  
  public SearchCommand (java.util.List<hydra.langs.kusto.kql.TableName> datasets, hydra.langs.kusto.kql.Expression pattern) {
    this.datasets = datasets;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SearchCommand)) {
      return false;
    }
    SearchCommand o = (SearchCommand) (other);
    return datasets.equals(o.datasets) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * datasets.hashCode() + 3 * pattern.hashCode();
  }
  
  public SearchCommand withDatasets(java.util.List<hydra.langs.kusto.kql.TableName> datasets) {
    return new SearchCommand(datasets, pattern);
  }
  
  public SearchCommand withPattern(hydra.langs.kusto.kql.Expression pattern) {
    return new SearchCommand(datasets, pattern);
  }
}