// Note: this is an automatically generated file. Do not edit.

package hydra.ext.kusto.kql;

import java.io.Serializable;

/**
 * Search across all datasets and columns or, if provided, specific datasets and/or columns
 */
public class SearchCommand implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/kusto/kql.SearchCommand");
  
  public static final hydra.core.Name FIELD_NAME_DATASETS = new hydra.core.Name("datasets");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final java.util.List<hydra.ext.kusto.kql.TableName> datasets;
  
  public final hydra.ext.kusto.kql.Expression pattern;
  
  public SearchCommand (java.util.List<hydra.ext.kusto.kql.TableName> datasets, hydra.ext.kusto.kql.Expression pattern) {
    java.util.Objects.requireNonNull((datasets));
    java.util.Objects.requireNonNull((pattern));
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
  
  public SearchCommand withDatasets(java.util.List<hydra.ext.kusto.kql.TableName> datasets) {
    java.util.Objects.requireNonNull((datasets));
    return new SearchCommand(datasets, pattern);
  }
  
  public SearchCommand withPattern(hydra.ext.kusto.kql.Expression pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new SearchCommand(datasets, pattern);
  }
}