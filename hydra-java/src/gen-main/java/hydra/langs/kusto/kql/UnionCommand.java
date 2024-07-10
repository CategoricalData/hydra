// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class UnionCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.UnionCommand");
  
  public final java.util.List<hydra.langs.kusto.kql.Parameter> parameters;
  
  public final java.util.Optional<hydra.langs.kusto.kql.UnionKind> kind;
  
  public final java.util.Optional<hydra.langs.kusto.kql.ColumnName> withSource;
  
  public final java.util.Optional<Boolean> isFuzzy;
  
  public final java.util.List<hydra.langs.kusto.kql.TableName> tables;
  
  public UnionCommand (java.util.List<hydra.langs.kusto.kql.Parameter> parameters, java.util.Optional<hydra.langs.kusto.kql.UnionKind> kind, java.util.Optional<hydra.langs.kusto.kql.ColumnName> withSource, java.util.Optional<Boolean> isFuzzy, java.util.List<hydra.langs.kusto.kql.TableName> tables) {
    if (parameters == null) {
      throw new IllegalArgumentException("null value for 'parameters' argument");
    }
    if (kind == null) {
      throw new IllegalArgumentException("null value for 'kind' argument");
    }
    if (withSource == null) {
      throw new IllegalArgumentException("null value for 'withSource' argument");
    }
    if (isFuzzy == null) {
      throw new IllegalArgumentException("null value for 'isFuzzy' argument");
    }
    if (tables == null) {
      throw new IllegalArgumentException("null value for 'tables' argument");
    }
    this.parameters = parameters;
    this.kind = kind;
    this.withSource = withSource;
    this.isFuzzy = isFuzzy;
    this.tables = tables;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionCommand)) {
      return false;
    }
    UnionCommand o = (UnionCommand) (other);
    return parameters.equals(o.parameters) && kind.equals(o.kind) && withSource.equals(o.withSource) && isFuzzy.equals(o.isFuzzy) && tables.equals(o.tables);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode() + 3 * kind.hashCode() + 5 * withSource.hashCode() + 7 * isFuzzy.hashCode() + 11 * tables.hashCode();
  }
  
  public UnionCommand withParameters(java.util.List<hydra.langs.kusto.kql.Parameter> parameters) {
    if (parameters == null) {
      throw new IllegalArgumentException("null value for 'parameters' argument");
    }
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withKind(java.util.Optional<hydra.langs.kusto.kql.UnionKind> kind) {
    if (kind == null) {
      throw new IllegalArgumentException("null value for 'kind' argument");
    }
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withWithSource(java.util.Optional<hydra.langs.kusto.kql.ColumnName> withSource) {
    if (withSource == null) {
      throw new IllegalArgumentException("null value for 'withSource' argument");
    }
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withIsFuzzy(java.util.Optional<Boolean> isFuzzy) {
    if (isFuzzy == null) {
      throw new IllegalArgumentException("null value for 'isFuzzy' argument");
    }
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withTables(java.util.List<hydra.langs.kusto.kql.TableName> tables) {
    if (tables == null) {
      throw new IllegalArgumentException("null value for 'tables' argument");
    }
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
}