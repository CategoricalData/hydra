// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class UnionCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.UnionCommand");
  
  public final java.util.List<hydra.langs.kusto.kql.Parameter> parameters;
  
  public final hydra.util.Opt<hydra.langs.kusto.kql.UnionKind> kind;
  
  public final hydra.util.Opt<hydra.langs.kusto.kql.ColumnName> withSource;
  
  public final hydra.util.Opt<Boolean> isFuzzy;
  
  public final java.util.List<hydra.langs.kusto.kql.TableName> tables;
  
  public UnionCommand (java.util.List<hydra.langs.kusto.kql.Parameter> parameters, hydra.util.Opt<hydra.langs.kusto.kql.UnionKind> kind, hydra.util.Opt<hydra.langs.kusto.kql.ColumnName> withSource, hydra.util.Opt<Boolean> isFuzzy, java.util.List<hydra.langs.kusto.kql.TableName> tables) {
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((kind));
    java.util.Objects.requireNonNull((withSource));
    java.util.Objects.requireNonNull((isFuzzy));
    java.util.Objects.requireNonNull((tables));
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
    java.util.Objects.requireNonNull((parameters));
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withKind(hydra.util.Opt<hydra.langs.kusto.kql.UnionKind> kind) {
    java.util.Objects.requireNonNull((kind));
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withWithSource(hydra.util.Opt<hydra.langs.kusto.kql.ColumnName> withSource) {
    java.util.Objects.requireNonNull((withSource));
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withIsFuzzy(hydra.util.Opt<Boolean> isFuzzy) {
    java.util.Objects.requireNonNull((isFuzzy));
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
  
  public UnionCommand withTables(java.util.List<hydra.langs.kusto.kql.TableName> tables) {
    java.util.Objects.requireNonNull((tables));
    return new UnionCommand(parameters, kind, withSource, isFuzzy, tables);
  }
}