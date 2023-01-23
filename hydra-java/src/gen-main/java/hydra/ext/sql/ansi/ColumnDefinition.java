package hydra.ext.sql.ansi;

public class ColumnDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ColumnDefinition");
  
  public final hydra.ext.sql.ansi.ColumnName name;
  
  public final java.util.Optional<hydra.ext.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain;
  
  public final java.util.Optional<hydra.ext.sql.ansi.ReferenceScopeCheck> refScope;
  
  public final java.util.Optional<hydra.ext.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration;
  
  public final java.util.List<hydra.ext.sql.ansi.ColumnConstraintDefinition> constraints;
  
  public final java.util.Optional<hydra.ext.sql.ansi.CollateClause> collate;
  
  public ColumnDefinition (hydra.ext.sql.ansi.ColumnName name, java.util.Optional<hydra.ext.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain, java.util.Optional<hydra.ext.sql.ansi.ReferenceScopeCheck> refScope, java.util.Optional<hydra.ext.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration, java.util.List<hydra.ext.sql.ansi.ColumnConstraintDefinition> constraints, java.util.Optional<hydra.ext.sql.ansi.CollateClause> collate) {
    this.name = name;
    this.typeOrDomain = typeOrDomain;
    this.refScope = refScope;
    this.defaultOrIdentityOrGeneration = defaultOrIdentityOrGeneration;
    this.constraints = constraints;
    this.collate = collate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnDefinition)) {
      return false;
    }
    ColumnDefinition o = (ColumnDefinition) (other);
    return name.equals(o.name) && typeOrDomain.equals(o.typeOrDomain) && refScope.equals(o.refScope) && defaultOrIdentityOrGeneration.equals(o.defaultOrIdentityOrGeneration) && constraints.equals(o.constraints) && collate.equals(o.collate);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typeOrDomain.hashCode() + 5 * refScope.hashCode() + 7 * defaultOrIdentityOrGeneration.hashCode() + 11 * constraints.hashCode() + 13 * collate.hashCode();
  }
  
  public ColumnDefinition withName(hydra.ext.sql.ansi.ColumnName name) {
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withTypeOrDomain(java.util.Optional<hydra.ext.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain) {
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withRefScope(java.util.Optional<hydra.ext.sql.ansi.ReferenceScopeCheck> refScope) {
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withDefaultOrIdentityOrGeneration(java.util.Optional<hydra.ext.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration) {
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withConstraints(java.util.List<hydra.ext.sql.ansi.ColumnConstraintDefinition> constraints) {
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withCollate(java.util.Optional<hydra.ext.sql.ansi.CollateClause> collate) {
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
}