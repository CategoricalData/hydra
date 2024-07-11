// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class ColumnDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.ColumnDefinition");
  
  public final hydra.langs.sql.ansi.ColumnName name;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.ReferenceScopeCheck> refScope;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration;
  
  public final java.util.List<hydra.langs.sql.ansi.ColumnConstraintDefinition> constraints;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate;
  
  public ColumnDefinition (hydra.langs.sql.ansi.ColumnName name, hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain, hydra.util.Opt<hydra.langs.sql.ansi.ReferenceScopeCheck> refScope, hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration, java.util.List<hydra.langs.sql.ansi.ColumnConstraintDefinition> constraints, hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (typeOrDomain == null) {
      throw new IllegalArgumentException("null value for 'typeOrDomain' argument");
    }
    if (refScope == null) {
      throw new IllegalArgumentException("null value for 'refScope' argument");
    }
    if (defaultOrIdentityOrGeneration == null) {
      throw new IllegalArgumentException("null value for 'defaultOrIdentityOrGeneration' argument");
    }
    if (constraints == null) {
      throw new IllegalArgumentException("null value for 'constraints' argument");
    }
    if (collate == null) {
      throw new IllegalArgumentException("null value for 'collate' argument");
    }
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
  
  public ColumnDefinition withName(hydra.langs.sql.ansi.ColumnName name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withTypeOrDomain(hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain) {
    if (typeOrDomain == null) {
      throw new IllegalArgumentException("null value for 'typeOrDomain' argument");
    }
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withRefScope(hydra.util.Opt<hydra.langs.sql.ansi.ReferenceScopeCheck> refScope) {
    if (refScope == null) {
      throw new IllegalArgumentException("null value for 'refScope' argument");
    }
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withDefaultOrIdentityOrGeneration(hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration) {
    if (defaultOrIdentityOrGeneration == null) {
      throw new IllegalArgumentException("null value for 'defaultOrIdentityOrGeneration' argument");
    }
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withConstraints(java.util.List<hydra.langs.sql.ansi.ColumnConstraintDefinition> constraints) {
    if (constraints == null) {
      throw new IllegalArgumentException("null value for 'constraints' argument");
    }
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withCollate(hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    if (collate == null) {
      throw new IllegalArgumentException("null value for 'collate' argument");
    }
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
}