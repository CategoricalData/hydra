// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class ColumnDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/sql/ansi.ColumnDefinition");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_OR_DOMAIN = new hydra.core.Name("typeOrDomain");
  
  public static final hydra.core.Name FIELD_NAME_REF_SCOPE = new hydra.core.Name("refScope");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_OR_IDENTITY_OR_GENERATION = new hydra.core.Name("defaultOrIdentityOrGeneration");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_COLLATE = new hydra.core.Name("collate");
  
  public final hydra.langs.sql.ansi.ColumnName name;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.ReferenceScopeCheck> refScope;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration;
  
  public final java.util.List<hydra.langs.sql.ansi.ColumnConstraintDefinition> constraints;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate;
  
  public ColumnDefinition (hydra.langs.sql.ansi.ColumnName name, hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain, hydra.util.Opt<hydra.langs.sql.ansi.ReferenceScopeCheck> refScope, hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration, java.util.List<hydra.langs.sql.ansi.ColumnConstraintDefinition> constraints, hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typeOrDomain));
    java.util.Objects.requireNonNull((refScope));
    java.util.Objects.requireNonNull((defaultOrIdentityOrGeneration));
    java.util.Objects.requireNonNull((constraints));
    java.util.Objects.requireNonNull((collate));
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
    java.util.Objects.requireNonNull((name));
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withTypeOrDomain(hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_TypeOrDomain_Option> typeOrDomain) {
    java.util.Objects.requireNonNull((typeOrDomain));
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withRefScope(hydra.util.Opt<hydra.langs.sql.ansi.ReferenceScopeCheck> refScope) {
    java.util.Objects.requireNonNull((refScope));
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withDefaultOrIdentityOrGeneration(hydra.util.Opt<hydra.langs.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option> defaultOrIdentityOrGeneration) {
    java.util.Objects.requireNonNull((defaultOrIdentityOrGeneration));
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withConstraints(java.util.List<hydra.langs.sql.ansi.ColumnConstraintDefinition> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
  
  public ColumnDefinition withCollate(hydra.util.Opt<hydra.langs.sql.ansi.CollateClause> collate) {
    java.util.Objects.requireNonNull((collate));
    return new ColumnDefinition(name, typeOrDomain, refScope, defaultOrIdentityOrGeneration, constraints, collate);
  }
}