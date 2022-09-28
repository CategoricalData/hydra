package hydra.ext.graphql.syntax;

public class ScalarTypeExtension {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ScalarTypeExtension");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.ext.graphql.syntax.Directives directives;
  
  public ScalarTypeExtension (hydra.ext.graphql.syntax.Name name, hydra.ext.graphql.syntax.Directives directives) {
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalarTypeExtension)) {
      return false;
    }
    ScalarTypeExtension o = (ScalarTypeExtension) (other);
    return name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode();
  }
  
  public ScalarTypeExtension withName(hydra.ext.graphql.syntax.Name name) {
    return new ScalarTypeExtension(name, directives);
  }
  
  public ScalarTypeExtension withDirectives(hydra.ext.graphql.syntax.Directives directives) {
    return new ScalarTypeExtension(name, directives);
  }
}