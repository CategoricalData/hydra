package hydra.ext.graphql.syntax;

public class SchemaExtension_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.SchemaExtension.Sequence2");
  
  public final hydra.ext.graphql.syntax.Directives directives;
  
  public SchemaExtension_Sequence2 (hydra.ext.graphql.syntax.Directives directives) {
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaExtension_Sequence2)) {
      return false;
    }
    SchemaExtension_Sequence2 o = (SchemaExtension_Sequence2) (other);
    return directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * directives.hashCode();
  }
}