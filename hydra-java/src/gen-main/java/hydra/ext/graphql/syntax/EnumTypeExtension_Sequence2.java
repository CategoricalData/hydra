package hydra.ext.graphql.syntax;

public class EnumTypeExtension_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.EnumTypeExtension.Sequence2");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.ext.graphql.syntax.Directives directives;
  
  public EnumTypeExtension_Sequence2 (hydra.ext.graphql.syntax.Name name, hydra.ext.graphql.syntax.Directives directives) {
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumTypeExtension_Sequence2)) {
      return false;
    }
    EnumTypeExtension_Sequence2 o = (EnumTypeExtension_Sequence2) (other);
    return name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode();
  }
  
  public EnumTypeExtension_Sequence2 withName(hydra.ext.graphql.syntax.Name name) {
    return new EnumTypeExtension_Sequence2(name, directives);
  }
  
  public EnumTypeExtension_Sequence2 withDirectives(hydra.ext.graphql.syntax.Directives directives) {
    return new EnumTypeExtension_Sequence2(name, directives);
  }
}