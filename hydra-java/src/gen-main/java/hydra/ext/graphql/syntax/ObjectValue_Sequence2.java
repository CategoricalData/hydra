package hydra.ext.graphql.syntax;

public class ObjectValue_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ObjectValue.Sequence2");
  
  public final java.util.List<hydra.ext.graphql.syntax.ObjectField> listOfObjectField;
  
  public ObjectValue_Sequence2 (java.util.List<hydra.ext.graphql.syntax.ObjectField> listOfObjectField) {
    this.listOfObjectField = listOfObjectField;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectValue_Sequence2)) {
      return false;
    }
    ObjectValue_Sequence2 o = (ObjectValue_Sequence2) (other);
    return listOfObjectField.equals(o.listOfObjectField);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfObjectField.hashCode();
  }
}