package hydra.langs.relationalModel;

/**
 * A unique relation (table) name
 */
public class RelationName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/relationalModel.RelationName");
  
  /**
   * A unique relation (table) name
   */
  public final String value;
  
  public RelationName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationName)) {
      return false;
    }
    RelationName o = (RelationName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}