package hydra.langs.xml.schema;

/**
 * See https://www.w3.org/TR/xmlschema-2/#non-fundamental
 */
public class ConstrainingFacet {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/xml/schema.ConstrainingFacet");
  
  public ConstrainingFacet () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstrainingFacet)) {
      return false;
    }
    ConstrainingFacet o = (ConstrainingFacet) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}