package hydra.ext.scala.meta;

public class ScalaSymbol {
  public final String name;
  
  public ScalaSymbol (String name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalaSymbol)) {
      return false;
    }
    ScalaSymbol o = (ScalaSymbol) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}