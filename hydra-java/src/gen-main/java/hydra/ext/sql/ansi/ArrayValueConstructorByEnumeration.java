package hydra.ext.sql.ansi;

public class ArrayValueConstructorByEnumeration {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ArrayValueConstructorByEnumeration");
  
  public final hydra.ext.sql.ansi.LeftBracketOrTrigraph leftBracketOrTrigraph;
  
  public final hydra.ext.sql.ansi.ArrayElementList arrayElementList;
  
  public final hydra.ext.sql.ansi.RightBracketOrTrigraph rightBracketOrTrigraph;
  
  public ArrayValueConstructorByEnumeration (hydra.ext.sql.ansi.LeftBracketOrTrigraph leftBracketOrTrigraph, hydra.ext.sql.ansi.ArrayElementList arrayElementList, hydra.ext.sql.ansi.RightBracketOrTrigraph rightBracketOrTrigraph) {
    this.leftBracketOrTrigraph = leftBracketOrTrigraph;
    this.arrayElementList = arrayElementList;
    this.rightBracketOrTrigraph = rightBracketOrTrigraph;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayValueConstructorByEnumeration)) {
      return false;
    }
    ArrayValueConstructorByEnumeration o = (ArrayValueConstructorByEnumeration) (other);
    return leftBracketOrTrigraph.equals(o.leftBracketOrTrigraph) && arrayElementList.equals(o.arrayElementList) && rightBracketOrTrigraph.equals(o.rightBracketOrTrigraph);
  }
  
  @Override
  public int hashCode() {
    return 2 * leftBracketOrTrigraph.hashCode() + 3 * arrayElementList.hashCode() + 5 * rightBracketOrTrigraph.hashCode();
  }
  
  public ArrayValueConstructorByEnumeration withLeftBracketOrTrigraph(hydra.ext.sql.ansi.LeftBracketOrTrigraph leftBracketOrTrigraph) {
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
  
  public ArrayValueConstructorByEnumeration withArrayElementList(hydra.ext.sql.ansi.ArrayElementList arrayElementList) {
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
  
  public ArrayValueConstructorByEnumeration withRightBracketOrTrigraph(hydra.ext.sql.ansi.RightBracketOrTrigraph rightBracketOrTrigraph) {
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
}