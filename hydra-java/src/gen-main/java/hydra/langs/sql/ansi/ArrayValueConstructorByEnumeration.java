// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class ArrayValueConstructorByEnumeration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.ArrayValueConstructorByEnumeration");
  
  public final hydra.langs.sql.ansi.LeftBracketOrTrigraph leftBracketOrTrigraph;
  
  public final hydra.langs.sql.ansi.ArrayElementList arrayElementList;
  
  public final hydra.langs.sql.ansi.RightBracketOrTrigraph rightBracketOrTrigraph;
  
  public ArrayValueConstructorByEnumeration (hydra.langs.sql.ansi.LeftBracketOrTrigraph leftBracketOrTrigraph, hydra.langs.sql.ansi.ArrayElementList arrayElementList, hydra.langs.sql.ansi.RightBracketOrTrigraph rightBracketOrTrigraph) {
    if (leftBracketOrTrigraph == null) {
      throw new IllegalArgumentException("null value for 'leftBracketOrTrigraph' argument");
    }
    if (arrayElementList == null) {
      throw new IllegalArgumentException("null value for 'arrayElementList' argument");
    }
    if (rightBracketOrTrigraph == null) {
      throw new IllegalArgumentException("null value for 'rightBracketOrTrigraph' argument");
    }
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
  
  public ArrayValueConstructorByEnumeration withLeftBracketOrTrigraph(hydra.langs.sql.ansi.LeftBracketOrTrigraph leftBracketOrTrigraph) {
    if (leftBracketOrTrigraph == null) {
      throw new IllegalArgumentException("null value for 'leftBracketOrTrigraph' argument");
    }
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
  
  public ArrayValueConstructorByEnumeration withArrayElementList(hydra.langs.sql.ansi.ArrayElementList arrayElementList) {
    if (arrayElementList == null) {
      throw new IllegalArgumentException("null value for 'arrayElementList' argument");
    }
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
  
  public ArrayValueConstructorByEnumeration withRightBracketOrTrigraph(hydra.langs.sql.ansi.RightBracketOrTrigraph rightBracketOrTrigraph) {
    if (rightBracketOrTrigraph == null) {
      throw new IllegalArgumentException("null value for 'rightBracketOrTrigraph' argument");
    }
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
}