// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class ArrayValueConstructorByEnumeration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.ArrayValueConstructorByEnumeration");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_BRACKET_OR_TRIGRAPH = new hydra.core.Name("leftBracketOrTrigraph");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY_ELEMENT_LIST = new hydra.core.Name("arrayElementList");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT_BRACKET_OR_TRIGRAPH = new hydra.core.Name("rightBracketOrTrigraph");
  
  public final hydra.ext.org.ansi.sql.syntax.LeftBracketOrTrigraph leftBracketOrTrigraph;
  
  public final hydra.ext.org.ansi.sql.syntax.ArrayElementList arrayElementList;
  
  public final hydra.ext.org.ansi.sql.syntax.RightBracketOrTrigraph rightBracketOrTrigraph;
  
  public ArrayValueConstructorByEnumeration (hydra.ext.org.ansi.sql.syntax.LeftBracketOrTrigraph leftBracketOrTrigraph, hydra.ext.org.ansi.sql.syntax.ArrayElementList arrayElementList, hydra.ext.org.ansi.sql.syntax.RightBracketOrTrigraph rightBracketOrTrigraph) {
    java.util.Objects.requireNonNull((leftBracketOrTrigraph));
    java.util.Objects.requireNonNull((arrayElementList));
    java.util.Objects.requireNonNull((rightBracketOrTrigraph));
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
  
  public ArrayValueConstructorByEnumeration withLeftBracketOrTrigraph(hydra.ext.org.ansi.sql.syntax.LeftBracketOrTrigraph leftBracketOrTrigraph) {
    java.util.Objects.requireNonNull((leftBracketOrTrigraph));
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
  
  public ArrayValueConstructorByEnumeration withArrayElementList(hydra.ext.org.ansi.sql.syntax.ArrayElementList arrayElementList) {
    java.util.Objects.requireNonNull((arrayElementList));
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
  
  public ArrayValueConstructorByEnumeration withRightBracketOrTrigraph(hydra.ext.org.ansi.sql.syntax.RightBracketOrTrigraph rightBracketOrTrigraph) {
    java.util.Objects.requireNonNull((rightBracketOrTrigraph));
    return new ArrayValueConstructorByEnumeration(leftBracketOrTrigraph, arrayElementList, rightBracketOrTrigraph);
  }
}