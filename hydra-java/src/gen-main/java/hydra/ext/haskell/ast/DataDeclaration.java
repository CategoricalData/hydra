// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A data type declaration
 */
public class DataDeclaration implements Serializable, Comparable<DataDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.ast.DataDeclaration");
  
  public static final hydra.core.Name KEYWORD = new hydra.core.Name("keyword");
  
  public static final hydra.core.Name CONTEXT = new hydra.core.Name("context");
  
  public static final hydra.core.Name HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name CONSTRUCTORS = new hydra.core.Name("constructors");
  
  public static final hydra.core.Name DERIVING = new hydra.core.Name("deriving");
  
  /**
   * The 'data' or 'newtype' keyword
   */
  public final hydra.ext.haskell.ast.DataOrNewtype keyword;
  
  /**
   * Type class constraints
   */
  public final hydra.util.ConsList<hydra.ext.haskell.ast.Assertion> context;
  
  /**
   * The declaration head
   */
  public final hydra.ext.haskell.ast.DeclarationHead head;
  
  /**
   * The data constructors
   */
  public final hydra.util.ConsList<hydra.ext.haskell.ast.ConstructorWithComments> constructors;
  
  /**
   * Derived type class instances
   */
  public final hydra.util.ConsList<hydra.ext.haskell.ast.Deriving> deriving;
  
  public DataDeclaration (hydra.ext.haskell.ast.DataOrNewtype keyword, hydra.util.ConsList<hydra.ext.haskell.ast.Assertion> context, hydra.ext.haskell.ast.DeclarationHead head, hydra.util.ConsList<hydra.ext.haskell.ast.ConstructorWithComments> constructors, hydra.util.ConsList<hydra.ext.haskell.ast.Deriving> deriving) {
    this.keyword = keyword;
    this.context = context;
    this.head = head;
    this.constructors = constructors;
    this.deriving = deriving;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataDeclaration)) {
      return false;
    }
    DataDeclaration o = (DataDeclaration) other;
    return java.util.Objects.equals(
      this.keyword,
      o.keyword) && java.util.Objects.equals(
      this.context,
      o.context) && java.util.Objects.equals(
      this.head,
      o.head) && java.util.Objects.equals(
      this.constructors,
      o.constructors) && java.util.Objects.equals(
      this.deriving,
      o.deriving);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keyword) + 3 * java.util.Objects.hashCode(context) + 5 * java.util.Objects.hashCode(head) + 7 * java.util.Objects.hashCode(constructors) + 11 * java.util.Objects.hashCode(deriving);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DataDeclaration other) {
    int cmp = 0;
    cmp = ((Comparable) keyword).compareTo(other.keyword);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      context.hashCode(),
      other.context.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      constructors.hashCode(),
      other.constructors.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      deriving.hashCode(),
      other.deriving.hashCode());
  }
  
  public DataDeclaration withKeyword(hydra.ext.haskell.ast.DataOrNewtype keyword) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withContext(hydra.util.ConsList<hydra.ext.haskell.ast.Assertion> context) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withHead(hydra.ext.haskell.ast.DeclarationHead head) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withConstructors(hydra.util.ConsList<hydra.ext.haskell.ast.ConstructorWithComments> constructors) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withDeriving(hydra.util.ConsList<hydra.ext.haskell.ast.Deriving> deriving) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
}
