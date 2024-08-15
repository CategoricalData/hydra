// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * A data type declaration
 */
public class DataDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.DataDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORD = new hydra.core.Name("keyword");
  
  public static final hydra.core.Name FIELD_NAME_CONTEXT = new hydra.core.Name("context");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTORS = new hydra.core.Name("constructors");
  
  public static final hydra.core.Name FIELD_NAME_DERIVING = new hydra.core.Name("deriving");
  
  public final hydra.langs.haskell.ast.DataDeclaration_Keyword keyword;
  
  public final java.util.List<hydra.langs.haskell.ast.Assertion> context;
  
  public final hydra.langs.haskell.ast.DeclarationHead head;
  
  public final java.util.List<hydra.langs.haskell.ast.ConstructorWithComments> constructors;
  
  public final java.util.List<hydra.langs.haskell.ast.Deriving> deriving;
  
  public DataDeclaration (hydra.langs.haskell.ast.DataDeclaration_Keyword keyword, java.util.List<hydra.langs.haskell.ast.Assertion> context, hydra.langs.haskell.ast.DeclarationHead head, java.util.List<hydra.langs.haskell.ast.ConstructorWithComments> constructors, java.util.List<hydra.langs.haskell.ast.Deriving> deriving) {
    java.util.Objects.requireNonNull((keyword));
    java.util.Objects.requireNonNull((context));
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((constructors));
    java.util.Objects.requireNonNull((deriving));
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
    DataDeclaration o = (DataDeclaration) (other);
    return keyword.equals(o.keyword) && context.equals(o.context) && head.equals(o.head) && constructors.equals(o.constructors) && deriving.equals(o.deriving);
  }
  
  @Override
  public int hashCode() {
    return 2 * keyword.hashCode() + 3 * context.hashCode() + 5 * head.hashCode() + 7 * constructors.hashCode() + 11 * deriving.hashCode();
  }
  
  public DataDeclaration withKeyword(hydra.langs.haskell.ast.DataDeclaration_Keyword keyword) {
    java.util.Objects.requireNonNull((keyword));
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withContext(java.util.List<hydra.langs.haskell.ast.Assertion> context) {
    java.util.Objects.requireNonNull((context));
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withHead(hydra.langs.haskell.ast.DeclarationHead head) {
    java.util.Objects.requireNonNull((head));
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withConstructors(java.util.List<hydra.langs.haskell.ast.ConstructorWithComments> constructors) {
    java.util.Objects.requireNonNull((constructors));
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withDeriving(java.util.List<hydra.langs.haskell.ast.Deriving> deriving) {
    java.util.Objects.requireNonNull((deriving));
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
}