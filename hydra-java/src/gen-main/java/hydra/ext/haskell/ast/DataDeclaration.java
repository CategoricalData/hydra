package hydra.ext.haskell.ast;

/**
 * A data type declaration
 */
public class DataDeclaration {
  public final DataDeclaration_Keyword keyword;
  
  public final java.util.List<Assertion> context;
  
  public final DeclarationHead head;
  
  public final java.util.List<Constructor> constructors;
  
  public final java.util.List<Deriving> deriving;
  
  public DataDeclaration (DataDeclaration_Keyword keyword, java.util.List<Assertion> context, DeclarationHead head, java.util.List<Constructor> constructors, java.util.List<Deriving> deriving) {
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
  
  public DataDeclaration withKeyword(DataDeclaration_Keyword keyword) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withContext(java.util.List<Assertion> context) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withHead(DeclarationHead head) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withConstructors(java.util.List<Constructor> constructors) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
  
  public DataDeclaration withDeriving(java.util.List<Deriving> deriving) {
    return new DataDeclaration(keyword, context, head, constructors, deriving);
  }
}