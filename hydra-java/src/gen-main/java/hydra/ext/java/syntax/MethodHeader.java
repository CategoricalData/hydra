package hydra.ext.java.syntax;

public class MethodHeader {
  public final java.util.List<TypeParameter> parameters;
  
  public final Result result;
  
  public final MethodDeclarator declarator;
  
  public final java.util.Optional<Throws> throws_;
  
  public MethodHeader (java.util.List<TypeParameter> parameters, Result result, MethodDeclarator declarator, java.util.Optional<Throws> throws_) {
    this.parameters = parameters;
    this.result = result;
    this.declarator = declarator;
    this.throws_ = throws_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodHeader)) {
      return false;
    }
    MethodHeader o = (MethodHeader) (other);
    return parameters.equals(o.parameters) && result.equals(o.result) && declarator.equals(o.declarator) && throws_.equals(o.throws_);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode() + 3 * result.hashCode() + 5 * declarator.hashCode() + 7 * throws_.hashCode();
  }
  
  public MethodHeader withParameters(java.util.List<TypeParameter> parameters) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withResult(Result result) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withDeclarator(MethodDeclarator declarator) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withThrows(java.util.Optional<Throws> throws_) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
}