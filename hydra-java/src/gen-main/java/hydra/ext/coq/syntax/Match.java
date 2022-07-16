package hydra.ext.coq.syntax;

public class Match {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<CaseItem> caseItems;
  
  public final java.util.Optional<Term100> return_;
  
  public final Boolean pipe;
  
  public final java.util.List<Equation> equations;
  
  public Match (java.util.List<CaseItem> caseItems, java.util.Optional<Term100> return_, Boolean pipe, java.util.List<Equation> equations) {
    this.caseItems = caseItems;
    this.return_ = return_;
    this.pipe = pipe;
    this.equations = equations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Match)) {
      return false;
    }
    Match o = (Match) (other);
    return caseItems.equals(o.caseItems) && return_.equals(o.return_) && pipe.equals(o.pipe) && equations.equals(o.equations);
  }
  
  @Override
  public int hashCode() {
    return 2 * caseItems.hashCode() + 3 * return_.hashCode() + 5 * pipe.hashCode() + 7 * equations.hashCode();
  }
  
  public Match withCaseItems(java.util.List<CaseItem> caseItems) {
    return new Match(caseItems, return_, pipe, equations);
  }
  
  public Match withReturn(java.util.Optional<Term100> return_) {
    return new Match(caseItems, return_, pipe, equations);
  }
  
  public Match withPipe(Boolean pipe) {
    return new Match(caseItems, return_, pipe, equations);
  }
  
  public Match withEquations(java.util.List<Equation> equations) {
    return new Match(caseItems, return_, pipe, equations);
  }
}