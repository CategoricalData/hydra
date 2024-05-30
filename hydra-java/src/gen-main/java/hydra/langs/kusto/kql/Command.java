package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class Command implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Command");
  
  private Command () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Count instance) ;
    
    R visit(Distinct instance) ;
    
    R visit(Extend instance) ;
    
    R visit(Join instance) ;
    
    R visit(Limit instance) ;
    
    R visit(Mvexpand instance) ;
    
    R visit(OrderBy instance) ;
    
    R visit(Parse instance) ;
    
    R visit(Print instance) ;
    
    R visit(Project instance) ;
    
    R visit(ProjectAway instance) ;
    
    R visit(ProjectRename instance) ;
    
    R visit(Render instance) ;
    
    R visit(Search instance) ;
    
    R visit(SortBy instance) ;
    
    R visit(Summarize instance) ;
    
    R visit(Take instance) ;
    
    R visit(Top instance) ;
    
    R visit(Union instance) ;
    
    R visit(Where instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Command instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Count instance) {
      return otherwise((instance));
    }
    
    default R visit(Distinct instance) {
      return otherwise((instance));
    }
    
    default R visit(Extend instance) {
      return otherwise((instance));
    }
    
    default R visit(Join instance) {
      return otherwise((instance));
    }
    
    default R visit(Limit instance) {
      return otherwise((instance));
    }
    
    default R visit(Mvexpand instance) {
      return otherwise((instance));
    }
    
    default R visit(OrderBy instance) {
      return otherwise((instance));
    }
    
    default R visit(Parse instance) {
      return otherwise((instance));
    }
    
    default R visit(Print instance) {
      return otherwise((instance));
    }
    
    default R visit(Project instance) {
      return otherwise((instance));
    }
    
    default R visit(ProjectAway instance) {
      return otherwise((instance));
    }
    
    default R visit(ProjectRename instance) {
      return otherwise((instance));
    }
    
    default R visit(Render instance) {
      return otherwise((instance));
    }
    
    default R visit(Search instance) {
      return otherwise((instance));
    }
    
    default R visit(SortBy instance) {
      return otherwise((instance));
    }
    
    default R visit(Summarize instance) {
      return otherwise((instance));
    }
    
    default R visit(Take instance) {
      return otherwise((instance));
    }
    
    default R visit(Top instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
    
    default R visit(Where instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Count extends hydra.langs.kusto.kql.Command implements Serializable {
    public Count () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * See https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/distinct-operator
   */
  public static final class Distinct extends hydra.langs.kusto.kql.Command implements Serializable {
    /**
     * See https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/distinct-operator
     */
    public final java.util.List<hydra.langs.kusto.kql.ColumnName> value;
    
    public Distinct (java.util.List<hydra.langs.kusto.kql.ColumnName> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Distinct)) {
        return false;
      }
      Distinct o = (Distinct) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Extend extends hydra.langs.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.ColumnAssignment> value;
    
    public Extend (java.util.List<hydra.langs.kusto.kql.ColumnAssignment> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extend)) {
        return false;
      }
      Extend o = (Extend) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Join extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.JoinCommand value;
    
    public Join (hydra.langs.kusto.kql.JoinCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Join)) {
        return false;
      }
      Join o = (Join) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Limit extends hydra.langs.kusto.kql.Command implements Serializable {
    public final Integer value;
    
    public Limit (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Limit)) {
        return false;
      }
      Limit o = (Limit) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Mvexpand extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.ColumnName value;
    
    public Mvexpand (hydra.langs.kusto.kql.ColumnName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mvexpand)) {
        return false;
      }
      Mvexpand o = (Mvexpand) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class OrderBy extends hydra.langs.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.SortBy> value;
    
    public OrderBy (java.util.List<hydra.langs.kusto.kql.SortBy> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrderBy)) {
        return false;
      }
      OrderBy o = (OrderBy) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Parse extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.ParseCommand value;
    
    public Parse (hydra.langs.kusto.kql.ParseCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parse)) {
        return false;
      }
      Parse o = (Parse) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Print extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.PrintCommand value;
    
    public Print (hydra.langs.kusto.kql.PrintCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Print)) {
        return false;
      }
      Print o = (Print) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Project extends hydra.langs.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.Projection> value;
    
    public Project (java.util.List<hydra.langs.kusto.kql.Projection> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Project)) {
        return false;
      }
      Project o = (Project) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ProjectAway extends hydra.langs.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.ColumnName> value;
    
    public ProjectAway (java.util.List<hydra.langs.kusto.kql.ColumnName> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ProjectAway)) {
        return false;
      }
      ProjectAway o = (ProjectAway) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ProjectRename extends hydra.langs.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.ColumnAlias> value;
    
    public ProjectRename (java.util.List<hydra.langs.kusto.kql.ColumnAlias> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ProjectRename)) {
        return false;
      }
      ProjectRename o = (ProjectRename) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Render extends hydra.langs.kusto.kql.Command implements Serializable {
    public final String value;
    
    public Render (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Render)) {
        return false;
      }
      Render o = (Render) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Search extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.SearchCommand value;
    
    public Search (hydra.langs.kusto.kql.SearchCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Search)) {
        return false;
      }
      Search o = (Search) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SortBy extends hydra.langs.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.SortBy> value;
    
    public SortBy (java.util.List<hydra.langs.kusto.kql.SortBy> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SortBy)) {
        return false;
      }
      SortBy o = (SortBy) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Summarize extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.SummarizeCommand value;
    
    public Summarize (hydra.langs.kusto.kql.SummarizeCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Summarize)) {
        return false;
      }
      Summarize o = (Summarize) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Limit a search to a specified number of results
   */
  public static final class Take extends hydra.langs.kusto.kql.Command implements Serializable {
    /**
     * Limit a search to a specified number of results
     */
    public final Integer value;
    
    public Take (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Take)) {
        return false;
      }
      Take o = (Take) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Top extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.TopCommand value;
    
    public Top (hydra.langs.kusto.kql.TopCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Top)) {
        return false;
      }
      Top o = (Top) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Union extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.UnionCommand value;
    
    public Union (hydra.langs.kusto.kql.UnionCommand value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Where extends hydra.langs.kusto.kql.Command implements Serializable {
    public final hydra.langs.kusto.kql.Expression value;
    
    public Where (hydra.langs.kusto.kql.Expression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Where)) {
        return false;
      }
      Where o = (Where) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}