// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public abstract class Command implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/kusto/kql.Command");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name FIELD_NAME_DISTINCT = new hydra.core.Name("distinct");
  
  public static final hydra.core.Name FIELD_NAME_EXTEND = new hydra.core.Name("extend");
  
  public static final hydra.core.Name FIELD_NAME_JOIN = new hydra.core.Name("join");
  
  public static final hydra.core.Name FIELD_NAME_LIMIT = new hydra.core.Name("limit");
  
  public static final hydra.core.Name FIELD_NAME_MVEXPAND = new hydra.core.Name("mvexpand");
  
  public static final hydra.core.Name FIELD_NAME_ORDER_BY = new hydra.core.Name("orderBy");
  
  public static final hydra.core.Name FIELD_NAME_PARSE = new hydra.core.Name("parse");
  
  public static final hydra.core.Name FIELD_NAME_PRINT = new hydra.core.Name("print");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT = new hydra.core.Name("project");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT_AWAY = new hydra.core.Name("projectAway");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT_RENAME = new hydra.core.Name("projectRename");
  
  public static final hydra.core.Name FIELD_NAME_RENDER = new hydra.core.Name("render");
  
  public static final hydra.core.Name FIELD_NAME_SEARCH = new hydra.core.Name("search");
  
  public static final hydra.core.Name FIELD_NAME_SORT_BY = new hydra.core.Name("sortBy");
  
  public static final hydra.core.Name FIELD_NAME_SUMMARIZE = new hydra.core.Name("summarize");
  
  public static final hydra.core.Name FIELD_NAME_TAKE = new hydra.core.Name("take");
  
  public static final hydra.core.Name FIELD_NAME_TOP = new hydra.core.Name("top");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
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
  
  public static final class Count extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
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
  public static final class Distinct extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnName> value;
    
    public Distinct (java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnName> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Extend extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnAssignment> value;
    
    public Extend (java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnAssignment> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Join extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.JoinCommand value;
    
    public Join (hydra.ext.com.microsoft.kusto.kql.JoinCommand value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Limit extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final Integer value;
    
    public Limit (Integer value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Mvexpand extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.ColumnName value;
    
    public Mvexpand (hydra.ext.com.microsoft.kusto.kql.ColumnName value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class OrderBy extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> value;
    
    public OrderBy (java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Parse extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.ParseCommand value;
    
    public Parse (hydra.ext.com.microsoft.kusto.kql.ParseCommand value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Print extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.PrintCommand value;
    
    public Print (hydra.ext.com.microsoft.kusto.kql.PrintCommand value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Project extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.Projection> value;
    
    public Project (java.util.List<hydra.ext.com.microsoft.kusto.kql.Projection> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class ProjectAway extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnName> value;
    
    public ProjectAway (java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnName> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class ProjectRename extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnAlias> value;
    
    public ProjectRename (java.util.List<hydra.ext.com.microsoft.kusto.kql.ColumnAlias> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Render extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final String value;
    
    public Render (String value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Search extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.SearchCommand value;
    
    public Search (hydra.ext.com.microsoft.kusto.kql.SearchCommand value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class SortBy extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> value;
    
    public SortBy (java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Summarize extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.SummarizeCommand value;
    
    public Summarize (hydra.ext.com.microsoft.kusto.kql.SummarizeCommand value) {
      java.util.Objects.requireNonNull((value));
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
  public static final class Take extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final Integer value;
    
    public Take (Integer value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Top extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.TopCommand value;
    
    public Top (hydra.ext.com.microsoft.kusto.kql.TopCommand value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Union extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.UnionCommand value;
    
    public Union (hydra.ext.com.microsoft.kusto.kql.UnionCommand value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Where extends hydra.ext.com.microsoft.kusto.kql.Command implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.Expression value;
    
    public Where (hydra.ext.com.microsoft.kusto.kql.Expression value) {
      java.util.Objects.requireNonNull((value));
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