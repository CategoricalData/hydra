package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class TabularExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.TabularExpression");
  
  private TabularExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Command instance) ;
    
    R visit(Pipeline instance) ;
    
    R visit(Let instance) ;
    
    R visit(Table instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TabularExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Command instance) {
      return otherwise((instance));
    }
    
    default R visit(Pipeline instance) {
      return otherwise((instance));
    }
    
    default R visit(Let instance) {
      return otherwise((instance));
    }
    
    default R visit(Table instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Command extends hydra.langs.kusto.kql.TabularExpression implements Serializable {
    public final hydra.langs.kusto.kql.Command value;
    
    public Command (hydra.langs.kusto.kql.Command value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Command)) {
        return false;
      }
      Command o = (Command) (other);
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
  
  public static final class Pipeline extends hydra.langs.kusto.kql.TabularExpression implements Serializable {
    public final hydra.langs.kusto.kql.PipelineExpression value;
    
    public Pipeline (hydra.langs.kusto.kql.PipelineExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pipeline)) {
        return false;
      }
      Pipeline o = (Pipeline) (other);
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
  
  public static final class Let extends hydra.langs.kusto.kql.TabularExpression implements Serializable {
    public final hydra.langs.kusto.kql.LetExpression value;
    
    public Let (hydra.langs.kusto.kql.LetExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) (other);
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
  
  public static final class Table extends hydra.langs.kusto.kql.TabularExpression implements Serializable {
    public final hydra.langs.kusto.kql.TableName value;
    
    public Table (hydra.langs.kusto.kql.TableName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Table)) {
        return false;
      }
      Table o = (Table) (other);
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