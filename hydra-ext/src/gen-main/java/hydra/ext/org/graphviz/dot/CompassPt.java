// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public abstract class CompassPt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphviz.dot.CompassPt");
  
  public static final hydra.core.Name FIELD_NAME_N = new hydra.core.Name("n");
  
  public static final hydra.core.Name FIELD_NAME_NE = new hydra.core.Name("ne");
  
  public static final hydra.core.Name FIELD_NAME_E = new hydra.core.Name("e");
  
  public static final hydra.core.Name FIELD_NAME_SE = new hydra.core.Name("se");
  
  public static final hydra.core.Name FIELD_NAME_S = new hydra.core.Name("s");
  
  public static final hydra.core.Name FIELD_NAME_SW = new hydra.core.Name("sw");
  
  public static final hydra.core.Name FIELD_NAME_W = new hydra.core.Name("w");
  
  public static final hydra.core.Name FIELD_NAME_NW = new hydra.core.Name("nw");
  
  public static final hydra.core.Name FIELD_NAME_C = new hydra.core.Name("c");
  
  public static final hydra.core.Name FIELD_NAME_NONE = new hydra.core.Name("none");
  
  private CompassPt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(N instance) ;
    
    R visit(Ne instance) ;
    
    R visit(E instance) ;
    
    R visit(Se instance) ;
    
    R visit(S instance) ;
    
    R visit(Sw instance) ;
    
    R visit(W instance) ;
    
    R visit(Nw instance) ;
    
    R visit(C instance) ;
    
    R visit(None instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CompassPt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(N instance) {
      return otherwise((instance));
    }
    
    default R visit(Ne instance) {
      return otherwise((instance));
    }
    
    default R visit(E instance) {
      return otherwise((instance));
    }
    
    default R visit(Se instance) {
      return otherwise((instance));
    }
    
    default R visit(S instance) {
      return otherwise((instance));
    }
    
    default R visit(Sw instance) {
      return otherwise((instance));
    }
    
    default R visit(W instance) {
      return otherwise((instance));
    }
    
    default R visit(Nw instance) {
      return otherwise((instance));
    }
    
    default R visit(C instance) {
      return otherwise((instance));
    }
    
    default R visit(None instance) {
      return otherwise((instance));
    }
  }
  
  public static final class N extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public N () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof N)) {
        return false;
      }
      N o = (N) (other);
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
  
  public static final class Ne extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public Ne () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ne)) {
        return false;
      }
      Ne o = (Ne) (other);
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
  
  public static final class E extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public E () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof E)) {
        return false;
      }
      E o = (E) (other);
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
  
  public static final class Se extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public Se () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Se)) {
        return false;
      }
      Se o = (Se) (other);
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
  
  public static final class S extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public S () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof S)) {
        return false;
      }
      S o = (S) (other);
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
  
  public static final class Sw extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public Sw () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sw)) {
        return false;
      }
      Sw o = (Sw) (other);
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
  
  public static final class W extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public W () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof W)) {
        return false;
      }
      W o = (W) (other);
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
  
  public static final class Nw extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public Nw () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nw)) {
        return false;
      }
      Nw o = (Nw) (other);
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
  
  public static final class C extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public C () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof C)) {
        return false;
      }
      C o = (C) (other);
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
  
  public static final class None extends hydra.ext.org.graphviz.dot.CompassPt implements Serializable {
    public None () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) (other);
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
}