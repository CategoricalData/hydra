// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

public abstract class PageType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.parquet.format.PageType");
  
  public static final hydra.core.Name FIELD_NAME_DATA_PAGE = new hydra.core.Name("dataPage");
  
  public static final hydra.core.Name FIELD_NAME_INDEX_PAGE = new hydra.core.Name("indexPage");
  
  public static final hydra.core.Name FIELD_NAME_DICTIONARY_PAGE = new hydra.core.Name("dictionaryPage");
  
  public static final hydra.core.Name FIELD_NAME_DATA_PAGE_V2 = new hydra.core.Name("dataPageV2");
  
  private PageType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(DataPage instance) ;
    
    R visit(IndexPage instance) ;
    
    R visit(DictionaryPage instance) ;
    
    R visit(DataPageV2 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PageType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(DataPage instance) {
      return otherwise((instance));
    }
    
    default R visit(IndexPage instance) {
      return otherwise((instance));
    }
    
    default R visit(DictionaryPage instance) {
      return otherwise((instance));
    }
    
    default R visit(DataPageV2 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class DataPage extends hydra.ext.org.apache.parquet.format.PageType implements Serializable {
    public DataPage () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataPage)) {
        return false;
      }
      DataPage o = (DataPage) (other);
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
  
  public static final class IndexPage extends hydra.ext.org.apache.parquet.format.PageType implements Serializable {
    public IndexPage () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IndexPage)) {
        return false;
      }
      IndexPage o = (IndexPage) (other);
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
  
  public static final class DictionaryPage extends hydra.ext.org.apache.parquet.format.PageType implements Serializable {
    public DictionaryPage () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DictionaryPage)) {
        return false;
      }
      DictionaryPage o = (DictionaryPage) (other);
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
  
  public static final class DataPageV2 extends hydra.ext.org.apache.parquet.format.PageType implements Serializable {
    public DataPageV2 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataPageV2)) {
        return false;
      }
      DataPageV2 o = (DataPageV2) (other);
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