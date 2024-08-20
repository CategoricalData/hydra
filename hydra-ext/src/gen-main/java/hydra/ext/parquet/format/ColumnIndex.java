// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * Description for ColumnIndex. Each &lt;array-field&gt;[i] refers to the page at OffsetIndex.page_locations[i]
 */
public class ColumnIndex implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.ColumnIndex");
  
  public static final hydra.core.Name FIELD_NAME_NULL_PAGES = new hydra.core.Name("nullPages");
  
  public static final hydra.core.Name FIELD_NAME_MIN_VALUES = new hydra.core.Name("minValues");
  
  public static final hydra.core.Name FIELD_NAME_MAX_VALUES = new hydra.core.Name("maxValues");
  
  public static final hydra.core.Name FIELD_NAME_BOUNDARY_ORDER = new hydra.core.Name("boundaryOrder");
  
  public static final hydra.core.Name FIELD_NAME_NULL_COUNTS = new hydra.core.Name("nullCounts");
  
  /**
   * A list of Boolean values to determine the validity of the corresponding min and max values. If true, a page contains only null values, and writers have to set the corresponding entries in min_values and max_values to byte[0], so that all lists have the same length. If false, the corresponding entries in min_values and max_values must be valid.
   */
  public final java.util.List<Boolean> nullPages;
  
  /**
   * minValues and maxValues are lists containing lower and upper bounds for the values of each page determined by the ColumnOrder of the column. These may be the actual minimum and maximum values found on a page, but can also be (more compact) values that do not exist on a page. For example, instead of storing "Blart Versenwald III", a writer may set min_values[i]="B", max_values[i]="C". Such more compact values must still be valid values within the column's logical type. Readers must make sure that list entries are populated before using them by inspecting null_pages.
   */
  public final java.util.List<String> minValues;
  
  public final java.util.List<String> maxValues;
  
  /**
   * Stores whether both min_values and max_values are orderd and if so, in which direction. This allows readers to perform binary searches in both lists. Readers cannot assume that max_values[i] &lt;= min_values[i+1], even if the lists are ordered.
   */
  public final hydra.ext.parquet.format.BoundaryOrder boundaryOrder;
  
  /**
   * A list containing the number of null values for each page
   */
  public final hydra.util.Opt<java.util.List<Long>> nullCounts;
  
  public ColumnIndex (java.util.List<Boolean> nullPages, java.util.List<String> minValues, java.util.List<String> maxValues, hydra.ext.parquet.format.BoundaryOrder boundaryOrder, hydra.util.Opt<java.util.List<Long>> nullCounts) {
    java.util.Objects.requireNonNull((nullPages));
    java.util.Objects.requireNonNull((minValues));
    java.util.Objects.requireNonNull((maxValues));
    java.util.Objects.requireNonNull((boundaryOrder));
    java.util.Objects.requireNonNull((nullCounts));
    this.nullPages = nullPages;
    this.minValues = minValues;
    this.maxValues = maxValues;
    this.boundaryOrder = boundaryOrder;
    this.nullCounts = nullCounts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnIndex)) {
      return false;
    }
    ColumnIndex o = (ColumnIndex) (other);
    return nullPages.equals(o.nullPages) && minValues.equals(o.minValues) && maxValues.equals(o.maxValues) && boundaryOrder.equals(o.boundaryOrder) && nullCounts.equals(o.nullCounts);
  }
  
  @Override
  public int hashCode() {
    return 2 * nullPages.hashCode() + 3 * minValues.hashCode() + 5 * maxValues.hashCode() + 7 * boundaryOrder.hashCode() + 11 * nullCounts.hashCode();
  }
  
  public ColumnIndex withNullPages(java.util.List<Boolean> nullPages) {
    java.util.Objects.requireNonNull((nullPages));
    return new ColumnIndex(nullPages, minValues, maxValues, boundaryOrder, nullCounts);
  }
  
  public ColumnIndex withMinValues(java.util.List<String> minValues) {
    java.util.Objects.requireNonNull((minValues));
    return new ColumnIndex(nullPages, minValues, maxValues, boundaryOrder, nullCounts);
  }
  
  public ColumnIndex withMaxValues(java.util.List<String> maxValues) {
    java.util.Objects.requireNonNull((maxValues));
    return new ColumnIndex(nullPages, minValues, maxValues, boundaryOrder, nullCounts);
  }
  
  public ColumnIndex withBoundaryOrder(hydra.ext.parquet.format.BoundaryOrder boundaryOrder) {
    java.util.Objects.requireNonNull((boundaryOrder));
    return new ColumnIndex(nullPages, minValues, maxValues, boundaryOrder, nullCounts);
  }
  
  public ColumnIndex withNullCounts(hydra.util.Opt<java.util.List<Long>> nullCounts) {
    java.util.Objects.requireNonNull((nullCounts));
    return new ColumnIndex(nullPages, minValues, maxValues, boundaryOrder, nullCounts);
  }
}