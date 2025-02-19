// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.geojson.model;

import java.io.Serializable;

/**
 * A position is an array of numbers.  There MUST be two or more elements.  The first two elements are longitude and latitude, or easting and northing, precisely in that order and using decimal numbers.  Altitude or elevation MAY be included as an optional third element.
 */
public class Position implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.geojson.model.Position");
  
  public static final hydra.core.Name FIELD_NAME_LATITUDE = new hydra.core.Name("latitude");
  
  public static final hydra.core.Name FIELD_NAME_LONGITUDE = new hydra.core.Name("longitude");
  
  public static final hydra.core.Name FIELD_NAME_ALTITUDE = new hydra.core.Name("altitude");
  
  public final Double latitude;
  
  public final Double longitude;
  
  public final hydra.util.Opt<Double> altitude;
  
  public Position (Double latitude, Double longitude, hydra.util.Opt<Double> altitude) {
    java.util.Objects.requireNonNull((latitude));
    java.util.Objects.requireNonNull((longitude));
    java.util.Objects.requireNonNull((altitude));
    this.latitude = latitude;
    this.longitude = longitude;
    this.altitude = altitude;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Position)) {
      return false;
    }
    Position o = (Position) (other);
    return latitude.equals(o.latitude) && longitude.equals(o.longitude) && altitude.equals(o.altitude);
  }
  
  @Override
  public int hashCode() {
    return 2 * latitude.hashCode() + 3 * longitude.hashCode() + 5 * altitude.hashCode();
  }
  
  public Position withLatitude(Double latitude) {
    java.util.Objects.requireNonNull((latitude));
    return new Position(latitude, longitude, altitude);
  }
  
  public Position withLongitude(Double longitude) {
    java.util.Objects.requireNonNull((longitude));
    return new Position(latitude, longitude, altitude);
  }
  
  public Position withAltitude(hydra.util.Opt<Double> altitude) {
    java.util.Objects.requireNonNull((altitude));
    return new Position(latitude, longitude, altitude);
  }
}