// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Temporal instant functions
 */
public class TemporalInstantFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.TemporalInstantFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DATE = new hydra.core.Name("date");
  
  public static final hydra.core.Name FIELD_NAME_DATE_REALTIME = new hydra.core.Name("date.realtime");
  
  public static final hydra.core.Name FIELD_NAME_DATE_STATEMENT = new hydra.core.Name("date.statement");
  
  public static final hydra.core.Name FIELD_NAME_DATE_TRANSACTION = new hydra.core.Name("date.transaction");
  
  public static final hydra.core.Name FIELD_NAME_DATE_TRUNCATE = new hydra.core.Name("date.truncate");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME = new hydra.core.Name("datetime");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME_FROMEPOCH = new hydra.core.Name("datetime.fromepoch");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME_FROMEPOCHMILLIS = new hydra.core.Name("datetime.fromepochmillis");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME_REALTIME = new hydra.core.Name("datetime.realtime");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME_STATEMENT = new hydra.core.Name("datetime.statement");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME_TRANSACTION = new hydra.core.Name("datetime.transaction");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME_TRUNCATE = new hydra.core.Name("datetime.truncate");
  
  public static final hydra.core.Name FIELD_NAME_LOCALDATETIME = new hydra.core.Name("localdatetime");
  
  public static final hydra.core.Name FIELD_NAME_LOCALDATETIME_REALTIME = new hydra.core.Name("localdatetime.realtime");
  
  public static final hydra.core.Name FIELD_NAME_LOCALDATETIME_STATEMENT = new hydra.core.Name("localdatetime.statement");
  
  public static final hydra.core.Name FIELD_NAME_LOCALDATETIME_TRANSACTION = new hydra.core.Name("localdatetime.transaction");
  
  public static final hydra.core.Name FIELD_NAME_LOCALDATETIME_TRUNCATE = new hydra.core.Name("localdatetime.truncate");
  
  public static final hydra.core.Name FIELD_NAME_LOCALTIME = new hydra.core.Name("localtime");
  
  public static final hydra.core.Name FIELD_NAME_LOCALTIME_REALTIME = new hydra.core.Name("localtime.realtime");
  
  public static final hydra.core.Name FIELD_NAME_LOCALTIME_STATEMENT = new hydra.core.Name("localtime.statement");
  
  public static final hydra.core.Name FIELD_NAME_LOCALTIME_TRANSACTION = new hydra.core.Name("localtime.transaction");
  
  public static final hydra.core.Name FIELD_NAME_LOCALTIME_TRUNCATE = new hydra.core.Name("localtime.truncate");
  
  public static final hydra.core.Name FIELD_NAME_TIME = new hydra.core.Name("time");
  
  public static final hydra.core.Name FIELD_NAME_TIME_REALTIME = new hydra.core.Name("time.realtime");
  
  public static final hydra.core.Name FIELD_NAME_TIME_STATEMENT = new hydra.core.Name("time.statement");
  
  public static final hydra.core.Name FIELD_NAME_TIME_TRANSACTION = new hydra.core.Name("time.transaction");
  
  public static final hydra.core.Name FIELD_NAME_TIME_TRUNCATE = new hydra.core.Name("time.truncate");
  
  /**
   * The date() function. Creates a DATE instant.
   */
  public final Boolean date;
  
  /**
   * The date.realtime() function. Returns the current DATE instant using the realtime clock.
   */
  public final Boolean date_realtime;
  
  /**
   * The date.statement() function. Returns the current DATE instant using the statement clock.
   */
  public final Boolean date_statement;
  
  /**
   * The date.transaction() function. Returns the current DATE instant using the transaction clock.
   */
  public final Boolean date_transaction;
  
  /**
   * The date.truncate() function. Truncates the given temporal value to a DATE instant using the specified unit.
   */
  public final Boolean date_truncate;
  
  /**
   * The datetime() function. Creates a ZONED DATETIME instant.
   */
  public final Boolean datetime;
  
  /**
   * The datetime.fromepoch() function. Creates a ZONED DATETIME given the seconds and nanoseconds since the start of the epoch.
   */
  public final Boolean datetime_fromepoch;
  
  /**
   * The datetime.fromepochmillis() function. Creates a ZONED DATETIME given the milliseconds since the start of the epoch.
   */
  public final Boolean datetime_fromepochmillis;
  
  /**
   * The datetime.realtime() function. Returns the current ZONED DATETIME instant using the realtime clock.
   */
  public final Boolean datetime_realtime;
  
  /**
   * The datetime.statement() function. Returns the current ZONED DATETIME instant using the statement clock.
   */
  public final Boolean datetime_statement;
  
  /**
   * The datetime.transaction() function. Returns the current ZONED DATETIME instant using the transaction clock.
   */
  public final Boolean datetime_transaction;
  
  /**
   * The datetime.truncate() function. Truncates the given temporal value to a ZONED DATETIME instant using the specified unit.
   */
  public final Boolean datetime_truncate;
  
  /**
   * The localdatetime() function. Creates a LOCAL DATETIME instant.
   */
  public final Boolean localdatetime;
  
  /**
   * The localdatetime.realtime() function. Returns the current LOCAL DATETIME instant using the realtime clock.
   */
  public final Boolean localdatetime_realtime;
  
  /**
   * The localdatetime.statement() function. Returns the current LOCAL DATETIME instant using the statement clock.
   */
  public final Boolean localdatetime_statement;
  
  /**
   * The localdatetime.transaction() function. Returns the current LOCAL DATETIME instant using the transaction clock.
   */
  public final Boolean localdatetime_transaction;
  
  /**
   * The localdatetime.truncate() function. Truncates the given temporal value to a LOCAL DATETIME instant using the specified unit.
   */
  public final Boolean localdatetime_truncate;
  
  /**
   * The localtime() function. Creates a LOCAL TIME instant.
   */
  public final Boolean localtime;
  
  /**
   * The localtime.realtime() function. Returns the current LOCAL TIME instant using the realtime clock.
   */
  public final Boolean localtime_realtime;
  
  /**
   * The localtime.statement() function. Returns the current LOCAL TIME instant using the statement clock.
   */
  public final Boolean localtime_statement;
  
  /**
   * The localtime.transaction() function. Returns the current LOCAL TIME instant using the transaction clock.
   */
  public final Boolean localtime_transaction;
  
  /**
   * The localtime.truncate() function. Truncates the given temporal value to a LOCAL TIME instant using the specified unit.
   */
  public final Boolean localtime_truncate;
  
  /**
   * The time() function. Creates a ZONED TIME instant.
   */
  public final Boolean time;
  
  /**
   * The time.realtime() function. Returns the current ZONED TIME instant using the realtime clock.
   */
  public final Boolean time_realtime;
  
  /**
   * The time.statement() function. Returns the current ZONED TIME instant using the statement clock.
   */
  public final Boolean time_statement;
  
  /**
   * The time.transaction() function. Returns the current ZONED TIME instant using the transaction clock.
   */
  public final Boolean time_transaction;
  
  /**
   * The time.truncate() function. Truncates the given temporal value to a ZONED TIME instant using the specified unit.
   */
  public final Boolean time_truncate;
  
  public TemporalInstantFunctionFeatures (Boolean date, Boolean date_realtime, Boolean date_statement, Boolean date_transaction, Boolean date_truncate, Boolean datetime, Boolean datetime_fromepoch, Boolean datetime_fromepochmillis, Boolean datetime_realtime, Boolean datetime_statement, Boolean datetime_transaction, Boolean datetime_truncate, Boolean localdatetime, Boolean localdatetime_realtime, Boolean localdatetime_statement, Boolean localdatetime_transaction, Boolean localdatetime_truncate, Boolean localtime, Boolean localtime_realtime, Boolean localtime_statement, Boolean localtime_transaction, Boolean localtime_truncate, Boolean time, Boolean time_realtime, Boolean time_statement, Boolean time_transaction, Boolean time_truncate) {
    java.util.Objects.requireNonNull((date));
    java.util.Objects.requireNonNull((date_realtime));
    java.util.Objects.requireNonNull((date_statement));
    java.util.Objects.requireNonNull((date_transaction));
    java.util.Objects.requireNonNull((date_truncate));
    java.util.Objects.requireNonNull((datetime));
    java.util.Objects.requireNonNull((datetime_fromepoch));
    java.util.Objects.requireNonNull((datetime_fromepochmillis));
    java.util.Objects.requireNonNull((datetime_realtime));
    java.util.Objects.requireNonNull((datetime_statement));
    java.util.Objects.requireNonNull((datetime_transaction));
    java.util.Objects.requireNonNull((datetime_truncate));
    java.util.Objects.requireNonNull((localdatetime));
    java.util.Objects.requireNonNull((localdatetime_realtime));
    java.util.Objects.requireNonNull((localdatetime_statement));
    java.util.Objects.requireNonNull((localdatetime_transaction));
    java.util.Objects.requireNonNull((localdatetime_truncate));
    java.util.Objects.requireNonNull((localtime));
    java.util.Objects.requireNonNull((localtime_realtime));
    java.util.Objects.requireNonNull((localtime_statement));
    java.util.Objects.requireNonNull((localtime_transaction));
    java.util.Objects.requireNonNull((localtime_truncate));
    java.util.Objects.requireNonNull((time));
    java.util.Objects.requireNonNull((time_realtime));
    java.util.Objects.requireNonNull((time_statement));
    java.util.Objects.requireNonNull((time_transaction));
    java.util.Objects.requireNonNull((time_truncate));
    this.date = date;
    this.date_realtime = date_realtime;
    this.date_statement = date_statement;
    this.date_transaction = date_transaction;
    this.date_truncate = date_truncate;
    this.datetime = datetime;
    this.datetime_fromepoch = datetime_fromepoch;
    this.datetime_fromepochmillis = datetime_fromepochmillis;
    this.datetime_realtime = datetime_realtime;
    this.datetime_statement = datetime_statement;
    this.datetime_transaction = datetime_transaction;
    this.datetime_truncate = datetime_truncate;
    this.localdatetime = localdatetime;
    this.localdatetime_realtime = localdatetime_realtime;
    this.localdatetime_statement = localdatetime_statement;
    this.localdatetime_transaction = localdatetime_transaction;
    this.localdatetime_truncate = localdatetime_truncate;
    this.localtime = localtime;
    this.localtime_realtime = localtime_realtime;
    this.localtime_statement = localtime_statement;
    this.localtime_transaction = localtime_transaction;
    this.localtime_truncate = localtime_truncate;
    this.time = time;
    this.time_realtime = time_realtime;
    this.time_statement = time_statement;
    this.time_transaction = time_transaction;
    this.time_truncate = time_truncate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TemporalInstantFunctionFeatures)) {
      return false;
    }
    TemporalInstantFunctionFeatures o = (TemporalInstantFunctionFeatures) (other);
    return date.equals(o.date) && date_realtime.equals(o.date_realtime) && date_statement.equals(o.date_statement) && date_transaction.equals(o.date_transaction) && date_truncate.equals(o.date_truncate) && datetime.equals(o.datetime) && datetime_fromepoch.equals(o.datetime_fromepoch) && datetime_fromepochmillis.equals(o.datetime_fromepochmillis) && datetime_realtime.equals(o.datetime_realtime) && datetime_statement.equals(o.datetime_statement) && datetime_transaction.equals(o.datetime_transaction) && datetime_truncate.equals(o.datetime_truncate) && localdatetime.equals(o.localdatetime) && localdatetime_realtime.equals(o.localdatetime_realtime) && localdatetime_statement.equals(o.localdatetime_statement) && localdatetime_transaction.equals(o.localdatetime_transaction) && localdatetime_truncate.equals(o.localdatetime_truncate) && localtime.equals(o.localtime) && localtime_realtime.equals(o.localtime_realtime) && localtime_statement.equals(o.localtime_statement) && localtime_transaction.equals(o.localtime_transaction) && localtime_truncate.equals(o.localtime_truncate) && time.equals(o.time) && time_realtime.equals(o.time_realtime) && time_statement.equals(o.time_statement) && time_transaction.equals(o.time_transaction) && time_truncate.equals(o.time_truncate);
  }
  
  @Override
  public int hashCode() {
    return 2 * date.hashCode() + 3 * date_realtime.hashCode() + 5 * date_statement.hashCode() + 7 * date_transaction.hashCode() + 11 * date_truncate.hashCode() + 13 * datetime.hashCode() + 17 * datetime_fromepoch.hashCode() + 19 * datetime_fromepochmillis.hashCode() + 23 * datetime_realtime.hashCode() + 29 * datetime_statement.hashCode() + 31 * datetime_transaction.hashCode() + 37 * datetime_truncate.hashCode() + 41 * localdatetime.hashCode() + 43 * localdatetime_realtime.hashCode() + 47 * localdatetime_statement.hashCode() + 53 * localdatetime_transaction.hashCode() + 59 * localdatetime_truncate.hashCode() + 61 * localtime.hashCode() + 67 * localtime_realtime.hashCode() + 71 * localtime_statement.hashCode() + 2 * localtime_transaction.hashCode() + 3 * localtime_truncate.hashCode() + 5 * time.hashCode() + 7 * time_realtime.hashCode() + 11 * time_statement.hashCode() + 13 * time_transaction.hashCode() + 17 * time_truncate.hashCode();
  }
  
  public TemporalInstantFunctionFeatures withDate(Boolean date) {
    java.util.Objects.requireNonNull((date));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_realtime(Boolean date_realtime) {
    java.util.Objects.requireNonNull((date_realtime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_statement(Boolean date_statement) {
    java.util.Objects.requireNonNull((date_statement));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_transaction(Boolean date_transaction) {
    java.util.Objects.requireNonNull((date_transaction));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_truncate(Boolean date_truncate) {
    java.util.Objects.requireNonNull((date_truncate));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime(Boolean datetime) {
    java.util.Objects.requireNonNull((datetime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_fromepoch(Boolean datetime_fromepoch) {
    java.util.Objects.requireNonNull((datetime_fromepoch));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_fromepochmillis(Boolean datetime_fromepochmillis) {
    java.util.Objects.requireNonNull((datetime_fromepochmillis));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_realtime(Boolean datetime_realtime) {
    java.util.Objects.requireNonNull((datetime_realtime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_statement(Boolean datetime_statement) {
    java.util.Objects.requireNonNull((datetime_statement));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_transaction(Boolean datetime_transaction) {
    java.util.Objects.requireNonNull((datetime_transaction));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_truncate(Boolean datetime_truncate) {
    java.util.Objects.requireNonNull((datetime_truncate));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime(Boolean localdatetime) {
    java.util.Objects.requireNonNull((localdatetime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_realtime(Boolean localdatetime_realtime) {
    java.util.Objects.requireNonNull((localdatetime_realtime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_statement(Boolean localdatetime_statement) {
    java.util.Objects.requireNonNull((localdatetime_statement));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_transaction(Boolean localdatetime_transaction) {
    java.util.Objects.requireNonNull((localdatetime_transaction));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_truncate(Boolean localdatetime_truncate) {
    java.util.Objects.requireNonNull((localdatetime_truncate));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime(Boolean localtime) {
    java.util.Objects.requireNonNull((localtime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_realtime(Boolean localtime_realtime) {
    java.util.Objects.requireNonNull((localtime_realtime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_statement(Boolean localtime_statement) {
    java.util.Objects.requireNonNull((localtime_statement));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_transaction(Boolean localtime_transaction) {
    java.util.Objects.requireNonNull((localtime_transaction));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_truncate(Boolean localtime_truncate) {
    java.util.Objects.requireNonNull((localtime_truncate));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime(Boolean time) {
    java.util.Objects.requireNonNull((time));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_realtime(Boolean time_realtime) {
    java.util.Objects.requireNonNull((time_realtime));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_statement(Boolean time_statement) {
    java.util.Objects.requireNonNull((time_statement));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_transaction(Boolean time_transaction) {
    java.util.Objects.requireNonNull((time_transaction));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_truncate(Boolean time_truncate) {
    java.util.Objects.requireNonNull((time_truncate));
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
}