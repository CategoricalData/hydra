// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Temporal instant functions
 */
public class TemporalInstantFunctionFeatures implements Serializable, Comparable<TemporalInstantFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.TemporalInstantFunctionFeatures");
  
  public static final hydra.core.Name DATE = new hydra.core.Name("date");
  
  public static final hydra.core.Name DATE_REALTIME = new hydra.core.Name("date.realtime");
  
  public static final hydra.core.Name DATE_STATEMENT = new hydra.core.Name("date.statement");
  
  public static final hydra.core.Name DATE_TRANSACTION = new hydra.core.Name("date.transaction");
  
  public static final hydra.core.Name DATE_TRUNCATE = new hydra.core.Name("date.truncate");
  
  public static final hydra.core.Name DATETIME = new hydra.core.Name("datetime");
  
  public static final hydra.core.Name DATETIME_FROMEPOCH = new hydra.core.Name("datetime.fromepoch");
  
  public static final hydra.core.Name DATETIME_FROMEPOCHMILLIS = new hydra.core.Name("datetime.fromepochmillis");
  
  public static final hydra.core.Name DATETIME_REALTIME = new hydra.core.Name("datetime.realtime");
  
  public static final hydra.core.Name DATETIME_STATEMENT = new hydra.core.Name("datetime.statement");
  
  public static final hydra.core.Name DATETIME_TRANSACTION = new hydra.core.Name("datetime.transaction");
  
  public static final hydra.core.Name DATETIME_TRUNCATE = new hydra.core.Name("datetime.truncate");
  
  public static final hydra.core.Name LOCALDATETIME = new hydra.core.Name("localdatetime");
  
  public static final hydra.core.Name LOCALDATETIME_REALTIME = new hydra.core.Name("localdatetime.realtime");
  
  public static final hydra.core.Name LOCALDATETIME_STATEMENT = new hydra.core.Name("localdatetime.statement");
  
  public static final hydra.core.Name LOCALDATETIME_TRANSACTION = new hydra.core.Name("localdatetime.transaction");
  
  public static final hydra.core.Name LOCALDATETIME_TRUNCATE = new hydra.core.Name("localdatetime.truncate");
  
  public static final hydra.core.Name LOCALTIME = new hydra.core.Name("localtime");
  
  public static final hydra.core.Name LOCALTIME_REALTIME = new hydra.core.Name("localtime.realtime");
  
  public static final hydra.core.Name LOCALTIME_STATEMENT = new hydra.core.Name("localtime.statement");
  
  public static final hydra.core.Name LOCALTIME_TRANSACTION = new hydra.core.Name("localtime.transaction");
  
  public static final hydra.core.Name LOCALTIME_TRUNCATE = new hydra.core.Name("localtime.truncate");
  
  public static final hydra.core.Name TIME = new hydra.core.Name("time");
  
  public static final hydra.core.Name TIME_REALTIME = new hydra.core.Name("time.realtime");
  
  public static final hydra.core.Name TIME_STATEMENT = new hydra.core.Name("time.statement");
  
  public static final hydra.core.Name TIME_TRANSACTION = new hydra.core.Name("time.transaction");
  
  public static final hydra.core.Name TIME_TRUNCATE = new hydra.core.Name("time.truncate");
  
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
    TemporalInstantFunctionFeatures o = (TemporalInstantFunctionFeatures) other;
    return java.util.Objects.equals(
      this.date,
      o.date) && java.util.Objects.equals(
      this.date_realtime,
      o.date_realtime) && java.util.Objects.equals(
      this.date_statement,
      o.date_statement) && java.util.Objects.equals(
      this.date_transaction,
      o.date_transaction) && java.util.Objects.equals(
      this.date_truncate,
      o.date_truncate) && java.util.Objects.equals(
      this.datetime,
      o.datetime) && java.util.Objects.equals(
      this.datetime_fromepoch,
      o.datetime_fromepoch) && java.util.Objects.equals(
      this.datetime_fromepochmillis,
      o.datetime_fromepochmillis) && java.util.Objects.equals(
      this.datetime_realtime,
      o.datetime_realtime) && java.util.Objects.equals(
      this.datetime_statement,
      o.datetime_statement) && java.util.Objects.equals(
      this.datetime_transaction,
      o.datetime_transaction) && java.util.Objects.equals(
      this.datetime_truncate,
      o.datetime_truncate) && java.util.Objects.equals(
      this.localdatetime,
      o.localdatetime) && java.util.Objects.equals(
      this.localdatetime_realtime,
      o.localdatetime_realtime) && java.util.Objects.equals(
      this.localdatetime_statement,
      o.localdatetime_statement) && java.util.Objects.equals(
      this.localdatetime_transaction,
      o.localdatetime_transaction) && java.util.Objects.equals(
      this.localdatetime_truncate,
      o.localdatetime_truncate) && java.util.Objects.equals(
      this.localtime,
      o.localtime) && java.util.Objects.equals(
      this.localtime_realtime,
      o.localtime_realtime) && java.util.Objects.equals(
      this.localtime_statement,
      o.localtime_statement) && java.util.Objects.equals(
      this.localtime_transaction,
      o.localtime_transaction) && java.util.Objects.equals(
      this.localtime_truncate,
      o.localtime_truncate) && java.util.Objects.equals(
      this.time,
      o.time) && java.util.Objects.equals(
      this.time_realtime,
      o.time_realtime) && java.util.Objects.equals(
      this.time_statement,
      o.time_statement) && java.util.Objects.equals(
      this.time_transaction,
      o.time_transaction) && java.util.Objects.equals(
      this.time_truncate,
      o.time_truncate);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(date) + 3 * java.util.Objects.hashCode(date_realtime) + 5 * java.util.Objects.hashCode(date_statement) + 7 * java.util.Objects.hashCode(date_transaction) + 11 * java.util.Objects.hashCode(date_truncate) + 13 * java.util.Objects.hashCode(datetime) + 17 * java.util.Objects.hashCode(datetime_fromepoch) + 19 * java.util.Objects.hashCode(datetime_fromepochmillis) + 23 * java.util.Objects.hashCode(datetime_realtime) + 29 * java.util.Objects.hashCode(datetime_statement) + 31 * java.util.Objects.hashCode(datetime_transaction) + 37 * java.util.Objects.hashCode(datetime_truncate) + 41 * java.util.Objects.hashCode(localdatetime) + 43 * java.util.Objects.hashCode(localdatetime_realtime) + 47 * java.util.Objects.hashCode(localdatetime_statement) + 53 * java.util.Objects.hashCode(localdatetime_transaction) + 59 * java.util.Objects.hashCode(localdatetime_truncate) + 61 * java.util.Objects.hashCode(localtime) + 67 * java.util.Objects.hashCode(localtime_realtime) + 71 * java.util.Objects.hashCode(localtime_statement);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemporalInstantFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) date).compareTo(other.date);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) date_realtime).compareTo(other.date_realtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) date_statement).compareTo(other.date_statement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) date_transaction).compareTo(other.date_transaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) date_truncate).compareTo(other.date_truncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime).compareTo(other.datetime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime_fromepoch).compareTo(other.datetime_fromepoch);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime_fromepochmillis).compareTo(other.datetime_fromepochmillis);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime_realtime).compareTo(other.datetime_realtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime_statement).compareTo(other.datetime_statement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime_transaction).compareTo(other.datetime_transaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datetime_truncate).compareTo(other.datetime_truncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localdatetime).compareTo(other.localdatetime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localdatetime_realtime).compareTo(other.localdatetime_realtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localdatetime_statement).compareTo(other.localdatetime_statement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localdatetime_transaction).compareTo(other.localdatetime_transaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localdatetime_truncate).compareTo(other.localdatetime_truncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localtime).compareTo(other.localtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localtime_realtime).compareTo(other.localtime_realtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localtime_statement).compareTo(other.localtime_statement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localtime_transaction).compareTo(other.localtime_transaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) localtime_truncate).compareTo(other.localtime_truncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) time).compareTo(other.time);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) time_realtime).compareTo(other.time_realtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) time_statement).compareTo(other.time_statement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) time_transaction).compareTo(other.time_transaction);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) time_truncate).compareTo(other.time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate(Boolean date) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_realtime(Boolean date_realtime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_statement(Boolean date_statement) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_transaction(Boolean date_transaction) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDate_truncate(Boolean date_truncate) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime(Boolean datetime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_fromepoch(Boolean datetime_fromepoch) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_fromepochmillis(Boolean datetime_fromepochmillis) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_realtime(Boolean datetime_realtime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_statement(Boolean datetime_statement) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_transaction(Boolean datetime_transaction) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withDatetime_truncate(Boolean datetime_truncate) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime(Boolean localdatetime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_realtime(Boolean localdatetime_realtime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_statement(Boolean localdatetime_statement) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_transaction(Boolean localdatetime_transaction) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaldatetime_truncate(Boolean localdatetime_truncate) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime(Boolean localtime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_realtime(Boolean localtime_realtime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_statement(Boolean localtime_statement) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_transaction(Boolean localtime_transaction) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withLocaltime_truncate(Boolean localtime_truncate) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime(Boolean time) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_realtime(Boolean time_realtime) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_statement(Boolean time_statement) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_transaction(Boolean time_transaction) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
  
  public TemporalInstantFunctionFeatures withTime_truncate(Boolean time_truncate) {
    return new TemporalInstantFunctionFeatures(date, date_realtime, date_statement, date_transaction, date_truncate, datetime, datetime_fromepoch, datetime_fromepochmillis, datetime_realtime, datetime_statement, datetime_transaction, datetime_truncate, localdatetime, localdatetime_realtime, localdatetime_statement, localdatetime_transaction, localdatetime_truncate, localtime, localtime_realtime, localtime_statement, localtime_transaction, localtime_truncate, time, time_realtime, time_statement, time_transaction, time_truncate);
  }
}
