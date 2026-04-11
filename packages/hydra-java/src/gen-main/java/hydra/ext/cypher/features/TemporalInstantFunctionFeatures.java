// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Temporal instant functions
 */
public class TemporalInstantFunctionFeatures implements Serializable, Comparable<TemporalInstantFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.TemporalInstantFunctionFeatures");

  public static final hydra.core.Name DATE = new hydra.core.Name("date");

  public static final hydra.core.Name DATE_REALTIME = new hydra.core.Name("dateRealtime");

  public static final hydra.core.Name DATE_STATEMENT = new hydra.core.Name("dateStatement");

  public static final hydra.core.Name DATE_TRANSACTION = new hydra.core.Name("dateTransaction");

  public static final hydra.core.Name DATE_TRUNCATE = new hydra.core.Name("dateTruncate");

  public static final hydra.core.Name DATETIME = new hydra.core.Name("datetime");

  public static final hydra.core.Name DATETIME_FROMEPOCH = new hydra.core.Name("datetimeFromepoch");

  public static final hydra.core.Name DATETIME_FROMEPOCHMILLIS = new hydra.core.Name("datetimeFromepochmillis");

  public static final hydra.core.Name DATETIME_REALTIME = new hydra.core.Name("datetimeRealtime");

  public static final hydra.core.Name DATETIME_STATEMENT = new hydra.core.Name("datetimeStatement");

  public static final hydra.core.Name DATETIME_TRANSACTION = new hydra.core.Name("datetimeTransaction");

  public static final hydra.core.Name DATETIME_TRUNCATE = new hydra.core.Name("datetimeTruncate");

  public static final hydra.core.Name LOCALDATETIME = new hydra.core.Name("localdatetime");

  public static final hydra.core.Name LOCALDATETIME_REALTIME = new hydra.core.Name("localdatetimeRealtime");

  public static final hydra.core.Name LOCALDATETIME_STATEMENT = new hydra.core.Name("localdatetimeStatement");

  public static final hydra.core.Name LOCALDATETIME_TRANSACTION = new hydra.core.Name("localdatetimeTransaction");

  public static final hydra.core.Name LOCALDATETIME_TRUNCATE = new hydra.core.Name("localdatetimeTruncate");

  public static final hydra.core.Name LOCALTIME = new hydra.core.Name("localtime");

  public static final hydra.core.Name LOCALTIME_REALTIME = new hydra.core.Name("localtimeRealtime");

  public static final hydra.core.Name LOCALTIME_STATEMENT = new hydra.core.Name("localtimeStatement");

  public static final hydra.core.Name LOCALTIME_TRANSACTION = new hydra.core.Name("localtimeTransaction");

  public static final hydra.core.Name LOCALTIME_TRUNCATE = new hydra.core.Name("localtimeTruncate");

  public static final hydra.core.Name TIME = new hydra.core.Name("time");

  public static final hydra.core.Name TIME_REALTIME = new hydra.core.Name("timeRealtime");

  public static final hydra.core.Name TIME_STATEMENT = new hydra.core.Name("timeStatement");

  public static final hydra.core.Name TIME_TRANSACTION = new hydra.core.Name("timeTransaction");

  public static final hydra.core.Name TIME_TRUNCATE = new hydra.core.Name("timeTruncate");

  /**
   * The date() function. Creates a DATE instant.
   */
  public final Boolean date;

  /**
   * The date.realtime() function. Returns the current DATE instant using the realtime clock.
   */
  public final Boolean dateRealtime;

  /**
   * The date.statement() function. Returns the current DATE instant using the statement clock.
   */
  public final Boolean dateStatement;

  /**
   * The date.transaction() function. Returns the current DATE instant using the transaction clock.
   */
  public final Boolean dateTransaction;

  /**
   * The date.truncate() function. Truncates the given temporal value to a DATE instant using the specified unit.
   */
  public final Boolean dateTruncate;

  /**
   * The datetime() function. Creates a ZONED DATETIME instant.
   */
  public final Boolean datetime;

  /**
   * The datetime.fromepoch() function. Creates a ZONED DATETIME given the seconds and nanoseconds since the start of the epoch.
   */
  public final Boolean datetimeFromepoch;

  /**
   * The datetime.fromepochmillis() function. Creates a ZONED DATETIME given the milliseconds since the start of the epoch.
   */
  public final Boolean datetimeFromepochmillis;

  /**
   * The datetime.realtime() function. Returns the current ZONED DATETIME instant using the realtime clock.
   */
  public final Boolean datetimeRealtime;

  /**
   * The datetime.statement() function. Returns the current ZONED DATETIME instant using the statement clock.
   */
  public final Boolean datetimeStatement;

  /**
   * The datetime.transaction() function. Returns the current ZONED DATETIME instant using the transaction clock.
   */
  public final Boolean datetimeTransaction;

  /**
   * The datetime.truncate() function. Truncates the given temporal value to a ZONED DATETIME instant using the specified unit.
   */
  public final Boolean datetimeTruncate;

  /**
   * The localdatetime() function. Creates a LOCAL DATETIME instant.
   */
  public final Boolean localdatetime;

  /**
   * The localdatetime.realtime() function. Returns the current LOCAL DATETIME instant using the realtime clock.
   */
  public final Boolean localdatetimeRealtime;

  /**
   * The localdatetime.statement() function. Returns the current LOCAL DATETIME instant using the statement clock.
   */
  public final Boolean localdatetimeStatement;

  /**
   * The localdatetime.transaction() function. Returns the current LOCAL DATETIME instant using the transaction clock.
   */
  public final Boolean localdatetimeTransaction;

  /**
   * The localdatetime.truncate() function. Truncates the given temporal value to a LOCAL DATETIME instant using the specified unit.
   */
  public final Boolean localdatetimeTruncate;

  /**
   * The localtime() function. Creates a LOCAL TIME instant.
   */
  public final Boolean localtime;

  /**
   * The localtime.realtime() function. Returns the current LOCAL TIME instant using the realtime clock.
   */
  public final Boolean localtimeRealtime;

  /**
   * The localtime.statement() function. Returns the current LOCAL TIME instant using the statement clock.
   */
  public final Boolean localtimeStatement;

  /**
   * The localtime.transaction() function. Returns the current LOCAL TIME instant using the transaction clock.
   */
  public final Boolean localtimeTransaction;

  /**
   * The localtime.truncate() function. Truncates the given temporal value to a LOCAL TIME instant using the specified unit.
   */
  public final Boolean localtimeTruncate;

  /**
   * The time() function. Creates a ZONED TIME instant.
   */
  public final Boolean time;

  /**
   * The time.realtime() function. Returns the current ZONED TIME instant using the realtime clock.
   */
  public final Boolean timeRealtime;

  /**
   * The time.statement() function. Returns the current ZONED TIME instant using the statement clock.
   */
  public final Boolean timeStatement;

  /**
   * The time.transaction() function. Returns the current ZONED TIME instant using the transaction clock.
   */
  public final Boolean timeTransaction;

  /**
   * The time.truncate() function. Truncates the given temporal value to a ZONED TIME instant using the specified unit.
   */
  public final Boolean timeTruncate;

  public TemporalInstantFunctionFeatures (Boolean date, Boolean dateRealtime, Boolean dateStatement, Boolean dateTransaction, Boolean dateTruncate, Boolean datetime, Boolean datetimeFromepoch, Boolean datetimeFromepochmillis, Boolean datetimeRealtime, Boolean datetimeStatement, Boolean datetimeTransaction, Boolean datetimeTruncate, Boolean localdatetime, Boolean localdatetimeRealtime, Boolean localdatetimeStatement, Boolean localdatetimeTransaction, Boolean localdatetimeTruncate, Boolean localtime, Boolean localtimeRealtime, Boolean localtimeStatement, Boolean localtimeTransaction, Boolean localtimeTruncate, Boolean time, Boolean timeRealtime, Boolean timeStatement, Boolean timeTransaction, Boolean timeTruncate) {
    this.date = date;
    this.dateRealtime = dateRealtime;
    this.dateStatement = dateStatement;
    this.dateTransaction = dateTransaction;
    this.dateTruncate = dateTruncate;
    this.datetime = datetime;
    this.datetimeFromepoch = datetimeFromepoch;
    this.datetimeFromepochmillis = datetimeFromepochmillis;
    this.datetimeRealtime = datetimeRealtime;
    this.datetimeStatement = datetimeStatement;
    this.datetimeTransaction = datetimeTransaction;
    this.datetimeTruncate = datetimeTruncate;
    this.localdatetime = localdatetime;
    this.localdatetimeRealtime = localdatetimeRealtime;
    this.localdatetimeStatement = localdatetimeStatement;
    this.localdatetimeTransaction = localdatetimeTransaction;
    this.localdatetimeTruncate = localdatetimeTruncate;
    this.localtime = localtime;
    this.localtimeRealtime = localtimeRealtime;
    this.localtimeStatement = localtimeStatement;
    this.localtimeTransaction = localtimeTransaction;
    this.localtimeTruncate = localtimeTruncate;
    this.time = time;
    this.timeRealtime = timeRealtime;
    this.timeStatement = timeStatement;
    this.timeTransaction = timeTransaction;
    this.timeTruncate = timeTruncate;
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
      this.dateRealtime,
      o.dateRealtime) && java.util.Objects.equals(
      this.dateStatement,
      o.dateStatement) && java.util.Objects.equals(
      this.dateTransaction,
      o.dateTransaction) && java.util.Objects.equals(
      this.dateTruncate,
      o.dateTruncate) && java.util.Objects.equals(
      this.datetime,
      o.datetime) && java.util.Objects.equals(
      this.datetimeFromepoch,
      o.datetimeFromepoch) && java.util.Objects.equals(
      this.datetimeFromepochmillis,
      o.datetimeFromepochmillis) && java.util.Objects.equals(
      this.datetimeRealtime,
      o.datetimeRealtime) && java.util.Objects.equals(
      this.datetimeStatement,
      o.datetimeStatement) && java.util.Objects.equals(
      this.datetimeTransaction,
      o.datetimeTransaction) && java.util.Objects.equals(
      this.datetimeTruncate,
      o.datetimeTruncate) && java.util.Objects.equals(
      this.localdatetime,
      o.localdatetime) && java.util.Objects.equals(
      this.localdatetimeRealtime,
      o.localdatetimeRealtime) && java.util.Objects.equals(
      this.localdatetimeStatement,
      o.localdatetimeStatement) && java.util.Objects.equals(
      this.localdatetimeTransaction,
      o.localdatetimeTransaction) && java.util.Objects.equals(
      this.localdatetimeTruncate,
      o.localdatetimeTruncate) && java.util.Objects.equals(
      this.localtime,
      o.localtime) && java.util.Objects.equals(
      this.localtimeRealtime,
      o.localtimeRealtime) && java.util.Objects.equals(
      this.localtimeStatement,
      o.localtimeStatement) && java.util.Objects.equals(
      this.localtimeTransaction,
      o.localtimeTransaction) && java.util.Objects.equals(
      this.localtimeTruncate,
      o.localtimeTruncate) && java.util.Objects.equals(
      this.time,
      o.time) && java.util.Objects.equals(
      this.timeRealtime,
      o.timeRealtime) && java.util.Objects.equals(
      this.timeStatement,
      o.timeStatement) && java.util.Objects.equals(
      this.timeTransaction,
      o.timeTransaction) && java.util.Objects.equals(
      this.timeTruncate,
      o.timeTruncate);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(date) + 3 * java.util.Objects.hashCode(dateRealtime) + 5 * java.util.Objects.hashCode(dateStatement) + 7 * java.util.Objects.hashCode(dateTransaction) + 11 * java.util.Objects.hashCode(dateTruncate) + 13 * java.util.Objects.hashCode(datetime) + 17 * java.util.Objects.hashCode(datetimeFromepoch) + 19 * java.util.Objects.hashCode(datetimeFromepochmillis) + 23 * java.util.Objects.hashCode(datetimeRealtime) + 29 * java.util.Objects.hashCode(datetimeStatement) + 31 * java.util.Objects.hashCode(datetimeTransaction) + 37 * java.util.Objects.hashCode(datetimeTruncate) + 41 * java.util.Objects.hashCode(localdatetime) + 43 * java.util.Objects.hashCode(localdatetimeRealtime) + 47 * java.util.Objects.hashCode(localdatetimeStatement) + 53 * java.util.Objects.hashCode(localdatetimeTransaction) + 59 * java.util.Objects.hashCode(localdatetimeTruncate) + 61 * java.util.Objects.hashCode(localtime) + 67 * java.util.Objects.hashCode(localtimeRealtime) + 71 * java.util.Objects.hashCode(localtimeStatement);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemporalInstantFunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      date,
      other.date);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dateRealtime,
      other.dateRealtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dateStatement,
      other.dateStatement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dateTransaction,
      other.dateTransaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dateTruncate,
      other.dateTruncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetime,
      other.datetime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetimeFromepoch,
      other.datetimeFromepoch);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetimeFromepochmillis,
      other.datetimeFromepochmillis);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetimeRealtime,
      other.datetimeRealtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetimeStatement,
      other.datetimeStatement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetimeTransaction,
      other.datetimeTransaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datetimeTruncate,
      other.datetimeTruncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localdatetime,
      other.localdatetime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localdatetimeRealtime,
      other.localdatetimeRealtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localdatetimeStatement,
      other.localdatetimeStatement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localdatetimeTransaction,
      other.localdatetimeTransaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localdatetimeTruncate,
      other.localdatetimeTruncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localtime,
      other.localtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localtimeRealtime,
      other.localtimeRealtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localtimeStatement,
      other.localtimeStatement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localtimeTransaction,
      other.localtimeTransaction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      localtimeTruncate,
      other.localtimeTruncate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      time,
      other.time);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      timeRealtime,
      other.timeRealtime);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      timeStatement,
      other.timeStatement);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      timeTransaction,
      other.timeTransaction);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      timeTruncate,
      other.timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDate(Boolean date) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDateRealtime(Boolean dateRealtime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDateStatement(Boolean dateStatement) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDateTransaction(Boolean dateTransaction) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDateTruncate(Boolean dateTruncate) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetime(Boolean datetime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetimeFromepoch(Boolean datetimeFromepoch) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetimeFromepochmillis(Boolean datetimeFromepochmillis) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetimeRealtime(Boolean datetimeRealtime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetimeStatement(Boolean datetimeStatement) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetimeTransaction(Boolean datetimeTransaction) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withDatetimeTruncate(Boolean datetimeTruncate) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaldatetime(Boolean localdatetime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaldatetimeRealtime(Boolean localdatetimeRealtime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaldatetimeStatement(Boolean localdatetimeStatement) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaldatetimeTransaction(Boolean localdatetimeTransaction) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaldatetimeTruncate(Boolean localdatetimeTruncate) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaltime(Boolean localtime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaltimeRealtime(Boolean localtimeRealtime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaltimeStatement(Boolean localtimeStatement) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaltimeTransaction(Boolean localtimeTransaction) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withLocaltimeTruncate(Boolean localtimeTruncate) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withTime(Boolean time) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withTimeRealtime(Boolean timeRealtime) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withTimeStatement(Boolean timeStatement) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withTimeTransaction(Boolean timeTransaction) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }

  public TemporalInstantFunctionFeatures withTimeTruncate(Boolean timeTruncate) {
    return new TemporalInstantFunctionFeatures(date, dateRealtime, dateStatement, dateTransaction, dateTruncate, datetime, datetimeFromepoch, datetimeFromepochmillis, datetimeRealtime, datetimeStatement, datetimeTransaction, datetimeTruncate, localdatetime, localdatetimeRealtime, localdatetimeStatement, localdatetimeTransaction, localdatetimeTruncate, localtime, localtimeRealtime, localtimeStatement, localtimeTransaction, localtimeTruncate, time, timeRealtime, timeStatement, timeTransaction, timeTruncate);
  }
}
