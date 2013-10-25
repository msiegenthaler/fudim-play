package support.http

import java.util.Locale
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

/** Parses and serializes date times as specified by HTTP (cookies, expires header etc.). */
object HttpDate {
  def parse(from: String) = {
    alternateFormats.view.flatMap { format ⇒
      try { Some(format.parseDateTime(from).withZone(utc)) }
      catch { case e: IllegalArgumentException ⇒ None }
    }.headOption
  }

  def serialize(dateTime: DateTime) = format.print(dateTime)

  private val enUS = new Locale("en-us")
  private val format = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss zz").withLocale(enUS).withZoneUTC
  private val alternateFormats = format ::
    ("EEE, dd MMM yyyy HH:mm:ss ZZ" :: "EEE, dd MMM yyyy HH:mm:ss ZZZ" :: "EEE, dd MMM yyyy HH:mm:ss" ::
      "EEE, dd-MMM-yyyy HH:mm:ss zz" :: "EEE, dd-MMM-yyyy HH:mm:ss ZZ" :: "EEE, dd-MMM-yyyy HH:mm:ss ZZZ" :: "EEE, dd-MMM-yyyy HH:mm:ss" ::
      Nil).map(DateTimeFormat.forPattern _).map(_.withLocale(enUS)).map(_.withZoneUTC)
  private val utc = DateTimeZone.forID("UTC")
}