package models.db

import org.specs2.mutable.Specification
import support._
import models.playbinding.{Fudim, FudimResources}
import models.FudimVersion
import org.joda.time.{DateTime, Duration}

class DatabaseVersionRepoSpec extends Specification {
  trait repo extends withModel {
    def repo = new DatabaseVersionRepo with FudimResources
    def create() = execTx(repo.create())
  }

  "DatabaseVersionRepo" should {
    "be able to create a new version" in new repo {
      val v = create()
      v must not beNull
    }
    "create versions with ascending ids" in new repo {
      val vs = (1 to 100).map(_ => create()).toList
      def checkAsc(vs: List[FudimVersion]): Unit = vs match {
        case a :: b :: tail =>
          a.id must be < b.id
          a.at.isAfter(b.at) must_== false
          checkAsc(b :: tail)
        case _ => ()
      }
      checkAsc(vs)
    }
    "create ascending versions" in new repo {
      val vs = (1 to 100).map(_ => create()).toList
      def checkAsc(vs: List[FudimVersion]): Unit = vs match {
        case a :: b :: tail =>
          a must be < b
          checkAsc(b :: tail)
        case _ => ()
      }
      checkAsc(vs)
    }
    "create versions with timestamp very close to now" in new repo {
      val v = create()
      val d = new Duration(v.at, DateTime.now())
      Math.abs(d.getMillis) must be < 5000L
    }
  }
}
