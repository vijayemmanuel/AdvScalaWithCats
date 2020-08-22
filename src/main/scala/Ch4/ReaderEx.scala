package Ch4

import cats.data.Reader

object ReaderInterface {

  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[A] = Reader[Db,A]

  def findUsername(userId: Int): DbReader[Option[String]] = {
      Reader(db => db.usernames.get(userId))
    }

  def checkPassword(
                     username: String,
                     password: String
                   ): DbReader[Boolean] = {
    Reader(db => {
      val result = for {
        users <- db.usernames
      }yield (if ((users._2 == username) && (db.passwords(users._2) == password)) true else false)
      result.filter( x => x == true).headOption match {
        case Some(_) => true
        case None => false
      }
    })


  }

  def checkLogin(
                  userId: Int,
                  password: String
                ): DbReader[Boolean] = {
    import cats.syntax.applicative._ // for `pure`
    for {
      username <- findUsername(userId)
      passwordOk <- username.map {
        username => checkPassword(username, password) }.getOrElse {
        false.pure[DbReader] }
    } yield passwordOk
  }

}

object ReaderEx {

  import ReaderInterface._
  def main(args: Array[String]): Unit = {
    val db = Db(
      Map(
        1 -> "dade", 2 -> "kate", 3 -> "margo"
      ), Map(
        "dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret"
      ) )
    // db: Db = Db(Map(1 -> dade, 2 -> kate, 3 -> margo),Map(dade -> zerocool, kate -> acidburn, margo -> secret))
    println(checkLogin(1, "zerocool").run(db))// res8: cats.Id[Boolean] = true
    println(checkLogin(4, "davinci").run(db))// res9: cats.Id[Boolean] = false


  }

}
