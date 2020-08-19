package Ch1
import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

object ShowEx {



  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow = Show.show [Cat] { cat =>
    s"${cat.name.show} is a ${cat.age.show} year old ${cat.color} cat."
  }

  def main(args: Array[String]): Unit = {

    println(Cat("Garfield", 2, "ginger and black").show)
  }



}