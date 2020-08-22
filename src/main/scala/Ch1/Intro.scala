package Ch1

final case class Cat (name: String, age: Int, color: String)
object Ch1 {

  // Typeclass Printable
  trait Printable[A] {
    def format(x: A): String
  }

  // Interface Object style
  /*object Printable {
    //def format[A](x: A)(implicit printable: Printable[A]): String = {
    //  printable.format(x)
    //}
    // Or use way to implement .. IMO better
    def format[A: Printable](x: A): String = {
      implicitly[Printable[A]].format(x)
    }

    def print[A: Printable](x: A): Unit = {
      println(format(x))
    }
   */


  //Interface Syntax style
  object PrintableSyntax {
    implicit class PrintOps[A] (value : A) {

      // Interface syntax does not work correctly with implicitly format
      def format(implicit p: Printable[A]): String = {
        p.format(value)
      }

      def print(implicit p: Printable[A]): Unit = {
        println(p.format(value))
      }

    }
  }


}

object PrintableInstances {
  import Ch1.Printable
  implicit object StringPrintable extends Printable[String] {
    def format(x: String) = x.toString
  }

  implicit object IntPrintable extends Printable[Int] {
    def format(x: Int): String = x.toString()
  }
  // Or use the below way to implement
  //implicit val IntPritable = new Printable[Int] {
  //  def format[Int](x: Int): String = x.toString()
  //}

  implicit val catPrintable = new Printable[Cat] {
    def format(x: Cat): String = {
      s"${x.name} is a ${x.age} year-old ${x.color} cat"
    }
  }
}





  object Main {
  def main(args: Array[String]): Unit = {

    import Ch1.Printable
    import PrintableInstances._
    println ("--------------------------------------")
    //import PrintableInstances.catPrintable
    //Printable.print(1)

    // Define a cat
    val cat = Cat("Pussy", 2, "White")
    // Usung normal toString
    println (cat)
    //Using typeclass
    //Printable.print(cat)

    import Ch1.PrintableSyntax._
    cat.print

    println ("--------------------------------------")
  }
}