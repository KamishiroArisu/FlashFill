package flash.fill

object Main {
    def main(args: Array[String]): Unit = {
        val algorithm = Algorithm()
        val input = Set(
            (List("01/21/2007"), "01"),
            (List("22.12.2002"), "12"),
            (List("2008-23-03"), "03")
        )
        algorithm.learn(input)

        val questions = List(
            List("02/05/2021"),
            List("17.11.1996"),
            List("2077-01-04")
        )

        println("learn:")
        input.foreach { case (in, out) => println(in.mkString("") + "  ->  " + out) }

        println()
        println("answer:")
        questions.foreach(q => println(q.mkString("") + "  ->  " + algorithm.answer(q).get))
    }
}
