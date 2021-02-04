package flash.fill.test

import flash.fill.Algorithm
import org.scalatest.funsuite.AnyFunSuite

class ExampleTest extends AnyFunSuite {
    test("Mixed Date Parsing") {
        val algorithm = Algorithm()
        val input = Set(
            (List("01/21/2007"), "01"),
            (List("22.12.2002"), "12"),
            (List("2008-23-03"), "03")
        )
        algorithm.learn(input)
        assert(algorithm.answer(List("02/05/2021")).contains("02"))
        assert(algorithm.answer(List("17.11.1996")).contains("11"))
        assert(algorithm.answer(List("2077-01-04")).contains("04"))
    }

    test("Phone Numbers") {
        val algorithm = Algorithm()
        val input = Set(
            (List("323-708-7700"), "323-708-7700"),
            (List("(425)-706-7709"), "425-706-7709"),
            (List("(557)-337-8804"), "557-337-8804"),
            (List("510.220.5586"), "510-220-5586"),
            (List("235 7654"), "425-235-7654"),
            (List("745-8139"), "425-745-8139")
        )
        algorithm.learn(input)
        assert(algorithm.answer(List("123-456-7890")).contains("123-456-7890"))
        assert(algorithm.answer(List("123.456.7890")).contains("123-456-7890"))
        assert(algorithm.answer(List("(123)-456-7890")).contains("123-456-7890"))
        assert(algorithm.answer(List("456-7890")).contains("425-456-7890"))
        assert(algorithm.answer(List("456 7890")).contains("425-456-7890"))
    }

    test("Conditional Concatenation") {
        val algorithm = Algorithm()
        val input = Set(
            (List("Alex", "Asst."), "Alex(Asst.)"),
            (List("Jim", "Manager"), "Jim(Manager)"),
            (List("Ryan", ""), ""),
            (List("", "Asst."), "")
        )
        algorithm.learn(input)
        assert(algorithm.answer(List("Trump", "President")).contains("Trump(President)"))
        assert(algorithm.answer(List("Biden", "")).contains(""))
        assert(algorithm.answer(List("", "Prof.")).contains(""))
    }
}
