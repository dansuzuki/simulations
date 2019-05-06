
import scala.util.Random

object Sim01 extends App {
    println("sim01")

    class Person {
      var _greediness: Double = _
      var _wealth: Int = _

      def greediness = _greediness
      def greediness_=(v: Double) { _greediness = v }

      def wealth = _wealth
      def wealth_=(v: Int) { _wealth = v }

      override def toString(): String = s"Person(${"%10.9g".format(_greediness)},${_wealth})"
    }

    object Person {
      def apply(greediness: Int, wealth: Int): Person = {
        val ret = new Person
        ret.greediness = greediness
        ret.wealth = wealth
        ret
      }
    }

    object Step {
      val rand = new Random
      var resource: Int = 100000

      def apply(person: Person) {
          // check if person want to acquire wealth
          val w = person.wealth
          val g = person.greediness.toDouble
          val chance = g / w
          val check = Math.abs(rand.nextDouble)
          val acquired: Int = Math.round(resource * (rand.nextFloat * g)).toInt
          if(chance > check && resource > 0) {
            resource = resource - acquired
            person.wealth = (w + acquired).toInt
          }

          val upkeep = Math.round(acquired * rand.nextFloat * rand.nextFloat).toInt
          if (w > upkeep) {
            person.wealth = w - upkeep
            resource = resource + upkeep
          }

          val newGreediness = {
            (acquired + g) / (person.wealth + g)
            /*
            val ng = Math.sin(g/w)
            if (g + ng <= 0.0) g
            else g + ng
            */
          }
          person.greediness = newGreediness

      }

      def increaseResource() {
        if (rand.nextDouble < 0.01) {
          resource = resource + 1
        }
      }
    }

    val persons = (0 to 10).map(n => Person(1,1))
    (0 to args(0).toInt).foreach(n => {
      persons.foreach(p => Step(p))
      Step.increaseResource()
    })
    persons.foreach(println)
}
