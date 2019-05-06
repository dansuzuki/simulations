class Land(rw: Int, rh: Int) {
  val replenishAmount = 5
  val replenishRate = 3


  var _ctr: Int = replenishRate
  var _raw: Int = rw
  var _rth: Int = rh


  def decompose(n: Int = 1): Land = {
    val amount = if (_rth >= n) n else _rth
    _rth = _rth - amount
    _raw = _raw + amount
    this
  }

  def harvest(desired: Int): Int = {
    val amount = if (_rth >= desired) desired else _rth
    _rth = _rth - amount
    amount
  }

  def raw: Int = _raw

  def receiveManure(n: Int = 1): Land = {
    _raw = _raw + n
    this
  }

  def replenish(n: Int): Land = {
    if (_raw > n) {
      _raw = _raw - n
      _rth = _rth + n
    }
    else {
      _rth = _rth + _raw
      _raw = 0
    }
    this
  }

  def rth: Int = _rth

  def step() {
    if (_rth > 0) {
      // decompose
      decompose()
    }
    if(_ctr == 0 && _raw > 0) {
      _ctr = replenishRate
      replenish(replenishAmount)
    }
    else {
      _ctr = _ctr - 1
    }
  }

  override def toString(): String = s"Land(${this.raw}, ${this.rth})"
}

class Harvester(e: Int = 10, h: Int = 10, l: Int = 0) {
  val costToReproduce = 101
  var _energy: Int = e
  var _health: Int = h
  var _location: Int = l

  def health: Int = _health

  def location: Int = _location
  def location_=(l: Int) { _location = l }

  def step(land: Land): Option[Harvester] = {
    var ret: Option[Harvester] = None

    if (_energy > costToReproduce) {
      // try to reproduce
      val transfer = _energy - costToReproduce
      land.receiveManure(costToReproduce)
      _energy = 0

      ret = Some(new Harvester(transfer, 10, _location))
    }
    else if (land.rth > 0) {
      _energy = _energy + land.harvest(1)
    }
    else if (_energy >= 2) {
      // try to move
      _energy = _energy - 2
      land.receiveManure(2)
      _location = _location + 1
    }
    else {
      // upkeep cost
      if(_energy > 0) {
        _energy = _energy - 1
        land.receiveManure(1)
      }
    }

    if (_energy <= 0) {
      _health = _health - 1
    }
    ret
  }

  override def toString(): String = s"Harvester(${_energy}, ${_health})"
}

object TestLand extends App {
  import collection.mutable.ArrayBuffer

  val lands: Array[Land]= (0 to 100).map(n => new Land(1, 100)).toArray
  val landSize = lands.size
  val harvesters = List(new Harvester())
  var maxPop: Int = 0
  val popChart: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()
  val theFuture = (0 to 1000000).foldLeft(harvesters)((a, c) => {
        // println(s"Step ${c}: ${land} ${a}")
        val ret = a.flatMap(h => {
          val land = lands(h.location)
          h.step(land) match {
            case None => List(h)
            case Some(spawned) => List(h, spawned)
          }
        })
        .filter(_.health > 0)
        .map(h => {
          if (h.location >= landSize) h.location = 0
          h
        })
        val sz = ret.size
        if (maxPop < sz) {
          maxPop = sz
        }
        popChart.append((c, sz))
        // println(sz)
        lands.foreach(_.step())
        // ret.foreach(println)
        // lands.foreach(println)
        // println("---")


        ret
    })

  import scalax.chart.api._
  val chart = XYLineChart(popChart)
  chart.show()
  // lands.foreach(println)
  // println(maxPop)
  // println(theFuture.size)
  // // theFuture.foreach(println)

}
