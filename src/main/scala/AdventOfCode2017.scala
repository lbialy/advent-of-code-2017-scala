object AdventOfCode2017 extends App {

  val puzzles: List[Puzzle] =
    Task1ASolution() :: Task1BSolution() ::
      Task2ASolution() :: Task2BSolution() ::
      Nil

  puzzles
    .map(s => (s.name, s.solution))
    .foreach { case (name, solution) =>
      println(s"[$name] - $solution")
    }

}
