package huntedlaby

import Cell.*
import Direction.*

object MazeGenerator:

  private def emptyGrid(width: Int, height: Int): Grid =
    Vector.fill(height, width)(Wall)

  private def inBounds(width: Int, height: Int, c: Coord): Boolean =
    c.x >= 0 && c.x < width && c.y >= 0 && c.y < height

  private def carveCell(cells: Grid, c: Coord): Grid =
    cells.updated(c.y, cells(c.y).updated(c.x, Floor))

  private def carveBetween(cells: Grid, from: Coord, to: Coord): Grid =
    val mid = Coord((from.x + to.x) / 2, (from.y + to.y) / 2)
    carveCell(carveCell(cells, mid), to)

  private def shuffleDirections(rng: Rng): (List[Direction], Rng) =
    val dirs = List(Up, Down, Left, Right)
    def loop(lst: List[Direction], acc: List[Direction], r: Rng): (List[Direction], Rng) =
      lst match
        case Nil => (acc, r)
        case _   =>
          val (i, r2) = r.nextInt(lst.length)
          val (picked, rest) = (lst(i), lst.patch(i, Nil, 1))
          loop(rest, picked :: acc, r2)
    loop(dirs, Nil, rng)

  private def move2(c: Coord, d: Direction): Coord = d match
    case Up    => Coord(c.x, c.y - 2)
    case Down  => Coord(c.x, c.y + 2)
    case Left  => Coord(c.x - 2, c.y)
    case Right => Coord(c.x + 2, c.y)

  private def carveFrom(width: Int, height: Int, pos: Coord, cells: Grid, rng: Rng): (Grid, Rng) =
    val (dirs, r2) = shuffleDirections(rng)

    def step(rem: List[Direction], grid: Grid, r: Rng): (Grid, Rng) =
      rem match
        case Nil => (grid, r)
        case d :: tail =>
          val next = move2(pos, d)
          if inBounds(width, height, next) && grid(next.y)(next.x) == Wall then
            val grid1 = carveBetween(grid, pos, next)
            val (grid2, rNext) = carveFrom(width, height, next, grid1, r)
            step(tail, grid2, rNext)
          else
            step(tail, grid, r)

    step(dirs, cells, r2)

  private def breakRandomWalls(
      grid: Grid,
      rng: Rng,
      width: Int,
      height: Int,
      breakRatio: Double = 0.10
  ): (Grid, Rng) =
    val wallCoords =
      (0 until height - 2).toVector.flatMap { y0 =>
        val y = y0 + 1
        (0 until width - 2).toVector.map(x0 => Coord(x0 + 1, y))
      }.filter(c => grid(c.y)(c.x) == Wall)

    if wallCoords.isEmpty then (grid, rng)
    else
      val toBreakCount = math.max(1, (wallCoords.size.toDouble * breakRatio).toInt)

      def pickN(n: Int, r: Rng, remaining: Vector[Coord], acc: Set[Coord]): (Set[Coord], Rng) =
        if n == 0 || remaining.isEmpty then (acc, r)
        else
          val (idx, r2) = r.nextInt(remaining.size)
          val chosen    = remaining(idx)
          val rest      = remaining.patch(idx, Nil, 1)
          pickN(n - 1, r2, rest, acc + chosen)

      val (chosenWalls, r2) = pickN(toBreakCount, rng, wallCoords, Set.empty)

      val newGrid =
        grid.zipWithIndex.map { case (row, y) =>
          row.zipWithIndex.map { case (cell, x) =>
            val coord = Coord(x, y)
            if chosenWalls.contains(coord) then Floor else cell
          }.toVector
        }.toVector

      (newGrid, r2)

  def generateMaze(width: Int, height: Int, rng: Rng): (Maze, Rng) =
    val w = if width % 2 == 0 then width - 1 else width
    val h = if height % 2 == 0 then height - 1 else height
    val base = emptyGrid(w, h)
    val start = Coord(1, 1)
    val carved = carveCell(base, start)
    val (gridPerfect, r2) = carveFrom(w, h, start, carved, rng)

    val (gridWithCycles, r3) = breakRandomWalls(gridPerfect, r2, w, h)

    val exits =
      (1 until w - 1).toVector.flatMap { x =>
        Vector(0, h - 1).map(y => Coord(x, y))
      }.filter { c =>
        (c.y == 0 && gridWithCycles(1)(c.x) == Floor) ||
        (c.y == h - 1 && gridWithCycles(h - 2)(c.x) == Floor)
      }

    val (exitIdx, r4) = if exits.nonEmpty then r3.nextInt(exits.length) else (0, r3)
    val exitCoord = if exits.nonEmpty then exits(exitIdx) else Coord(w - 2, h - 1)

    val exitGrid =
      if inBounds(w, h, exitCoord) then
        gridWithCycles.updated(exitCoord.y, gridWithCycles(exitCoord.y).updated(exitCoord.x, Exit))
      else gridWithCycles

    (Maze(w, h, exitGrid), r4)

  def placeEntities(maze: Maze, huntersCount: Int, rng: Rng): (Coord, List[Coord], Coord, Rng) =
    val exits =
      (0 until maze.height).toVector.flatMap { y =>
        (0 until maze.width).toVector.map(x => Coord(x, y))
      }.filter(c => maze.cells(c.y)(c.x) == Exit)

    val exit = exits.headOption.getOrElse(Coord(1, 1))

    val floorCells =
      (1 until maze.height - 1).toVector.flatMap { y =>
        (1 until maze.width - 1).toVector.map(x => Coord(x, y))
      }.filter(c => maze.cells(c.y)(c.x) == Floor)

    val player =
      if floorCells.nonEmpty then
        floorCells.maxBy(c => math.abs(c.x - exit.x) + math.abs(c.y - exit.y))
      else exit

    val available = floorCells.filterNot(_ == player).toVector

    def pickHunters(n: Int, r: Rng, remaining: Vector[Coord], acc: List[Coord]): (List[Coord], Rng) =
      if n == 0 || remaining.isEmpty then (acc.reverse, r)
      else
        val (idx, r2) = r.nextInt(remaining.size)
        val chosen    = remaining(idx)
        val rest      = remaining.patch(idx, Nil, 1)
        pickHunters(n - 1, r2, rest, chosen :: acc)

    val count = math.max(0, math.min(huntersCount, available.size))
    val (hunters, r2) = pickHunters(count, rng, available, Nil)

    (player, hunters, exit, r2)

end MazeGenerator
