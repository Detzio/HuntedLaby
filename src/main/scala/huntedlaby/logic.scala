package huntedlaby

import PlayerInput.*
import GameStatus.{Running, Won, Lost, Quit as GameQuit}

object Logic:

  def initialState(config: GameConfig, seed: Long): GameState =
    val rng0 = Rng(seed)
    val (maze, rng1) = MazeGenerator.generateMaze(config.width, config.height, rng0)
    val (player, hunters, exit, rng2) = MazeGenerator.placeEntities(maze, config.difficulty.huntersCount, rng1)
    GameState(
      maze = maze,
      playerPos = player,
      hunterPositions = hunters,
      exitPos = exit,
      rng = rng2,
      status = Running,
      turn = 0
    )

  private def moveCoord(c: Coord, input: PlayerInput): Coord = input match
    case MoveUp    => Coord(c.x, c.y - 1)
    case MoveDown  => Coord(c.x, c.y + 1)
    case MoveLeft  => Coord(c.x - 1, c.y)
    case MoveRight => Coord(c.x + 1, c.y)
    case PlayerInput.Quit      => c

  def applyPlayerInput(state: GameState, input: PlayerInput): GameState =
    state.status match
      case Won | Lost | GameQuit => state
      case Running =>
        input match
          case PlayerInput.Quit => state.copy(status = GameQuit)
          case _ =>
            val candidate = moveCoord(state.playerPos, input)
            val newPlayer =
              if state.maze.isWalkable(candidate) then candidate
              else state.playerPos
            val newStatus =
              if newPlayer == state.exitPos then Won
              else state.status
            state.copy(playerPos = newPlayer, status = newStatus)

  private def findPath(maze: Maze, from: Coord, to: Coord): Option[List[Coord]] =

    def bfs(queue: List[List[Coord]], visited: Set[Coord]): Option[List[Coord]] =
      queue match
        case Nil => None
        case path :: rest =>
          val current = path.head
          if current == to then Some(path.reverse)
          else
            val neighbors = maze.neighbors4(current).filterNot(visited.contains)
            val newVisited = visited ++ neighbors
            val newPaths   = neighbors.map(n => n :: path)
            bfs(rest ++ newPaths, newVisited)

    if from == to then Some(List(from))
    else bfs(List(List(from)), Set(from))

  private def hunterStep(maze: Maze, from: Coord, to: Coord): Coord =
    if from == to then from
    else
      findPath(maze, from, to) match
        case Some(path) if path.length >= 2 =>
          path(1)
        case _ =>
          val candidates = List(
            Coord(from.x + 1, from.y),
            Coord(from.x - 1, from.y),
            Coord(from.x, from.y + 1),
            Coord(from.x, from.y - 1)
          ).filter(maze.isWalkable)

          if candidates.isEmpty then from
          else
            candidates.minBy(c => math.abs(c.x - to.x) + math.abs(c.y - to.y))

  def updateHunters(state: GameState): GameState =
    state.status match
      case Won | Lost | GameQuit => state
      case Running =>
        val newHunterPositions =
          state.hunterPositions.map(pos => hunterStep(state.maze, pos, state.playerPos))

        val caught = newHunterPositions.exists(_ == state.playerPos)
        val newStatus = if caught then Lost else state.status

        state.copy(hunterPositions = newHunterPositions, status = newStatus)

  def step(state: GameState, input: PlayerInput): GameState =
    val afterPlayer  = applyPlayerInput(state, input)
    val afterHunters = updateHunters(afterPlayer)
    afterHunters.copy(turn = state.turn + 1)
