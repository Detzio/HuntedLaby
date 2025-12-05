package huntedlaby

final case class Coord(x: Int, y: Int)

enum Direction:
  case Up, Down, Left, Right

import Direction.*

enum Cell:
  case Wall, Floor, Exit

import Cell.*

type Grid = Vector[Vector[Cell]]

final case class Rng(seed: Long):
  def nextInt(bound: Int): (Int, Rng) =
    val newSeed = (seed * 6364136223846793005L + 1L) & 0xFFFFFFFFFFFFL
    val n       = (newSeed >>> 16).toInt
    val idx     = Math.floorMod(n, bound)
    (idx, Rng(newSeed))

  def nextCoord(width: Int, height: Int): (Coord, Rng) =
    val (ix, r1) = nextInt(width)
    val (iy, r2) = r1.nextInt(height)
    (Coord(ix, iy), r2)

enum PlayerInput:
  case MoveUp, MoveDown, MoveLeft, MoveRight, Quit

enum GameStatus:
  case Running, Won, Lost, Quit

import PlayerInput.*
import GameStatus.*

final case class Maze(width: Int, height: Int, cells: Grid):
  def cellAt(c: Coord): Option[Cell] =
    if c.y >= 0 && c.y < height && c.x >= 0 && c.x < width then
      Some(cells(c.y)(c.x))
    else None

  def isWalkable(c: Coord): Boolean =
    cellAt(c).exists {
      case Wall => false
      case _    => true
    }

  def neighbors4(c: Coord): List[Coord] =
    List(
      Coord(c.x, c.y - 1),
      Coord(c.x, c.y + 1),
      Coord(c.x - 1, c.y),
      Coord(c.x + 1, c.y)
    ).filter(isWalkable)

enum Difficulty(val huntersCount: Int):
  case Easy extends Difficulty(1)
  case Medium extends Difficulty(2)
  case Hard extends Difficulty(3)

final case class GameConfig(width: Int, height: Int, difficulty: Difficulty)

final case class GameState(
    maze: Maze,
    playerPos: Coord,
    hunterPositions: List[Coord],
    exitPos: Coord,
    rng: Rng,
    status: GameStatus,
    turn: Int
)
