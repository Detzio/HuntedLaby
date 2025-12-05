package huntedlaby

import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.{BorderPane, GridPane, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, Text}

import PlayerInput._
import GameStatus.{Running, Won, Lost, Quit => GameQuit}
import Logic._

object Main extends JFXApp3 {

  private val cellSize = 32.0

  override def start(): Unit = {
    val baseConfig = GameConfig(width = 41, height = 25, difficulty = Difficulty.Easy)

    def newInitialState(diff: Difficulty, seed: Long = System.currentTimeMillis()): GameState =
      initialState(baseConfig.copy(difficulty = diff), seed)

    val gridPane = new GridPane()
    gridPane.hgap = 0
    gridPane.vgap = 0

    val message = new Text()
    message.font = Font(16)
    message.fill = Color.White

    val bigMessage = new Text()
    bigMessage.visible = false
    bigMessage.fill = Color.White
    bigMessage.font = Font(56)

    val centerPane = new StackPane()
    centerPane.children.addAll(gridPane, bigMessage)

    val rootPane = new BorderPane()
    rootPane.center = centerPane
    rootPane.bottom = message
    BorderPane.setMargin(message, Insets(8))

    def render(state: GameState, difficulty: Difficulty): Unit = {
      gridPane.children.clear()

      val coords =
        (0 until state.maze.height).flatMap { y =>
          (0 until state.maze.width).map(x => (x, y))
        }

      coords.foldLeft(()) { case (_, (x, y)) =>
        val cell = state.maze.cells(y)(x)
        val rect = new Rectangle
        rect.width  = cellSize
        rect.height = cellSize
        rect.fill = cell match {
          case Cell.Wall  => Color.DarkGray
          case Cell.Floor => Color.Black
          case Cell.Exit  => Color.Green
        }
        gridPane.add(rect, x, y)
        ()
      }

      val player = new Text("P")
      player.fill = Color.Yellow
      player.font = Font(22)
      gridPane.add(player, state.playerPos.x, state.playerPos.y)

      state.hunterPositions.foldLeft(()) { case (_, hPos) =>
        val hunter = new Text("H")
        hunter.fill = Color.Red
        hunter.font = Font(22)
        gridPane.add(hunter, hPos.x, hPos.y)
        ()
      }

      val diffLabel = difficulty match
        case Difficulty.Easy   => "Facile (1 chasseur)"
        case Difficulty.Medium => "Moyen (2 chasseurs)"
        case Difficulty.Hard   => "Difficile (3 chasseurs)"

      message.text = state.status match {
        case Running  => s"${diffLabel} - Tour: ${state.turn} - ZQSD pour bouger, 1/2/3 pour changer de difficulté, X pour quitter, R pour rejouer."
        case Won      => s"${diffLabel} - Gagné en ${state.turn} tours ! Appuie sur R pour rejouer, ou 1/2/3 pour changer de difficulté."
        case Lost     => s"${diffLabel} - Perdu en ${state.turn} tours... Appuie sur R pour rejouer, ou 1/2/3 pour changer de difficulté."
        case GameQuit => s"${diffLabel} - Partie quittée après ${state.turn} tours. Appuie sur R pour rejouer, ou 1/2/3 pour changer de difficulté."
      }

      state.status match {
        case Won =>
          bigMessage.text = "GAGNÉ !"
          bigMessage.fill = Color.LimeGreen
          bigMessage.visible = true
        case Lost =>
          bigMessage.text = "PERDU !"
          bigMessage.fill = Color.Red
          bigMessage.visible = true
        case _ =>
          bigMessage.visible = false
      }
    }

    stage = new JFXApp3.PrimaryStage {
      title = "HuntedLaby"
      scene = new Scene {
        fill = Color.Black
        root = rootPane
      }
    }

    def attachHandlers(current: GameState, difficulty: Difficulty): Unit = {
      render(current, difficulty)

      stage.scene().onKeyPressed = (e: KeyEvent) => {
        val inputName = Option(e.code).map(_.getName.toLowerCase)

        val nextDifficulty = inputName match {
          case Some("1") => Some(Difficulty.Easy)
          case Some("2") => Some(Difficulty.Medium)
          case Some("3") => Some(Difficulty.Hard)
          case _          => None
        }

        nextDifficulty match {
          case Some(diff) =>
            val newState = newInitialState(diff)
            attachHandlers(newState, diff)
          case None =>
            current.status match {
              case Running =>
                val inputOpt: Option[PlayerInput] =
                  inputName.flatMap {
                    case "z" => Some(MoveUp)
                    case "s" => Some(MoveDown)
                    case "q" => Some(MoveLeft)
                    case "d" => Some(MoveRight)
                    case "x" => Some(PlayerInput.Quit)
                    case _    => None
                  }

                inputOpt match {
                  case Some(in) =>
                    val next = step(current, in)
                    attachHandlers(next, difficulty)
                  case None => ()
                }

              case Won | Lost | GameQuit =>
                inputName match {
                  case Some("r") =>
                    val newState = newInitialState(difficulty)
                    attachHandlers(newState, difficulty)
                  case _ => ()
                }
            }
        }
      }
    }

    val initialDifficulty = Difficulty.Easy
    attachHandlers(newInitialState(initialDifficulty), initialDifficulty)
  }
}
