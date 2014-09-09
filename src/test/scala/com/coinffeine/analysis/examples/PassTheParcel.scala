package com.coinffeine.analysis.examples

import com.coinffeine.analysis._
import com.coinffeine.analysis.graphviz.GameTreeVisualization
import com.coinffeine.lineage.Lineaged

object PassTheParcel {

  sealed trait Action
  case object Pass extends Action
  case object Stop extends Action

  val CollabRewards = Payoffs.fill(Lineaged.variable('collabReward, 2))
  val StopReward: Payoff = Lineaged.variable('stopReward, 3)
  val NoReward: Payoff = Lineaged.value(0)

  case class State(round: Int, maxRounds: Int, stopped: Boolean) extends GameState[Action] {

    override val currentPlayer: Player = round % 2 match {
      case 0 => Sam
      case 1 => Bob
    }

    private def lastPlayer = currentPlayer.otherPlayer

    override def actions: Set[Action] =
      if (round < maxRounds && !stopped) Set(Pass, Stop)
      else Set.empty

    override def play(action: Action) = {
      require(!isFinal)
      action match {
        case Pass => copy(round = round + 1)
        case Stop => copy(round = round + 1, stopped = true)
      }
    }

    override def payoffs = CollabRewards.scaleBy(passes, passes) + Payoffs(
      sam = if (hasStopped(Sam)) StopReward else NoReward,
      bob = if (hasStopped(Bob)) StopReward else NoReward
    )

    private def passes: Int = if (stopped) round - 1 else round

    private def hasStopped(player: Player) = stopped && lastPlayer == player
  }

  case class CollaborativeStrategy(override val player: Player) extends PureStrategy[Action] {
    override def selectAction(state: GameState[Action]): Action = Pass
  }

  val maxRounds = 10
  val initialState = State(0, maxRounds, stopped = false)
  val happyPath = List.fill(maxRounds)(Pass)
  val game = Game[Action](initialState)

  def analyzeCompleteGame = game.completeTree()
  def analyzeGame(collaborativePlayers: Set[Player], randomPlayers: Set[Player]) = {
    val collabStrats = collaborativePlayers.map { player => CollaborativeStrategy(player).allowedActions }
    val randomStrats = randomPlayers.map { player => AllowedActions.any[Action](player) }
    game.plausibleTree(collabStrats ++ randomStrats)
  }

  def main(args: Array[String]): Unit = {
    val tree = analyzeGame(collaborativePlayers = Set(Bob), randomPlayers = Set(Sam))

    println("Game tree:")
    println(tree)
    println("Dominant strategies")
    println(tree.dominantStrategies)
    new GameTreeVisualization[Action, game.type](tree, happyPath, tree.dominantStrategies)
      .writeTo("pass-the-parcel.dot")
  }
}
