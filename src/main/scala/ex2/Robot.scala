package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot) extends Robot:
  var batteryLevel: Int = 100
  val turnValue: Int = 5
  val actValue: Int = 10

  def decrementBattery(value: Int): Unit =
    batteryLevel = batteryLevel-value

  def checkBattery(value: Int): Boolean = this.batteryLevel > value

  export robot.{position, direction}

  override def turn(dir: Direction): Unit =
    if checkBattery(turnValue) then
      decrementBattery(turnValue)
      robot.turn(dir)
    else
      println("Batteria scarica")

  override def act(): Unit =
    if checkBattery(actValue) then
      decrementBattery(actValue)
      robot.act()
    else
      println("Batteria scarica")

  def logBattery(): Unit = println("Livello batteria: " + batteryLevel)

class RobotCanFail(val robot: Robot, failParamRaw: Int) extends Robot:

  export robot.{position, direction}

  val failParam: Double = failParamRaw.min(100).toDouble /100

  def failAction(): Boolean = Random.nextDouble() < failParam

  override def turn(dir: Direction): Unit =
    if !failAction() then robot.turn(dir) else println("Robot ha fallito")

  override def act(): Unit = if !failAction() then robot.act() else println("Robot ha fallito")

class RobotRepeated(val robot: Robot, nRepeat: Int) extends Robot:

  export robot.{position, direction, turn}

  override def act(): Unit = (1 to nRepeat).foreach(_=> robot.act())

@main def testRobot(): Unit =
  val r = SimpleRobot((0, 0), Direction.North)
  val robot = RobotCanFail(LoggingRobot(r), 10)
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
  robot.act()
  robot.act()
  robot.act()
  robot.act()
  robot.act()
  robot.act()
  robot.act()

