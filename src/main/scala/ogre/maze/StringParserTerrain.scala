package ogre.maze

import scala.util.Try

trait StringParserTerrain extends MazeDef {

  val level: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Position => Boolean =
    (position: Position) => Try(levelVector(position.row)(position.col) != 'O').getOrElse(false)

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Position = {
    (for {
      row <- levelVector.toStream
      col <- row.toStream
      if col == c
    } yield Position(levelVector.indexOf(row), row.indexOf(col))).head
  }

  lazy val vector: Vector[Vector[Char]] = Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos1: Position = findChar('@', vector)
  lazy val startPos2: Position = startPos1.dCol(1)
  lazy val startPos3: Position = startPos1.dRow(1)
  lazy val startPos4: Position = startPos1.dRow(1).dCol(1)
  lazy val goal: Position = findChar('$', vector)


  def pathToString(moves: List[Move], startOgre: Ogre, vector: Vector[Vector[Char]]): String = {
    def updatePos(position: Position, vector: Vector[Vector[Char]]) = vector.updated(position.row, vector(position.row).updated(position.col, '&'))

    def updatedVector(ogre: Ogre, vector: Vector[Vector[Char]]): Vector[Vector[Char]] = {
      val pos1Updated = updatePos(ogre.pos1, vector)
      val pos2Updated = updatePos(ogre.pos2, pos1Updated)
      val pos3Updated = updatePos(ogre.pos3, pos2Updated)
      updatePos(ogre.pos4, pos3Updated)
    }
    def innerRec(moves: List[Move], ogre: Ogre, acc: Vector[Vector[Char]]): Vector[Vector[Char]] = moves match {
      case List() => acc
      case head :: tail => head match {
        case Left => innerRec(tail, ogre.left, updatedVector(ogre.left, acc))
        case Right => innerRec(tail, ogre.right, updatedVector(ogre.right, acc))
        case Up => innerRec(tail, ogre.up, updatedVector(ogre.up, acc))
        case Down => innerRec(tail, ogre.down, updatedVector(ogre.down, acc))
      }
    }
    innerRec(moves, startOgre, updatedVector(startOgre, vector)).map(_.mkString("")).mkString("\n")
  }

  def solutionString(solution: List[Move], startOgre: Ogre, vector: Vector[Vector[Char]]): String = solution match {
    case List() => "No path"
    case head :: tail => pathToString(solution, startOgre, vector)
  }
}
