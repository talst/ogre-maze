    package ogre.maze

    trait MazeDef {

      case class Position(row: Int, col:Int) {
        def dRow(d: Int) = copy(row = row + d)
        def dCol(d: Int) = copy(col = col + d)
      }

      val startPos1: Position
      val startPos2: Position
      val startPos3: Position
      val startPos4: Position
      val goal: Position

      type Terrain = Position => Boolean

      val terrain: Terrain

      sealed abstract class Move
      case object Left extends Move
      case object Right extends Move
      case object Up extends Move
      case object Down extends Move

      def startOgre: Ogre = Ogre(startPos1, startPos2, startPos3, startPos4)

      case class Ogre(pos1: Position, pos2: Position, pos3: Position, pos4: Position) {
        def dRow(d: Int) = copy(pos1.dRow(d), pos2.dRow(d), pos3.dRow(d), pos4.dRow(d))
        def dCol(d: Int) = copy(pos1.dCol(d), pos2.dCol(d), pos3.dCol(d), pos4.dCol(d))

        def left = dCol(-1)
        def right = dCol(1)
        def up = dRow(-1)
        def down = dRow(1)

        def neighbors: List[(Ogre, Move)] = List((left, Left), (right, Right), (up, Up), (down, Down))
        def legalNeighbors : List[(Ogre, Move)] = for {
          (ogre, move) <- neighbors
          if ogre.isLegal
        } yield (ogre, move)

        def isLegal: Boolean = terrain(pos1) && terrain(pos2) && terrain(pos3) && terrain(pos4)
      }
    }

