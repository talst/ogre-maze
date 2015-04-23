package ogre.maze

trait Solver extends MazeDef {

  def done(ogre: Ogre): Boolean = ogre.pos1 == goal || ogre.pos2 == goal || ogre.pos3 == goal || ogre.pos4 == goal

  def neighborsWithHistory(ogre: Ogre, history: List[Move]): Stream[(Ogre, List[Move])] =
    for (legalNeighbor <- ogre.legalNeighbors.toStream) yield (legalNeighbor._1, legalNeighbor._2 :: history)

  def newNeighborsOnly(neighbors: Stream[(Ogre, List[Move])], explored: Set[Ogre]): Stream[(Ogre, List[Move])] =
    for {
      neighbor <- neighbors
      if !explored.contains(neighbor._1)
    } yield neighbor

  def from(initial: Stream[(Ogre, List[Move])], explored: Set[Ogre]): Stream[(Ogre, List[Move])] = {
    if (initial.isEmpty) initial
    else {
      val nextInitial = for {
        (ogre, history) <- initial
        nwh = neighborsWithHistory(ogre, history)
        next <- newNeighborsOnly(nwh, explored + ogre)
      } yield next
      initial ++ from(nextInitial, explored ++ (nextInitial map (_._1)))
    }
  }

  lazy val pathsFromStart: Stream[(Ogre, List[Move])] = from(Stream((startOgre, List())), Set(startOgre))

  lazy val pathsToGoal: Stream[(Ogre, List[Move])] =
    for  {
      path <- pathsFromStart
      if done(path._1)
    } yield path

  lazy val solution: List[Move] =
    if (pathsToGoal.isEmpty) List()
    else pathsToGoal.head._2.reverse
}
