package streams

import scala.collection.immutable

import LazyList.*
/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef:

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = b.isStanding && b.isLegal && Pos(b.b1.row, b.b1.col) == goal

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a lazy list of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */

//  1 #:: 2 #:: LazyList.empty
  def neighborsWithHistory(b: Block, history: List[Move]): LazyList[(Block, List[Move])] =
    val closestNeighbours: List[(Block, List[Move])]  = for (block, move) <- b.legalNeighbors yield (block, move +: history)
    val lazyClosestNeighbours: LazyList[(Block, List[Move])] = closestNeighbours.to(LazyList)
    lazyClosestNeighbours
//   b.neighbors #::

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: LazyList[(Block, List[Move])],
                       explored: Set[Block]): LazyList[(Block, List[Move])] =
    neighbors.filter((block, listMoves) => !explored.contains(block))


  /**
   * The function `from` returns the lazy list of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * lazy list.
   *
   * The blocks in the lazy list `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the lazy list.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * lazy list `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting lazy list should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the lazy list.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted lazy list.
   */
  def from(initial: LazyList[(Block, List[Move])],
           explored: Set[Block]): LazyList[(Block, List[Move])] =

    lazy val newNeighbors: LazyList[(Block, List[Move])] = newNeighborsOnly(initial, explored)
    val newExplored: Set[Block] = explored ++ newNeighbors.map(_._1)

    from(initial ++ newNeighbors , newExplored)
//    initial match
//      case head #:: tail => newNeighborsOnly(initial, explored)

  /**
   * The lazy list of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: LazyList[(Block, List[Move])] = from(LazyList((Block(startPos, startPos), List.empty)), Set.empty)

  /**
   * Returns a lazy list of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: LazyList[(Block, List[Move])] = pathsFromStart.filter((block, movesList) => block == goal)

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    pathsToGoal.reduce{(el1, el2) => if (el1._2.size > el2._2.size) then el1 else el2}._2
  }
