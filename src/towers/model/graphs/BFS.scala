package towers.model.graphs

class BFS[A] {

  var Path_list: List[A] = List()

  def bfs(graph: Graph[A], startID: Int, endID: Int): List[A] = {

    var explored: Set[Int] = Set(startID)

    val toExplore: Queue[Int] = new Queue()
    toExplore.enqueue(startID)

    while (!toExplore.empty()) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- graph.adjacencyList(nodeToExplore)) {
        if(!explored.contains(node)) {
          toExplore.enqueue(node)
          explored = explored + node
          if(node == endID){
            bfs(graph, startID, nodeToExplore)
            Path_list = Path_list :+ graph.nodes(node)
          }
        }
      }
    }
    if(startID == endID){
      Path_list = Path_list :+ graph.nodes(startID)
    }
    Path_list
  }

}
