package towers.model.ai

import towers.model.graphs.{BFS, Graph}
import play.api.libs.json.{JsValue, Json}
import towers.model.GridLocation
import towers.model.physics.PhysicsVector

class AIPlayer(id: String) {

  def computeMovement(jsonGameState: String): PhysicsVector = {
    // Do not edit this method. It will not be called during grading
    var path = getPath(jsonGameState)
    path = smoothPath(jsonGameState, path)
    pathToDirection(jsonGameState, path)
  }

  def getPath(jsonGameState: String): List[GridLocation] = {
    // TODO
    var shortest_path: List[GridLocation] = List()

    val parsed: JsValue = Json.parse(jsonGameState)

    val location_base = (parsed \ "base").as[Map[String, Int]]
    val location_base_grid: GridLocation = new GridLocation(location_base("x"), location_base("y"))

    var x_location = 0
    var y_location = 0

    val player = (parsed \ "players").as[List[JsValue]]
    for (i <- player) {
      if (id == (i \ "id").as[String]) {
        x_location = (i \ "x").as[Double].toInt
        y_location = (i \ "y").as[Double].toInt
      }
    }

    val walls = (parsed \ "walls").as[List[Map[String, Int]]]
    var wall_list: List[List[Int]] = List()
    for (i <- walls){
      wall_list = wall_list :+ List(i("x"), i("y"))
    }

    val grid_size = (parsed \ "gridSize").as[Map[String, Int]]
    val graph: Graph[List[Int]] = new Graph()
    var startID = 0
    var endID = 0
    var index_node = 0
    for (y <- 0 until grid_size("y")) {
      for (x <- 0 until grid_size("x")) {
        graph.addNode(index_node, List(x, y))
        index_node += 1
      }
    }
    var index_edge = 0
    for (y <- 0 until grid_size("y")){
      for(x <- 0 until grid_size("x")) {
        // check current tile is wall or not
        if (!wall_list.contains(List(x, y))) {
          // check it is last column
          if (x < grid_size("x") - 1) {
            // wall is right of tile
            if (wall_list.contains(List(x + 1, y))) {
              // wall is not below tile
              if (!wall_list.contains(List(x, y + 1)) && y + 1 < grid_size("y")) {
                graph.addEdge(index_edge, index_edge + grid_size("x"))
              }
            }
            // wall is not right of tile
            if (!wall_list.contains(List(x + 1, y))) {
              graph.addEdge(index_edge, index_edge + 1)
              //wall is not below tile
              if(!wall_list.contains(List(x, y+1)) && y + 1 < grid_size("y")){
                graph.addEdge(index_edge, index_edge + grid_size("x"))
              }
            }
          }
          //this is last column
          if(x == grid_size("x") - 1){
            if(!wall_list.contains(List(x, y+1)) && y < grid_size("y") - 1){
              graph.addEdge(index_edge, index_edge + grid_size("x"))
            }
          }
        }
        index_edge += 1
      }
    }
    for (z <- 0 until index_node){
      if(graph.nodes(z) == List(x_location, y_location)){
        startID = z
      }
      if(graph.nodes(z) == List(location_base_grid.x, location_base_grid.y)){
        endID = z
      }
    }

    val BFS = new BFS[List[Int]]

    val Path_list: List[List[Int]] = BFS.bfs(graph, startID, endID)

    for(i <- Path_list){
      shortest_path = shortest_path :+ new GridLocation(i.head, i(1))
    }

    shortest_path
  }

  def pathToDirection(jsonGameState: String, path: List[GridLocation]): PhysicsVector = {
    // TODO
    new PhysicsVector(Math.random() - 0.5, Math.random() - 0.5)
  }


  def smoothPath(jsonGameState: String, path: List[GridLocation]): List[GridLocation] = {
    // TODO
    path
  }

}
