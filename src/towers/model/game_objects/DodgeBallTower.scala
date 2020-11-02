package towers.model.game_objects

import play.api.libs.json.{JsValue, Json}
import towers.model.genetics.genes.Gene
import towers.model.physics.PhysicsVector

class DodgeBallTower(val x: Int, val y: Int) extends GameObject {

  // The height at which projectiles are fired
  val height = 3.0

  // Towers can only fire at players closer than this distance from the tower
  val sightRange = 5.0

  // The magnitude of the velocity at which projectiles are fired
  val projectileVelocity = 5.0


  def fire(jsonGameState: String): List[Projectile] = {
    // TODO: Objective 2
    List()
  }


  def aimFire(jsonGameState: String): List[Projectile] = {
    // TODO: Bonus Objective
    List()
  }


  // Suggested Genetic Algorithm setup
  def getFitnessFunction(targetPlayer: Player): PhysicsVector => Double = {
    null
  }

  def vectorIncubator(genes: List[Gene]): PhysicsVector = {
    null
  }

}
