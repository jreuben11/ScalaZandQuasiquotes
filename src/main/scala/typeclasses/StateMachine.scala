package typeclasses

/**
  * Created by joshr on 03/04/2016.
  */


import cats.data.State
import cats.std.function._

trait StateMachine {
  val nextLong: State[Seed, Long] = State(seed => (seed.next, seed.long))
  val nextBoolean: State[Seed, Boolean] = nextLong.map(long => long > 0)
  val createRobot: State[Seed, Robot] =
    for {
      id <- nextLong
      sentient <- nextBoolean
      isCatherine <- nextBoolean
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- nextBoolean
      model = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)

  val initialSeed = Seed(13L)
  val (finalState, robot) = createRobot.run(initialSeed).value
}

final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

final case class Robot(
                        id: Long,
                        sentient: Boolean,
                        name: String,
                        model: String)

