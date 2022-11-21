package debate

import munit.CatsEffectSuite

class RoleTests extends CatsEffectSuite {

  val roles = List(
    Observer, Facilitator, Judge
  ) ++ (0 to 3).map(Debater(_)).toList

  test("KeyEncoder / KeyDecoder work round-trip") {
    roles.foreach { role =>
      assertEquals(
        Role.roleKeyDecoder(Role.roleKeyEncoder(role)),
        Some(role)
      )
    }
  }
}
