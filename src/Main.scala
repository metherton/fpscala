import scala.concurrent.Future
import scala.util.{Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Main {

  case class OrganisationalUnit(team: String, circle: String, superCircle: String)

  case class RequiredPermission(employeeId: String, team: String, circle: String, superCircle: String, workerOrganisationalUnits: Set[OrganisationalUnit])

  case class EmployeePermission(restrictionLevel: Int, employeeId: String, team: String, circle: String, superCircle: String, agentOrganisationalUnits: Set[OrganisationalUnit]) {
    def isAllowedTo(requiredPermission: RequiredPermission): Boolean = {
      println(s"required permission: $requiredPermission")
      agentOrganisationalUnits.exists(agentOrgUnit => {
        println(s"agentorgunit: $agentOrgUnit")
        requiredPermission.workerOrganisationalUnits.map(workerOrgUnit => {
          println(s"workerOrgUnit: $workerOrgUnit")
          Vector[Int](1,2,3,4).filter(_ >= restrictionLevel).forall((l) => {
            val result = isAllowedToAtLevel(requiredPermission.employeeId, agentOrgUnit.team, agentOrgUnit.circle, agentOrgUnit.superCircle, workerOrgUnit, l)
            println(s"result: $result")
            result
          })
        }).reduce((a, b) => a || b)
      })
    }
    def isAllowedToAtLevel(workerId: String, agentTeam: String, agentCircle: String, agentSuperCircle: String, workerOrgUnit: OrganisationalUnit, level: Int): Boolean = {
      println(s"workerId: $workerId, team: $agentTeam, circle: $agentCircle, supercircle: $agentSuperCircle, workerOrgUnit: $workerOrgUnit, level: $level")
      level match {
      case 0 => false
      case 1 => employeeId == workerId
      case 2 => agentTeam.equals(workerOrgUnit.team)
      case 3 => agentCircle.equals(workerOrgUnit.circle)
      case 4 => agentSuperCircle.equals(workerOrgUnit.superCircle)
      case 5 => true
    }
    }
  }

  def main(args: Array[String]): Unit = {

    val futureBoolean = Future.successful(true)
    val result = futureBoolean.map {
      case true => Right(42)
      case _ => Left(666)
    }.andThen {
      case Success(value) => println(s"value: $value")
      case _ => println("another value")
    }
    println(s"result: $result")

    println("Hello world!")
//    val workerOrgUnit1_1_1 = OrganisationalUnit("team1_1_1", "circle1_1", "superCircle1")
//    val workerOrgUnit2_2_2 = OrganisationalUnit("team2_2_2", "circle2_2", "superCircle2")
//    val orgUnitsWorker = Set(workerOrgUnit1_1_1, workerOrgUnit2_2_2)
//    val agentOrgUnit1_1_1 = OrganisationalUnit("team1_1_1", "circle1_1", "superCircle1X")
//    val orgUnitsAgent = Set(agentOrgUnit1_1_1)
//
//    val reqPermissionWorker = RequiredPermission("emp1", "teamX", "circleX", "supercircleX", orgUnitsWorker)
//
//    val empPermissionAgent = EmployeePermission(2, "employeeId", "teamX", "circleX", "superCircleX", orgUnitsAgent)
//    println(s"isAllowedTo: ${empPermissionAgent.isAllowedTo(reqPermissionWorker)}")

  }




}