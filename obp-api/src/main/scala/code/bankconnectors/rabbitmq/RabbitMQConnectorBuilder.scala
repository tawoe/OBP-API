package code.bankconnectors.rabbitmq

import code.bankconnectors.generator.ConnectorBuilderUtil._
import net.liftweb.util.StringHelpers

import scala.language.postfixOps

object RabbitMQConnectorBuilder extends App {

  buildMethods(commonMethodNames.diff(omitMethods),
    "src/main/scala/code/bankconnectors/rabbitmq/RabbitMQConnector_vOct2024.scala",
     methodName => s"""sendRequest[InBound]("obp_${StringHelpers.snakify(methodName)}", req, callContext)""")
}

