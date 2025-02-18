package code.bankconnectors.generator

import code.bankconnectors.generator.ConnectorBuilderUtil._

object InOutCaseClassGenerator extends App {

  //We will copy the output to the `obp-commons/src/main/scala/com/openbankproject/commons/dto/JsonsTransfer.scala`
  //We need to check if the outbound/inbound is existing or not.
  val allExistingClasses = getClassesFromPackage("com.openbankproject.commons.dto")
  
  val code = connectorDeclsMethodsReturnOBPRequiredType
    .filterNot(it => allExistingClasses.toString.contains(s"${it.name.toString.capitalize}")) //find what we have not implemented 
    .map(it => {
      val returnType = it.returnType
      val tp = extractReturnModel(returnType)
      val isCaseClass = tp.typeSymbol.asClass.isCaseClass
      var payload = returnType.toString
        .replaceAll("([\\w\\.]+\\.)", "")
        .replaceFirst("OBPReturnType\\[Box\\[(.*)\\]\\]$", "$1")
        .replaceFirst("Future\\[Box\\[\\((.*), Option\\[CallContext\\]\\)\\]\\]$", "$1")
      if (!isCaseClass) {
        val name = tp.typeSymbol.name.toString
        println(name)
        payload = payload.replace(name, name + "Commons")
      }
      var parameters = it.asMethod.typeSignature.toString.replaceAll("([\\w\\.]+\\.)", "")
      if (parameters.startsWith("(callContext: Option[CallContext])")) {
        parameters = ""
      } else {
        parameters = parameters.replaceFirst("^\\(", ", ").replaceFirst(", callContext: Option.*$", "").replace(",", ",\n")
      }
      s"""
         |case class OutBound${it.name.toString.capitalize} (outboundAdapterCallContext: OutboundAdapterCallContext$parameters) extends TopicTrait
         |case class InBound${it.name.toString.capitalize} (inboundAdapterCallContext: InboundAdapterCallContext, status: Status, data: $payload) extends InBoundTrait[$payload]
     """.stripMargin
    })
  println("#################################Started########################################################################")
  code.foreach(println)
  
  println("#################################Finished########################################################################")
  println("Please copy and compair the result to obp-commons/src/main/scala/com/openbankproject/commons/dto/JsonsTransfer.scala")

}
