package ch.bsisa.hyperbird.patman.simulations.messages

case class RefreshSimulationMessage(dbUser:String, dbPassword:String, resourcesToDelete:List[String]) {

}