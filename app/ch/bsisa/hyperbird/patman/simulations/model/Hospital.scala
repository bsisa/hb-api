package ch.bsisa.hyperbird.patman.simulations.model

import java.util.Date

/**
 * Models an hospital 
 */
case class Hospital(code:String, schedule:Date, beds:List[Bed]) {

}