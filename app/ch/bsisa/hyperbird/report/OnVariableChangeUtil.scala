package ch.bsisa.hyperbird.report

/**
 * Small utility to help compare string value change from 
 * one for loop to another in the context of Play Template.
 * 
 * TODO: This solution waits for a better simpler solution.  
 */
class OnVariableChangeUtil {

  private var previousState: String = ""

  def isDifferent(currentState: String): Boolean = {
    println(s"compare ${currentState} with ${previousState}")
    val result = currentState != previousState
    previousState = currentState
    println(s"updated previousState to: ${previousState}" )    
    result
  }

}