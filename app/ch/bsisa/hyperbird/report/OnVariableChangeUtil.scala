package ch.bsisa.hyperbird.report

import scala.xml.Node

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
  
  private var previousNode : Option[Node] = None
  private var currentNode : Option[Node] = None
  private var nextNode : Option[Node] = None
  
  def update(node : Node) : Unit = {
    previousNode = currentNode
   	currentNode = nextNode
   	nextNode = Option(node)
  }

  def getPrevious() : Option[Node] = {
    previousNode
  }
  
  def getCurrent() : Option[Node] = {
    currentNode
  }
  
  def getNext() : Option[Node] = {
    nextNode
  }  
  
}