/**
 *
 */
package ch.bsisa.hyperbird

/**
 *
 * Contains POC project whose goal is to dynamically provide graphical user 
 * interface feedback to view running Actors on a map using Javascript Leaflet.
 * 
 * 
 * ==Actors==
 * 
 * [[ch.bsisa.hyperbird.actview.actors]] package contains actors modeling fleets 
 * [[ch.bsisa.hyperbird.actview.actors.FleetActor]] of geo-located objects
 * [[ch.bsisa.hyperbird.actview.actors.ObjectActor]] whose travel is activated 
 * once provided with a `destination`.
 *  
 * The travel computation is delegated to a child actor 
 * [[ch.bsisa.hyperbird.actview.actors.DriverActor]] whose current 
 * implementation is very basic but provides movement toward destination at 
 * a speed which allows satisfying end-user feedback for demonstration.
 * 
 * ===Actors hierarchy===
 * 
{{{
   actviewActorSystem + ( = Akka.system )
                      |
                      +- FleetActor -+
                                        |
                                        +- ObjectActor -+- DriverActor
                                        |
                                        +- ObjectActor -+- DriverActor
                                        |
                                        +- ObjectActor -+- DriverActor
                                        |
                                        + (...)
}}}                                        
 *     
 *     
 * ==API==
 * 
 * [[ch.bsisa.hyperbird.actview.controllers]] package contains a [[play.api.mvc.Controller]]
 * [[ch.bsisa.hyperbird.actview.controllers.ActviewApi]] which implements the REST API:
{{{
GET      /api/melfin/actview/launch                                        ch.bsisa.hyperbird.actview.controllers.ActviewApi.launchFleet(name, colour ?= "blue")
GET      /api/melfin/actview/shutdown                                      ch.bsisa.hyperbird.actview.controllers.ActviewApi.shutdownFleet(name)
GET      /api/melfin/actview/broadcast/:fleetName                          ch.bsisa.hyperbird.actview.controllers.ActviewApi.broadcastFleet(fleetName, message)
GET      /api/melfin/actview/destination/:fleetName/:objectId              ch.bsisa.hyperbird.actview.controllers.ActviewApi.getObjDestination(fleetName, objectId) 
GET      /api/melfin/actview/position/:fleetName/:objectId                 ch.bsisa.hyperbird.actview.controllers.ActviewApi.getObjPosition(fleetName, objectId)
}}}
 * 
 * 
 * ==Server Side Event push to JS Client==
 * 
 * Communication from server to Javascript clients is accomplished using client side 
 * `EventSource` and server side push defined in [[ch.bsisa.hyperbird.sse.ServerSideNotification]]
 * 
 * 
 * @author Patrick Refondini
 *
 */
package object actview {

}