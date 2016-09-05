package ch.bsisa.hb.geo.model

/**
 * Models [[Coordinates]] with additional `id` information.
 * 
 * `id` parameter is useful while dealing with list of coordinates requests.
 * This allows the caller to match coordinates results back to their intended 
 * target objects without relying on results position in a list.
 * 
 * @author Patrick Refondini
 */
case class CoordinatesWithId(id:String, coordinates:Coordinates)
