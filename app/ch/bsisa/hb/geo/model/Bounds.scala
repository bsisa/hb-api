package ch.bsisa.hb.geo.model

/**
 * Models lower right (`southWest`), upper left (`northEast`) bounds used to defined a rectangle shape.
 * 
 * @author Patrick Refondini
 */
case class Bounds(southWest : Coordinates, northEast : Coordinates)