package ch.bsisa.hb.geo.model

/**
 * Models 3 dimensions coordinates.
 * 
 * Used for swiss federal and GPS coordinates although x, y, z can be interpreted as a general 3 dimensions vector.
 * 
 * @param xEastingLng - x or Easting or Longitude 
 * @param yNorthingLat - y or Northing or Latitude
 * @param zAltitude - z or Altitude
 * 
 * @author Patrick Refondini 
 */
case class Coordinates(xEastingLng : Double, yNorthingLat : Double, zAltitude : Double = 500)
