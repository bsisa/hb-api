package ch.bsisa.hb.geo.model

/**
 * Models parameters necessary to compute `Bounds` from end-user geographical 
 * tracking defining two coordinates (GPS) on a raster image (pixels).
 * 
 * @author Patrick Refondini 
 */
case class BoundsRequest(rasterImgSize: Pixels, record1: PixelsCoordinates, record2: PixelsCoordinates)