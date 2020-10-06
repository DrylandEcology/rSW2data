SDA_spatialQuery2 <- function(geom, what='mukey', db = c("SSURGO", "STATSGO"),
  geomIntersection=FALSE
) {

  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)

  # sanity checks
  if(! what %in% c('mukey', 'geom')) {
    stop("query type must be either 'mukey' or 'geom'",call. = FALSE)
  }

  db <- match.arg(db)

  if (what == "mukey" && db == "STATSGO") {
    stop("query type 'mukey' for 'STATSGO' is not supported", call. = FALSE)
  }

  # geom must be an sp object
  if(! inherits(geom, 'Spatial')) {
    stop('`geom` must be a Spatial* object', call. = FALSE)
  }

  # geom must have a valid CRS
  if(is.na(sp::proj4string(geom))) {
    stop('`geom` must have a valid CRS', call. = FALSE)
  }

  # CRS conversion if needed
  target.prj <- "+proj=longlat +datum=WGS84"
  if(sp::proj4string(geom) != target.prj) {
    geom <- sp::spTransform(geom, sp::CRS(target.prj))
  }

  # WKT encoding
  # use a geometry collection
  wkt <- rgeos::writeWKT(geom, byid = FALSE)

  # slower query, returning geom + mukey
  # replacement for depreciated SDA_query_features()
  # 10-30x faster than spatial-returning query by input feature
  # TODO: this is 15x slower than non-spatial-returning-query in SDA_query_features()
  if(what == 'geom') {

    # as suggested in https://github.com/ncss-tech/soilDB/issues/36#issuecomment-349054067
    db_table <- switch(db, SSURGO = "mupolygon", STATSGO = "gsmmupolygon")

    # return intersection
    if(geomIntersection) {
      q <- sprintf("
               SELECT
                 mupolygongeo.STIntersection( geometry::STGeomFromText('%s', 4326) ).STAsText() AS geom, P.mukey
                 FROM %s AS P
                 WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1;", wkt, db_table, wkt)
    } else {
      # return overlapping
      q <- sprintf("
               SELECT
                 mupolygongeo.STAsText() AS geom, P.mukey
                 FROM %s AS P
                 WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1;", db_table, wkt)
    }


    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(soilDB::SDA_query(q))
    res <- soilDB::processSDA_WKT(res)
  }

  # faster query, returning mukey + muname
  # replacement for depreciated SDA_make_spatial_query()
  # ~ 3x faster than SDA_query_features()
  # TODO: how can we link these back with the source data?
  if(what == 'mukey') {
    q <- sprintf("SELECT mukey, muname
                FROM mapunit
                WHERE mukey IN (
                SELECT DISTINCT mukey from SDA_Get_Mukey_from_intersection_with_WktWgs84('%s')
                )", wkt)

    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(soilDB::SDA_query(q))
  }


  return(res)
}


