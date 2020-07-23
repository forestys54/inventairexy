# Module UI
  
#' @title   mod_download_ui and mod_download_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_download
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
#' #' @title Download THEAI products.
#' @description The function downloads product.
#'  (the content must be an URL, and the name the product name).
#'
#' @param api Path of the 'api.txt' file containing credentials
#'  of cnes account. If NA (default) the default credentials
#' @param spatial_extent Spatial extent
#' @param tile Tile
#' @param orbit Orbit
#' @param time_interval Time interval
#' @param time_period Time period
#' @param level Level
#' @param platform Platform
#' @param maxcloud Max cloud cover
#' @param collection Collection
#' @param writedir Write dir
#'  (username 'user', password 'user') will be used.
#' @return NULL
#'
#' @author Thibault Aubry, FORESTYS, (2020) \email{thibault.aubry@@forestys.fr}
#' @note License: GPL 3.0
#' @importFrom reticulate r_to_py
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the whole product
#' product_download(prodlist, outdir = tempdir())
#' 
#' # Download a serie of products
#' pos <- st_sfc(st_point(c(12.0, 44.8)), crs = st_crs(4326))
#' time_window <- as.Date(c("2017-05-01", "2017-07-30"))
#' example_theia_list <- s2_list(spatial_extent = pos, time_interval = time_window)
#' product_download(example_theia_list, outdir = tempdir())
#' }
#'
mod_download_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_download
#' @export
#' @keywords internal
    
mod_download_server <- function(input, output, session){
  ns <- session$ns
  
  product_download <- function(spatial_extent = NULL, tile = NULL, orbit = NULL, # spatial parameters
                               time_interval = NULL, time_period = "full", # temporal parameters
                               level = "L2A", platform = "SENTINEL2A", maxcloud = 101, # platform parameters
                               collection = "SENTINEL", api = NA, writedir = ".") {
    
    # convert input NA arguments in NULL
    for (a in c("spatial_extent", "tile", "orbit", "time_interval", "api", "level", "platform", "maxcloud", "collection")) {
      if (suppressWarnings(all(is.na(get(a))))) {
        assign(a, NULL)
      }
    }
    
    # check if spatial_extent was provided
    spatial_extent_exists <- if (!exists("spatial_extent")) {
      FALSE
    } else if (is.null(spatial_extent)) {
      FALSE
    } else if (is(spatial_extent, "POLYGON")) {
      if (length(spatial_extent) == 0) {
        FALSE
      } else {
        TRUE
      }
    } else {
      TRUE
    }
    
    # if not, retrieve it from tile
    if (!spatial_extent_exists) {
      if (is.null(tile)) {
        print_message(
          type = "error",
          "Au moins un parametre de spatial_extent et tile doit etre specifie."
        )
      } else {
        # extract and import tiles kml
        s2tiles_kmz <- system.file("extdata", "vector", "s2_tiles.kmz", package = "shinycnes") # ? # kml = destine a la gestion de l'affichage des donnees dans les logiciels de SIG
        s2tiles_kml <- gsub("\\.kmz$", ".kml", s2tiles_kmz)
        if (!file.exists(s2tiles_kml)) {
          unzip(
            zipfile = s2tiles_kmz,
            files = basename(s2tiles_kml),
            exdir = dirname(s2tiles_kml),
            unzip = "internal"
          )
        }
        s2tiles <- st_read(s2tiles_kml, stringsAsFactors = FALSE, quiet = TRUE)
        # take the the selected tiles as extent
        # (this will result in the selection of more tiles, cause to overlapping
        # areas; it is filtered in s2_download, but it is slow: FIXME).
        # It is not possible to use tile centroids, because tile of external areas
        # of orbits could not be included).
        spatial_extent <- suppressWarnings(
          s2tiles[s2tiles$Name %in% tile, ]
        )
      }
    }
    
    # checks on inputs
    spatext <- st_bbox(st_transform(spatial_extent, 4326)) # ? 
    
    # pass latitude, longitude if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
    if (spatext["xmin"] == spatext["xmax"] || spatext["ymin"] == spatext["ymax"]) {
      lon <- mean(spatext["xmin"], spatext["xmax"])
      lat <- mean(spatext["ymin"], spatext["ymax"])
      lonmin <- lonmax <- latmin <- latmax <- NULL
    } else {
      lonmin <- spatext["xmin"]
      lonmax <- spatext["xmax"]
      latmin <- spatext["ymin"]
      latmax <- spatext["ymax"]
      lon <- lat <- NULL
    }
    
    # checks on dates
    # TODO add checks on format
    if (length(time_interval) == 1) {
      time_interval <- rep(time_interval, 2) # rep = replicate, ici deux fois
    }
    # split time_interval in case of seasonal download
    time_intervals <- if (time_period == "full") {
      data.frame(
        "start" = strftime(time_interval[1], "%Y%m%d"),
        "end" = strftime(time_interval[2], "%Y%m%d"),
        stringsAsFactors = FALSE
      )
    } else if (time_period == "seasonal") {
      data.frame(
        "start" = strftime(seq(time_interval[1], time_interval[2], by = "year"), "%Y%m%d"),
        "end" = strftime(rev(seq(time_interval[2], time_interval[1], by = "-1 year")), "%Y%m%d"),
        stringsAsFactors = FALSE
      )
    }
    
    # convert orbits to integer
    if (is.null(orbit)) {
      orbit <- list(NULL)
    } else {
      orbit <- as.integer(orbit)
      if (anyNA(orbit)) {
        orbit <- list(NULL)
      }
    }
    
    # define theia_download path
    theia_download_path <- system.file("theia_download", package = "shinycnes") # ?
    
    # link to api
    if (is.null(api)) {
      api <- file.path(theia_download_path, "config_theia.cfg")
    }
    if (!file.exists(api)) {
      print_message(
        type = "error",
        "Le fichier config_theia.cfg contenant les identifiants THEIA est manquant."
      ) # TODO build it
    }
    
    # set level
    level <- switch(
      level,
      L1C = "LEVEL1C",
      L2A = "LEVEL2A",
      L3A = "LEVEL3A",
      "LEVEL2A"
    )
    
    # set collection
    collection <- switch(
      collection,
      sentinel2 = "SENTINEL2",
      "SENTINEL2"
    )
    
    # set platform
    platform <- switch(
      platform,
      s2a = "SENTINEL2A",
      "SENTINEL2A"
    )
    
    # command to pass
    cmd <- paste(
      paste0(file.path(theia_download_path, "theia_download.py")), "--latmin", latmin,
      "--latmax", latmax, "--lonmin", lonmin, "--lonmax", lonmax, "--maxcloud", maxcloud,
      "--collection", collection, "--alternative_config", paste0(file.path(theia_download_path, "config_theia.cfg")),
      "--start_date", time_interval[1], "--end_date", time_interval[2], "--platform", platform,
      "--write_dir", writedir
    )
    
    # download product
    # trace_function(trace_fun = system2("python", cmd, stdout = TRUE, stderr = FALSE),
    #                trace_funname = "productdownload",
    #                trace_files = file.path(writedir, c(filename, paste0(filename, ".zip")))
    # )
    
    msg <- capture.output(system2("python", cmd, stdout = TRUE, stderr = FALSE), split = TRUE)[-c(1, 2)]
    
    # return(invisible(NULL))
    return(msg)
  }
}
    
## To be copied in the UI
# mod_download_ui("download_ui_1")
    
## To be copied in the server
# callModule(mod_download_server, "download_ui_1")
 
 

