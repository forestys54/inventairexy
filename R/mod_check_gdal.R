# Module UI
  
#' @title   mod_check_gdal_ui and mod_check_gdal_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_check_gdal
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
#' #' @title Check GDAL installation
#' @description The function check that GDAL is installed and updated to
#'  the minimum required version (2.1.3, since previous versions do not
#'  manage SAFE format).
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case no GDAL installation is found; if FALSE, a warning is shown
#'  and FALSE is returned.
#' @param force (optional) Logical: if TRUE, install even if it is already
#'  installed (default is FALSE).
#' @return Logical (invisible): TRUE in case the installation is ok, FALSE
#'  if GDAL is missing and abort=FALSE (otherwise, the function stops).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}, Pascal Obstetar, (2019) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @importFrom gdalUtils gdal_setInstallation gdal_chooseInstallation
#' @importFrom rgdal getGDALVersionInfo
#' @importFrom jsonlite fromJSON
#' @examples
#' 
mod_check_gdal_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_check_gdal
#' @export
#' @keywords internal
    
mod_check_gdal_server <- function(input, output, session){
  ns <- session$ns
  
  check_gdal <- function(abort = TRUE, force = FALSE) {
    
    # set minimum GDAL version
    gdal_minversion <- package_version("2.1.3")
    
    # update shinymodal
    shinyjs::html(
      "cnes_download_message",
      as.character(div(
        align = "center",
        p("Verification de GDAL ..."),
        p(
          style = "text-align:center;font-size:500%;color:darkgrey;",
          icon("spinner", class = "fa-pulse")
        )
      ))
    )
    
    # load the saved GDAL path, if exists
    binpaths <- load_binpaths()
    if (force != TRUE & !is.null(binpaths$gdalinfo)) {
      # print_message( type = 'message', 'GDAL is already set; to reconfigure it, set force = TRUE.'  )
      gdal_setInstallation(search_path = dirname(binpaths$gdalinfo), rescan = TRUE) # met en place les options d'installation locales de GDAL
      return(invisible(TRUE))
    }
    
    # If GDAL is not found, search for it
    print_message(type = "message", "Recherche d'une installation GDAL valide ...")
    gdal_check_fastpath <- tryCatch(gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE), error = print)
    
    # Check if this found version supports OpenJPEG
    gdal_check_jp2 <- tryCatch(gdal_chooseInstallation(hasDrivers = c("JP2OpenJPEG")), error = print)
    
    # If GDAL is not found, or if found version does not support JP2, perform a full search
    if (is.null(getOption("gdalUtils_gdalPath")) | is(gdal_check_jp2, "error") | is(gdal_check_fastpath, "error")) { # acceder aux valeurs d'une seule option de la fonction
      print_message(
        type = "message", "GDAL n'a pas ete trouve dans le dossier selectionne, recherche dans le systeme entier (cela peut prendre du temps ...).")
      gdal_setInstallation(ignore.full_scan = FALSE, verbose = TRUE)
    }
    
    # Check again if this found version supports OpenJPEG
    gdal_check_jp2 <- tryCatch(gdal_chooseInstallation(hasDrivers = c("JP2OpenJPEG")), error = print)
    
    # set message method
    message_type <- ifelse(abort == TRUE, "error", "warning")
    
    # If GDAL is not found, return FALSE (this should not happen, since GDAL is required by rgdal)
    if (is.null(getOption("gdalUtils_gdalPath")) | is(gdal_check_jp2, "error")) {
      print_message(type = message_type, "GDAL n'a pas ete trouve, merci de l'installer.")
      return(invisible(FALSE))
    }
    
    # check requisites (minimum version)
    gdal_version <- package_version(gsub("^GDAL ([0-9.]*)[0-9A-Za-z/., ]*", "\\1", rgdal::getGDALVersionInfo(str = "--version")))
    
    if (gdal_version < gdal_minversion) {
      print_message(type = message_type, "La version de GDAL doit etre au minimum la version ", gdal_minversion, ". Merci de le mettre a jour.")
      return(invisible(FALSE))
    }
    
    # check requisites (JP2 support)
    gdal_JP2support <- length(grep("JP2OpenJPEG", gdalUtils::gdalinfo(formats = TRUE))) > 0
    if (!gdal_JP2support) {
      print_message(
        type = message_type, "Votre installation locale de GDAL ne supporte pas le format JPEG2000 (JP2OpenJPEG). ", "Merci d'installerune version de GDAL compatible avec JP2OpenJPEGL.",
        if (Sys.info()["sysname"] == "Windows" & message_type == "error") {
          paste0("\nNous recommandons d'utiliser l'installeur OSGeo4W ", "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86", if (Sys.info()["machine"] == "x86-64") {
            "_64"
          }, ".exe), ", "pour choisir l'\"Installation avancee\" et ", "pour verifier les packages \"gdal-python\" and \"openjpeg\".")
        }
      )
      return(invisible(FALSE))
    }
    
    # set this version to be used with gdalUtils
    gdal_chooseInstallation(hasDrivers = c("JP2OpenJPEG"))
    
    # save the path for use with external calls
    bin_ext <- ifelse(Sys.info()["sysname"] == "Windows", ".exe", "")
    gdal_dir <- getOption("gdalUtils_gdalPath")[[1]]$path
    binpaths$gdalbuildvrt <- normalize_path(file.path(gdal_dir, paste0("gdalbuildvrt", bin_ext)))
    binpaths$gdal_translate <- normalize_path(file.path(gdal_dir, paste0("gdal_translate", bin_ext)))
    binpaths$gdalwarp <- normalize_path(file.path(gdal_dir, paste0("gdalwarp", bin_ext)))
    binpaths$gdal_calc <- if (Sys.info()["sysname"] == "Windows") {
      binpaths$python <- normalize_path(file.path(gdal_dir, paste0("python", bin_ext)))
      paste0(binpaths$python, " ", normalize_path(file.path(gdal_dir, "gdal_calc.py")))
    } else {
      normalize_path(file.path(gdal_dir, "gdal_calc.py"))
    }
    binpaths$gdal_merge <- if (Sys.info()["sysname"] == "Windows") {
      binpaths$python <- normalize_path(file.path(gdal_dir, paste0("python", bin_ext)))
      paste0(binpaths$python, " ", normalize_path(file.path(gdal_dir, "gdal_merge.py")))
    } else {
      normalize_path(file.path(gdal_dir, "gdal_merge.py"))
    }
    binpaths$gdal_polygonize <- if (Sys.info()["sysname"] == "Windows") {
      binpaths$python <- normalize_path(file.path(gdal_dir, paste0("python", bin_ext)))
      paste0(binpaths$python, " ", normalize_path(file.path(gdal_dir, "gdal_polygonize.py")))
    } else {
      normalize_path(file.path(gdal_dir, "gdal_polygonize.py"))
    }
    binpaths$gdaldem <- normalize_path(file.path(gdal_dir, paste0("gdaldem", bin_ext)))
    binpaths$gdalinfo <- normalize_path(file.path(gdal_dir, paste0("gdalinfo", bin_ext)))
    binpaths$ogrinfo <- normalize_path(file.path(gdal_dir, paste0("ogrinfo", bin_ext)))
    
    lapply(c("gdalbuildvrt", "gdal_translate", "gdalwarp", "gdaldem", "gdalinfo", "ogrinfo"), function(x) {
      binpaths[[x]] <- normalize_path(binpaths[[x]])
    })
    writeLines(jsonlite::toJSON(binpaths, pretty = TRUE), attr(binpaths, "path"))
    
    print_message(type = "message", "la version de GDAL en cours d'utilisation : ", as.character(gdal_version))
    return(invisible(TRUE))
  }
  
}
    
## To be copied in the UI
# mod_check_gdal_ui("check_gdal_ui_1")
    
## To be copied in the server
# callModule(mod_check_gdal_server, "check_gdal_ui_1")
 
