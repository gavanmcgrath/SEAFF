#' Title update.ssts  Checks the date in ./data/sst.log to see if  sst.mnmean.nc is up to date, if not the it downloads the file from ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst/
#'
#' @return TRUE if file updated, FALSE otherwise
#' @export
#'
#' @examples update.slp()
update.slp <- function(){
  is.cur <- tryCatch(is.slp.current(),error = function(e) {
    warning("Likely no internet connection. Skipping check for new data.")
    TRUE})
  if (!is.cur){
    R.utils:::downloadFile(
    url = "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface/slp.mon.mean.nc",
      filename = "./data/SLP/slp.mon.mean.nc",
      overwrite = TRUE,
      skip=FALSE,
      username="Guest", password="anonymous",verbose=TRUE)
  }
  !is.cur
}
