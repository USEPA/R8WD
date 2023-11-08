#' Join WQP Measurement and Site Data
#'
#' @description This function takes data from the same WQP query and combines
#'              site data with measurements to map observations with physical
#'              sampling locations
#'
#' @param dat.full output of query to WQP portal using readWQPdata function in
#'        the dataRetrieval package
#' @param dat.sites output of query to WQP portal using whatWQPdat.sites function in the
#'        dataRetrieval package
#'
#' @return a combined data frame of WQP measurements with corresponding sampling
#'         site data
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr rename_at
#' @importFrom dplyr select_at
#' @importFrom dplyr select
#' @importFrom dplyr vars
#' @importFrom dplyr ends_with
#' @importFrom stringr str_replace
#' @export

joinWQPProfiles <- function(full = "null",
                            sites = "null"){

  dat.full <- full
  dat.sites <- sites

  # Join station data to the WQP data
  if (length(dat.sites > 1)) {
    if (nrow(dat.sites) > 0) {
      join1 <- dat.full %>%
        # join stations to results
        dplyr::left_join(dat.sites, by = "MonitoringLocationIdentifier", multiple = "all") %>%
        # remove ".x" suffix from column names
        dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
        # remove columns with ".y" suffix
        dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    } else {join1 = dat.full}
  } else {join1 = dat.full}
  return(join1)
}
