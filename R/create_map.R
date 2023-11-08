#' Create Summary Map of Sampling Locations
#'
#' @param .data full WQP data frame with site location data
#' @param type type of map to generate (i.e. simple map or circle marker map)
#' @param cluster indicates whether or not to cluster nearby icons for cleaner viewing
#' @param parameter optional input if user wants to map mean values for a parameter (by default plots all available parameters)
#' @param parameterColumn name of the column with parameter names (not likely to require adjustment)
#' @param digits digits used for displaying rounded data
#' @param radius_arg radius of points used in 'basic circle' plot type
#'
#' @return an interactive map of sample locations displayed as a Leaflet plot
#'
#' @importFrom leaflet addLegend
#' @importFrom leaflet colorBin
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet providerTileOptions
#' @importFrom leaflet clearShapes
#' @importFrom leaflet fitBounds
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#'
#' @export

create_map <- function(.data, type = "basic circle", cluster = FALSE,
                           parameter = NA, parameterColumn = 'CharacteristicName',
                           digits = 3,
                      radius_arg = 3){
  suppressWarnings({

    # for circle marker map
    # taken from this stackoverflow: https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes,
                               "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

      return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions,
                                opacity = opacity, title = "Measurements"))
    }

    # create baseline summary data, filter to data with relevant parameter tested
    if (is.na(parameter)) {
      parameter = unique(.data[, parameterColumn])
    }
    else if (!is.na(parameter) && !any(grepl(pattern = parameter, x = unique(.data[, parameterColumn])))) {
      ### if user wants to show a parameter, make sure the parameter exists in the dataset
      ### if parameter is specified but not found, error out
      stop(parameter, ' not found in data. Try one of these: ', paste0(unique(.data[, parameterColumn]), collapse = ', '))
    }
    removed.sd <- dplyr::filter(.data, is.na(HUCEightDigitCode), !(CharacteristicName %in% parameter))

    # create new data column for standardized monitoring location type labels
    .data$MapSamplingLocLabel = .data$MonitoringLocationTypeName
    ml_sites = unique(.data$MonitoringLocationTypeName)
    for (i in 1 : length(unique(ml_sites))) {
      new_label = R8WD::monitoring_location_types$Label[which(R8WD::monitoring_location_types$Name == ml_sites[i])]

      .data$map_sampling_loc_label = gsub(pattern = ml_sites[i], replacement = new_label, .data$MapSamplingLocLabel)
    }

    sumdat <- .data %>%
      dplyr::filter(!(is.na(HUCEightDigitCode)), (CharacteristicName %in% parameter)) %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure) %>%
      dplyr::summarise("Sample_Count" = length(unique(ActivityIdentifier)),
                       "Visit_Count" = length(unique(ActivityStartDate)),
                       "Parameter_Count" = length(unique(CharacteristicName)),
                       "Label_Type" = MapSamplingLocLabel)


    map <- leaflet::leaflet()%>%
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo",
                                options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
      # get rid of whatever was there before if loading a second dataset
      leaflet::clearShapes() %>%
      # fit boundaries of the map to a relevant window
      leaflet::fitBounds(lng1 = min(sumdat$LongitudeMeasure), lat1 = min(sumdat$LatitudeMeasure),
                         lng2 = max(sumdat$LongitudeMeasure), lat2 = max(sumdat$LatitudeMeasure))

    # add custom markers for a simple map
    if (type == "simple") {
      locationTypeIcons <- leaflet::iconList(
        well <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'well-24.png'),
          iconWidth = 24, iconHeight = 24),
        river = leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'river-24.png'),
          iconWidth = 24, iconHeight = 24),
        reservoir <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'reservoir-24.png'),
          iconWidth = 24, iconHeight = 24),
        land <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'land-24.png'),
          iconWidth = 24, iconHeight = 24),
        surface <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'surface-24.png'),
          iconWidth = 24, iconHeight = 24),
        groundwater <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'groundwater-24.png'),
          iconWidth = 24, iconHeight = 24),
        anthropogenic <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'anthropogenic-24.png'),
          iconWidth = 24, iconHeight = 24),
        wetlands <- leaflet::makeIcon(
          iconUrl = file.path(system.file('extdata', package = 'R8WD'), 'markericons', 'wetlands-24.png'),
          iconWidth = 24, iconHeight = 24))

      data_callout_text <- paste0("Site ID: ", sumdat$MonitoringLocationIdentifier,
                                  "<br> Site Name: ", sumdat$MonitoringLocationName,
                                  "<br> Measurement Count: ", sumdat$Sample_Count, # why use AvtivityIdentifier?
                                  "<br> Visit Count: ", sumdat$Visit_Count)

      map <- map %>%
        leaflet::clearShapes()

      if (cluster == TRUE) {
        map <- map %>% leaflet::addMarkers(., data = sumdat, lat = as.numeric(sumdat$LatitudeMeasure), lng = as.numeric(sumdat$LongitudeMeasure),
                                           icon =~ locationTypeIcons[Label_Type], popup = data_callout_text)
      } else {
        map <- map %>% leaflet::addMarkers(., data = sumdat, lat = as.numeric(sumdat$LatitudeMeasure), lng = as.numeric(sumdat$LongitudeMeasure),
                                           icon =~ locationTypeIcons[Label_Type], popup = data_callout_text)
      }

      # add circle markers to the map
    } else if (type == "circle") {
      # for circle marker map
      sumdat$radius <- 3
      sumdat$radius <- ifelse(sumdat$Sample_Count>10,5,sumdat$radius)
      sumdat$radius <- ifelse(sumdat$Sample_Count>50,8,sumdat$radius)
      sumdat$radius <- ifelse(sumdat$Sample_Count>100,10,sumdat$radius)
      sumdat$radius <- ifelse(sumdat$Sample_Count>200,15,sumdat$radius)
      sumdat$radius <- ifelse(sumdat$Sample_Count>500,20,sumdat$radius)
      sumdat$radius <- ifelse(sumdat$Sample_Count>1500,30,sumdat$radius)

      # create legend and color palette
      site_size = data.frame(Sample_n = c("<9",">10",">50",">100",">200",">500",">1500"),
                             Point_size = c(3,5,8,10,15,20,30))
      site_legend = subset(site_size, site_size$Point_size %in% unique(sumdat$radius))

      pal <- leaflet::colorBin(
        palette = "Blues",
        domain = sumdat$Parameter_Count)

      map <- map %>%
        leaflet::addCircleMarkers(data = sumdat, lng =~ as.numeric(LongitudeMeasure), lat =~ as.numeric(LatitudeMeasure),
                                  color="black",fillColor =~ pal(Parameter_Count), fillOpacity = 0.7,
                                  stroke = TRUE, weight = 1.5, radius=sumdat$radius,
                                  popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier,
                                                 "<br> Site Name: ", sumdat$MonitoringLocationName,
                                                 "<br> Measurement Count: ", sumdat$Sample_Count,
                                                 "<br> Visit Count: ", sumdat$Visit_Count,
                                                 "<br> Characteristic Count: ", sumdat$Parameter_Count)) %>%
        leaflet::addLegend("bottomright", pal = pal, values =sumdat$Parameter_Count,
                           title = "Characteristics", opacity = 0.5) %>%
        addLegendCustom(colors = "black",
                        labels = site_legend$Sample_n, sizes = site_legend$Point_size*2)
    }
    else if (type == "basic circle") {
      plotdat <- .data[grepl(x = .data[, parameterColumn], pattern = parameter), ] %>%
        dplyr::filter(!(is.na(HUCEightDigitCode))) %>%
        dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure) %>%
        dplyr::summarise("median" = median(ResultMeasureValue, na.rm = TRUE),
                         "interquartile range"    = IQR(ResultMeasureValue, na.rm = TRUE),
                         "mean"   = mean(ResultMeasureValue, na.rm = TRUE),
                         "sd"     = sd(ResultMeasureValue, na.rm = TRUE),
                         "n"      = sum(!is.na(ResultMeasureValue))
        )
      pal <- leaflet::colorBin(
        palette = "Blues",
        domain = plotdat$median)


      map <- map %>%
        leaflet::addCircleMarkers(data = plotdat, lng =~ as.numeric(LongitudeMeasure), lat =~ as.numeric(LatitudeMeasure),
                                  color="black",
                                  fillColor =~ pal(plotdat$median),
                                  radius = radius_arg,
                                  fillOpacity = 0.7,
                                  stroke = TRUE, weight = 1.5,
                                  # fill   = plotdat$median,
                                  popup  = paste0("Site ID: ", plotdat$MonitoringLocationIdentifier,
                                                  "<br> Site Name: ", plotdat$MonitoringLocationName,
                                                  "<br> Measurement Count: ", plotdat$n,
                                                  "<br> Median (IQR): ", paste0(round(plotdat$median, digits), "\u00B1", round(plotdat$`interquartile range`, digits)),
                                                  "<br> Mean (SD): ", paste0(round(plotdat$mean, digits), "\u00B1", round(plotdat$sd, digits))))
    }
    else {
      stop(type, " is not one of the available map types. Try either 'circle', 'basic circle', or simple'.", collapse = ", ")
    }
    return(map)
  })
}
