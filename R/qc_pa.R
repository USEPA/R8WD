
#' Quality control screening of Purple Air data
#'
#' @param dat dataset obtained with `get_pa()`
#' @param sensor_agreement_pct required level of sensor agreement, expressed as percent difference from the lower of the two sensors.
#' @param sensor_agreement_ug required level of sensor agreement, expressed in absolute terms
#' @param column_name name of the column with data of interest.
#' @param sensor_names defines how sensors are labeled in column names (there should be one column per sensor). These values are appended to `column_name` to identify the columns with data.
#' @param device_name name of column with device IDs
#' @param date_column name of column with time stamps
#'
#' @return dataframe with a data-flag column (e.g., 'pm2.5_atm_flag') and a column (e.g., 'pm2.5_atm_qc') that only includes data meeting the sensor agreement threshold.
#' @export
#'
#' @examples
#' qc_pa
#'
qc_pa <- function(dat,
                  sensor_agreement_pct = 0.80, # expressed as decimal fraction
                  sensor_agreement_ug  = 5,    # expressed as ug/m3
                  column_name      =  'pm2_5_atm', # name of column to check (without a/b) #e.g., 'pm2.5_atm_' for API data
                  sensor_names     = c('', '_b'), # defines how sensors are labeled within a device. # c('a', 'b') for API data
                  device_name      = 'mac_address',  # 'sensor_index' for API data, 'mac_address' for SD card data
                  date_column      = 'UTCDateTime' # 'date'
) {
  ### removes data that don't meet A/B agreement threshold
  ### agreement calculated as (highest value - lowest value)/lowest
  column_names   <- paste0(column_name, sensor_names) # c('a', 'b'))
  col_index <- which(tolower(names(dat)) %in% tolower(column_names))
  ### remove NAs
  dat <- na.omit(dat)

  difference_pct <- apply(X = dat[, col_index], MARGIN = 1, FUN = function(x) {diff(range(x, na.rm = TRUE)) / min(x, na.rm = TRUE)})
  difference_abs <- apply(X = dat[, col_index], MARGIN = 1, FUN = function(x) {diff(range(x, na.rm = TRUE))})
  ### observations are accepted if they are within 20% of each other OR the difference is less than 5ug/m3
  ### so, only rejected if both conditions are met
  obs_rejected   <- sum((difference_pct > (1 - sensor_agreement_pct)) & (difference_abs > sensor_agreement_ug), na.rm = TRUE) # reject if difference is greater than threshold
  which_rejected <- which((difference_pct > (1 - sensor_agreement_pct)) & (difference_abs > sensor_agreement_ug))
  dat[, paste0(column_name, '_flag')] <- NA
  dat[, paste0(column_name, '_qc')]   <- rowMeans(dat[, col_index], na.rm = TRUE)

  ### show a plot
  tst.dat       <- data.frame(x = 0:500)
  tst.dat$y.max <- tst.dat$x*(1+(1-sensor_agreement_pct))
  # tst.dat$y.max[tst.dat$y.max < sensor_agreement_ug] <- 5
  tst.dat$y.other1 <- tst.dat$x + 5
  tst.dat$y.other2 <- apply(tst.dat[, c('y.other1', 'y.max')], FUN = max, MARGIN = 1)


  for (i in 1:length(unique(dat[, device_name]))) {
    sensor_index <- which(dat[, device_name] %in% unique(dat[, device_name])[i])
    tmpDat <- dat[sensor_index, col_index]
    tmp <- summary(lm(tmpDat))
    rsq <- round(tmp$r.squared, 3)
    int <- round(coef(lm(tmpDat))[1], 2)
    cf  <- round(coef(lm(tmpDat))[2], 2)

    sensor_rejected_ug  <- which((difference_abs[sensor_index] > sensor_agreement_ug))
    sensor_rejected_pct <- which((difference_pct[sensor_index] > (1 - sensor_agreement_pct)))
    sensor_rejected <- which((difference_pct[sensor_index] > (1 - sensor_agreement_pct)) & (difference_abs[sensor_index] > sensor_agreement_ug))


    graphics::plot(tmpDat,
                   main = paste0('Sensor agreement check for\ndevice ', unique(dat[, device_name])[i], ', ', gsub(x = column_name, pattern = '_', replacement = ' ')), #' (B = A*', cf, '+', int, '; r2 = ', rsq, ')'),
                   pch = 19, las = 1, cex = 0.75,
                   ylab = 'Sensor B', xlab = 'Sensor A')
    # graphics::lines(x = tst.dat$x, y = tst.dat$y.other2, col = 'red3', lty = 2)
    # graphics::lines(x = tst.dat$y.other2, y = tst.dat$x, col = 'red3', lty = 2)
    graphics::abline(a = 0, b = 1, col = 'gray30', lty = 2)
    # if (length(sensor_rejected) > 0) {
    graphics::points(tmpDat[sensor_rejected_ug, ], bg = 'red4', pch = 21, cex = 1.5)
    graphics::points(tmpDat[sensor_rejected_pct, ], bg = 'orange2', pch = 21, cex = 1.5)
    graphics::points(tmpDat[sensor_rejected, ], bg = 'red1', pch = 21, cex = 1.5)
    legend(x = 'topleft',  inset = .05,
           legend = c(paste0('Data (n = ', sum(!is.na(tmpDat[,1])), ')'),
                      paste0('A-B variation exceeding ', (1-sensor_agreement_pct)*100, '% (n = ', length(sensor_rejected_pct), ')'),
                      paste0('A-B variation exceeding ', sensor_agreement_ug, ' ug/m3 (n = ', length(sensor_rejected_ug), ')'),
                      paste0('A-B variation exceeding both QC criteria (n = ', length(sensor_rejected), ')')),
           bty = 'n',
           pch = c(21, 21, 21, 21), pt.cex = 1.5,
           pt.bg = c('black', 'orange2', 'red4', 'red1'),
           y.intersp=1
           )
    # }
  }

  if (obs_rejected > 0) {
    cat(obs_rejected, 'observations R-flagged due to sensor agreement less than ', sensor_agreement_pct*100, '%:\n', paste0(as.character(dat[which(difference_pct >  (1 - sensor_agreement_pct)), date_column]), '\n'), '\n')
    dat[which_rejected, paste0(column_name, '_flag')] <- 'R'
    dat[which_rejected, paste0(column_name, '_qc')]   <- NA
  }
  invisible(dat)
}

