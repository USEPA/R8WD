#' Get Purple Air data from Purple Air API (based on https://api.purpleair.com/)
#'
#' @param api_key an API key obtained from your personal Purple Air account. Your first 1m tokens (i.e., data points downloaded) are free.
#' @param start_date start date for period of record
#' @param end_date end date for period of record
#' @param sensor sensor number. This is assigned by purple air and can be identified using their sensor map
#' @param average_interval averaging interval for returned data, expressed in minutes. The acceptable inputs are 0 (real-time), 10 (default if not specified), 30, 60, 360 (6 hour), 1440 (1 day)
#' @param parameters vector of parameter names to download. For a complete list of fields, see https://api.purpleair.com/
#'
#' @return dataframe
#' @export
#' @importFrom httr GET


get_pa <- function(
    api_key,
    start_date = as.POSIXct('2023-05-01', format = '%Y-%m-%d'), # posixct or malleable by as.posixct. will be converted to unix timestamp in seconds since Jan 1, 1970
    end_date = as.POSIXct(Sys.time()),
    sensor = 2478,
    average_interval = 1440, # 24-hour averages - raw data can only be downloaded for past 2 days
    parameters = c('humidity', # https://api.purpleair.com/ sensor data fields table: Relative humidity inside of the sensor housing (%). On average, this is 4% lower than ambient conditions. Null if not equipped.
                   'temperature', # Temperature inside of the sensor housing (F). On average, this is 8F higher than ambient conditions. Null if not equipped.
                   'pressure', # Current pressure in Millibars.
                   # 'pm2.5_atm', # three ways of converting plantower signal to ug/m3. This is ATM variant
                   'pm2.5_atm_a',
                   'pm2.5_atm_b'#,
                   # 'pm2.5_alt', # ALT variant, described at https://api.purpleair.com/
                   # 'pm2.5_alt_a',
                   # 'pm2.5_alt_b',
                   # 'pm2.5_cf_1', # CF = 1 variant
                   # 'pm2.5_cf_1_a',
                   # 'pm2.5_cf_1_b'
    ) # 'SENSOR_HISTORY_PM25_FIELDS'
) {
  # https://api.purpleair.com/#api-welcome-using-api-keys
  ### sign up for developer account to generate an API key
  ### create a project and transfer 'points' to the project
  ### then this query will work.
  ### queries burn through points. Not useful to pursue this.
  # api_key     <- API_key
  # start_date <- as.POSIXct('2023-05-01', format = '%Y-%m-%d')
  # sensor     <- 2478
  # average_interval <- 1440
  # end_date   <- as.POSIXct(Sys.time())
  # parameters = c(#'latitude', 'longitude', 'altitude', 'rssi',
  #                'humidity', 'temperature', 'pressure',
  #                'pm2.5_atm',
  #                'pm2.5_atm_a',
  #                'pm2.5_atm_b',
  #                'pm2.5_alt',
  #                'pm2.5_alt_a',
  #                'pm2.5_alt_b',
  #                'pm2.5_cf_1',
  #                'pm2.5_cf_1_a',
  #                'pm2.5_cf_1_b')

  start_time <- as.character(as.integer(as.POSIXct(start_date))) # can only download 2 days of real-time data
  end_time <- as.character(as.integer(as.POSIXct(end_date)))
  ### assemble url extension
  params_condensed <- paste0('&fields=', paste0(parameters, collapse = '%2C%20'))
  pa_url <- "https://api.purpleair.com/v1/sensors/"
  # Pull most recent data from API
  # fields <- paste0("?average=', average_interval, '&start_timestamp=", start_time, params_condensed
  # )
  # raw_data_hist <- httr::GET(url = paste(pa_url, sensor, fields,sep=""),
  #                            config = add_headers('X-API-Key' = api_key)) # lexical error
  # #Structurized data in form of R vectors and lists
  # jsonParsed     <- fromJSON(content(raw_data_hist, as='text'))
  # #Dataframe from JSON data
  # modJson        <- as.data.frame(jsonParsed)

  ### pull historical data as csv
  # GET https://api.purpleair.com/v1/sensors/2478/history/csv?start_timestamp=1682913600&end_timestamp=1686488883&average=1440&fields=pm2.5_atm%2C%20pm2.5_atm_a%2C%20pm2.5_atm_b
  fields <- paste0('?start_timestamp=', start_time, '&end_timestamp=', end_time, '&average=', average_interval, params_condensed
  )
  raw_data_csv <- httr::GET(url = paste(pa_url, sensor, '/history/csv', fields,sep=""),
                            config = add_headers('X-API-Key' = api_key)) # 740 points per query?!

  dat <- read.csv(textConnection(content(raw_data_csv, as='text')))
  str(dat)
  dat$date <- as.Date(as.POSIXct(dat$time_stamp, origin = '1970-01-01')) # weird that it's all out of order

  dat <- dat[order(dat$date), ]

  invisible(dat)

}
