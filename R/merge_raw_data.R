
#' Merge raw Purple Air .csv files downloaded from SD card storage
#'
#' @param directory Directory with csv files containing daily data. Follow R syntax - use '/' or '\\' to separate directories (e.g., `directory = "CCST/CAA105/data"`).
#' @param output_file Name of output data file, including extension (e.g., `final_data.csv`). Optional; if left as NULL the code will generate a date-stamped .csv file in `directory`. If this argument is provided, the file must be a csv.
#' @param replace_file If an output file is provided, this argument indicates whether to overwrite existing data, or append new data to the file.
#'
#' @return a dataframe is returned to the R console, and a merged csv file is written to `directory`
#' @export
#'
#'

merge_raw_data <- function(directory, # directory with daily csv files
                           output_file = NULL, # final data file (optional - code will generate a date-stamped file if this is left NULL)
                           replace_file = TRUE # Should final data be overwritten or appended to?
                           ) {
  # directory <- 'C:/Users/thill03/OneDrive - Environmental Protection Agency (EPA)/Documents/QAPPs/Tribes/CCST/CAA105/data'

  ### some initial checks on arguments
  if(!dir.exists(directory)) {
    stop('Target directory does not exist. Please check location and syntax.\n')
  }
  if(!is.null(output_file) && !file.exists(output_file)) {
    stop('Output file does not exist. Please check the location and filename.\n')
  }
  if(!is.logical(replace_file)) {
    stop('`replace_file` argument must be either TRUE or FALSE.\n')
  }

  ### identify all csv files
  filenames <- list.files(path = directory, pattern = '^[0-9]{8}.csv$',
             recursive = TRUE, full.names = TRUE)

  readcsv_without_characters <- function(x, skip = 0) {
    ### a function to read a csv file without getting tripped up by unrecognized symbols
    a <- readChar(x,
                  useBytes = TRUE,
                  nchars = 100000000) # seems to be enough for a day's worth of PA data
    b <- gsub("\\\032", " ", a)
    new_a <- read.table(header = TRUE, text = b, sep = ",", skip = skip)
    return(new_a)
  }

  file.list <- lapply(X = filenames, FUN = readcsv_without_characters)
  new_data   <- do.call(rbind, file.list)

  if (is.null(output_file)) {
    write.csv(x = new_data, file = file.path(directory, paste0('output_', Sys.Date(), '.csv')), row.names = FALSE)
    message("Merged dataset saved as ", file.path(directory, paste0('output_', Sys.Date(), '.csv')), '\n')
  } else {
    output_file <- gsub(x = output_file, pattern = '.csv|.xls|.xlsx|.txt', replacement = '')

    if (replace_file == TRUE) {
      ### scenario: output file is being overwritten
      write.csv(x = new_data, file = file.path(directory, paste0(output_file, '.csv')), row.names = FALSE)
      message("Merged dataset saved as ", file.path(directory, paste0('output_', Sys.Date(), '.csv')), '\n')
    } else if (replace_file == FALSE) {
      ### scenario: new data appended to existing output file
      ### step 1: load the existing file (if it exists)
      if (file.exists(file.path(directory, paste0(output_file, '.csv'))) == TRUE) {
        old_data <- read.csv(file = file.path(directory, paste0(output_file, '.csv')))
        old_data <- old_data[rowSums(is.na(old_data)) != ncol(old_data), ]
        ### step 2: identify station-date rows that appear only in the new dataset
        id_old <- paste0(old_data$mac_address, old_data$UTCDateTime)
        id_new <- paste0(new_data$mac_address,  new_data$UTCDateTime)
        unique_rows    <- which(!(id_new %in% id_old))
        data_to_append <- new_data[unique_rows, ]

        ### step 3: append only new data to the existing file, export as csv
        new_data <- rbind(old_data, data_to_append)
        write.csv(x = new_data, file = file.path(directory, paste0(output_file, '.csv')), row.names = FALSE)
        message(length(unique_rows), " new rows of data were appended to merged dataset ", file.path(directory, paste0('output_', Sys.Date(), '.csv')), '\n')

      } else {
        stop(paste0('\n', paste0(output_file, '.csv'), ' not found in directory \n', directory, '\n - please confirm filename and extension are correct or set function argument `replace_file = TRUE`'))
      }
    }
  }
  ### return the merged data in R (avoid the need to load it again to do additional processing)
  invisible(new_data)
}

### usage example:
# dat <- merge_raw_data(directory = "C:/Users/thill03/OneDrive - Environmental Protection Agency (EPA)/Documents/QAPPs/Tribes/CCST/CAA105/data",
#                replace_file = TRUE)
# head(dat)
