

#' Generate a Water Quality Report for an organization
#'
#' @param org organization (short name in WQP) whose data should be used to build the report. Multiple organization names can be input as a character vector; names will be homogenized and data will be pooled for analysis purposes.
#' @param startDate start date for data used in the report, in format `\%m-\%d-\%Y`
#' @param endDate final date for data used in the report, in format `\%m-\%d-\%Y`
#' @param parameters parameters to use in report. Must be entered as an index of acceptable parameters listed in params$params (e.g., `parameters = c(1:4,7)`)
#' @param extFile name of report-generating script, located in the inst/extdata folder of the R8WD R package
#' @param prompt_user if TRUE, user is prompted to use one of the organization codes in the `tribes` object provided with `R8WD`.
#' @param output either 'docx' or 'html'
#' @param output_directory directory where output will be located. If the directory doesn't exist it will be created.
#' @param draft_report TRUE or FALSE
#'
#' @return Quarto markdown document
#' @export
#' @examples
#' create_report(org = 'Tribe 1')
#' create_report(org = 'Tribe 2')
#'
create_report <- function(org = 'TURTLEMT',
                          startDate = '01-01-2015',
                          endDate   = '12-31-2022',
                          parameters = 1:11,
                          extFile = 'script_generateReport.qmd',
                          prompt_user = TRUE,
                          output = 'html',
                          output_directory = file.path(getwd(), 'R8WD_output'),
                          draft_report = TRUE) {

  ### format output
  if(!grepl(x = tolower(output[1]), pattern = '^docx$|^html$')) {
    stop('Acceptable output types are `html` or `docx`\n')
  } else {
    output <- tolower(output[1])
  }

  targetFile <- system.file('extdata', extFile, package = 'R8WD')
  ### check that output directory exists, create it if needed
  if (!dir.exists(output_directory)) {
    dir.create(output_directory)
  }
  # newFile    <- tempfile(paste0(org[1], '_QC-Report_', format(Sys.Date(), format = '%Y%m%d'), "_"), tmpdir = output_directory, fileext = '.qmd')
  ### make files easy for staff to work with
  newFile    <- file.path(output_directory, paste0(toupper(org[1]), '_QC-Report_', format(Sys.Date(), format = '%Y%m%d'), ".qmd"))
  # file.create(newFile, overwrite = TRUE)
  # if (file.exists(newFile)) file.remove(newFile)
  token      <- '\'REPLACE_THIS_TEXT\'' # capture quotations

  if (prompt_user) {
    if (!any(grepl(pattern = paste0(paste0('^', toupper(org), '$'), collapse = '|'), x = tribes))) {
      stop(toupper(org), ' not found in list of Tribal organizations. Recommended organization names:\n', paste0(sort(tribes), collapse = '\n'))
    }
  }

  ### if org is a vector, create an insertable string. This is benign where length == 1, so no ifelse statement
  org2 <- paste0("\'", paste0(gsub(x = toupper(org), pattern = "\'|\"", replacement = ''), collapse = '\',\''), "\'")
  ### test for presence
  # grep(token, readLines(targetFile), value = TRUE) # remove ^ and $ to make this work
  newText    <- gsub(x = readLines(targetFile), pattern = token, replacement = org2)

  ### add dates
  newText    <- gsub(x = newText, pattern = 'REPLACE_START_DATE', replacement = startDate)
  newText    <- gsub(x = newText, pattern = 'REPLACE_END_DATE',   replacement = endDate)

  ### add report type
  newText    <- gsub(x = newText, pattern = 'REPLACE_REPORT_TYPE', replacement = draft_report)


  ### add params
  # parameters
  # REPLACE_PARAMS
  newParams  <- paste0(paste0(gsub(x = as.character(parameters), pattern = "\'|\"", replacement = ''), collapse = ','))
  # tst <- gsub(x = readLines(targetFile), pattern = token, replacement = newParams)
  newText    <- gsub(x = newText, pattern = '\'REPLACE_PARAMS\'', replacement = newParams)

  ### format as html or docx
  if (grepl(x = output, pattern = '^html$')) {
  replacement_text <- ' html:
    embed-resources: true
    smooth-scroll: true
    theme: cosmo
    fontcolor: black
    toc: true
    toc-location: left
    toc-title: Table of Contents
    toc-depth: 3'
  supporting_data_text <-
    '## Supporting data

The following files contain the data used in this report:
'
  } else if (grepl(x = output, pattern = '^docx$')) {
    replacement_text <- ' docx:
    always_allow_html: yes
    prefer-html: true'
    supporting_data_text <- # until I learn how to embed csv files in created word docs
      ''
  }

  newText    <- gsub(x = newText, pattern = 'FORMAT_INPUT', replacement = replacement_text)
  newText    <- gsub(x = newText, pattern = 'SUPPORTING_DATA_TEXT', replacement = supporting_data_text)

  fileConn   <- file(newFile)
  writeLines(newText, fileConn)
  close(fileConn)

  utils::browseURL(newFile)

  # assign("args1", org, envir = .GlobalEnv)# <<- org # assign as global variable
  # fil <- tempfile('deleteMe', fileext = '.rds')
  # saveRDS(object = args1, file = fil, compress = FALSE)
  # print(args1)
  # source(system.file('extdata', extFile, package = 'R8WD'))
  # quarto::quarto_render(input = system.file('extdata', extFile, package = 'R8WD'))
  ### inconsistent quarto rendering error.
  # list.files('C://Program Files//RStudio//bin//quarto//bin//')
  # quarto_location <- 'C://Program Files//RStudio//bin//quarto//bin//quarto.cmd'
  # if (file.exists(quarto_location)) {
  #   Sys.setenv(QUARTO_PATH="/home/roger/quarto-cli/package/dist/bin/quarto")
  # }
}
