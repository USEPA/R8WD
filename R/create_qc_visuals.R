


#' Create tables and figures summarizing quality control data
#'
#' @param dataset list produced by summarizeQC
#' @param form type of table desired; 'coarse', 'blank', or 'rep'
#' @param summaryPeriod time period for summary plots; `all`, `year`, `month`, or `day`
#' @param parameters_wo_blanks parameters with no expectation for blanks or lab reps
#' @param field_only  should blank table focus only on field blanks?
#' @param collection_rate_target target collection rate for quality control measures (blanks and replicates)
#' @param replicate_variation_threshold variation threshold used for evaluating replicate performance: what is the target level of precision?
#'
#' @return a table for inclusion in markdown documents
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarize
#' @importFrom plyr join_all
#' @export
#'

create_qc_visuals <- function(dataset,
                             form = 'coarse',
                             summaryPeriod = 'month', # used for reps and blank collection rate calculations
                             parameters_wo_blanks = c('DO', 'pH', 'Temperature', 'Turbidity', 'Specific conductivity', 'Conductivity', 'TDS'),
                             field_only = TRUE,
                             collection_rate_target = 10,
                             replicate_variation_threshold = 20
                             ) {
  ### create coarse summary table
  ### observations by parameter
  ### dataset should be output of summarizeQC: e.g., allSummary

  if (!grepl(x = tolower(form), pattern = c('coarse|blank|rep'))) {
    stop('`form` argument not recognized. Acceptable inputs are `coarse`, `blank`, or `rep`')
  }

  table1 <- NULL
  plot1 <- NULL
  plot2 <- NULL
  summary_period_abbrev <- substr(summaryPeriod, 1, 2)

  ### get total samples per analyte to calculate blank ratio
  tot_samples <- plyr::ddply(dataset$data, c('CharacteristicName'), plyr::summarize,
                             `Total observations`            = sum(!is.na(ResultMeasureValue))
  )
  tmpDat           <- dataset$data
  tmpDat$year      <- as.numeric(substr(tmpDat$ActivityStartDate, 1, 4))
  tmpDat$month     <- as.numeric(substr(tmpDat$ActivityStartDate, 1, 4)) + (as.numeric(substr(tmpDat$ActivityStartDate, 6, 7)) / 12)
  tmpDat$day       <- as.numeric(substr(tmpDat$ActivityStartDate, 1, 4)) + (as.numeric(substr(tmpDat$ActivityStartDate, 6, 7)) / 12) + (as.numeric(substr(tmpDat$ActivityStartDate, 9, 10)) / 31)
  tmpDat$ye        <- format(tmpDat$ActivityStartDate, '%Y')
  tmpDat$mo        <- format(tmpDat$ActivityStartDate, '%b-%Y')
  tmpDat$da        <- format(tmpDat$ActivityStartDate, '%d-%b-%Y')

  blank_dat        <- dataset$blank_proc
  blank_dat        <- blank_dat[!is.na(blank_dat$ResultMeasureValue), ]
  if (field_only) {
    blank_dat <- blank_dat[grep(x = blank_dat$ActivityTypeCode, pattern = 'Field'), ]
  }
  rep_data   <- dataset$rep_summary
  if (field_only) {
    rep_data <- rep_data[grep(x = rep_data$ActivityTypeCode, pattern = 'Field'), ]
  }

  if (grepl(x = tolower(form), pattern = 'coarse', fixed = TRUE)) {
    sample_data <- plyr::ddply(dataset$data, c('CharacteristicName'), plyr::summarize,
                               `Total samples collected` = sum(!is.na(ResultMeasureValue)))

    blk_data <- plyr::ddply(blank_dat, c('CharacteristicName'), plyr::summarize,
                            `Field blanks collected` = sum(!is.na(ResultMeasureValue)))
    if (nrow(blk_data) == 0) {
      blk_data <- data.frame(sample_data[, 1])
      names(blk_data) <- 'CharacteristicName'
      blk_data$`Field blanks collected` <- 0
    }
    rep_data2 <- plyr::ddply(rep_data, c('CharacteristicName'), plyr::summarize,
                            `Field replicates collected` = sum(n))
    if (nrow(rep_data2) == 0) {
      rep_data2 <- data.frame(sample_data[, 1])
      names(rep_data2) <- 'CharacteristicName'
      rep_data2$`Field replicates collected` <- 0
    }

    table1 <- plyr::join_all(list(sample_data, blk_data, rep_data2), by = 'CharacteristicName')
    names(table1)[1]      <- 'Parameter'
    table1[is.na(table1)] <- 0

  }

  if (grepl(x = tolower(form), pattern = 'blank', fixed = TRUE)) {
    # blank_dat                <- dataset$blank_proc
    blank_dat$date           <- as.POSIXct(blank_dat$ActivityStartDate, format = '%Y-%m-%d')
    blank_dat$year            <- as.numeric(substr(blank_dat$ActivityStartDate, 1, 4))
    blank_dat$month           <- as.numeric(substr(blank_dat$ActivityStartDate, 1, 4)) + (as.numeric(substr(blank_dat$ActivityStartDate, 6, 7)) / 13)
    blank_dat$day             <- as.numeric(substr(blank_dat$ActivityStartDate, 1, 4)) + (as.numeric(substr(blank_dat$ActivityStartDate, 6, 7)) / 13) + (as.numeric(substr(blank_dat$ActivityStartDate, 9, 10)) / 311)
    blank_dat$ye <- format(blank_dat$date, '%Y')
    blank_dat$mo <- format(blank_dat$date, '%b-%Y')
    blank_dat$da <- format(blank_dat$date, '%d-%b-%Y')

    # if (field_only) {
    #   blank_dat <- blank_dat[grep(x = blank_dat$ActivityTypeCode, pattern = 'Field'), ]
    # }
    blank_perf  <- blank_dat[grepl(x = tolower(blank_dat$ActivityTypeCode), pattern = 'field blank'), ]
    blank_perf$`Performance target met?` <- ifelse(blank_perf$exceedMDL == 1, 'No', 'Yes')
    blank_perf$`Performance target met?`[is.na(blank_perf$`Performance target met?`)] <- 'No MRL reported'
    blank_perf$`Performance target met?` <- ordered(x = blank_perf$`Performance target met?`, levels = c('No', 'Yes', 'No MRL reported'))

    # if (any(is.na(blank_perf$`Performance target met?`))) {
    #   blank_perf$`Performance target met?` <- ordered(x = blank_perf$`Performance target met?`, levels = c('No', 'Yes', 'NA'))
    #   value_cols <- c("red", "green", 'gray')
    # } else {
    #   blank_perf$`Performance target met?` <- ordered(x = blank_perf$`Performance target met?`, levels = c('No', 'Yes'))
    #   value_cols <- c("red", "green")
    # }



    ### blanks aren't expected for sonde parameters - could implement this better
    analytes    <- tot_samples$CharacteristicName[which(!tot_samples$CharacteristicName %in% parameters_wo_blanks)]
    if (length(analytes) == 0) {
      ### table should still exist if no non-sonde data is collected
      analytes    <- NA
    }

    blank_table <- expand.grid('Sample Type' = ifelse(field_only, 'Field Blank', c('Field Blank', 'Lab Blank')),
                               'Analyte' = analytes)
    blank_table <- blank_table[order(blank_table$`Sample Type`, blank_table$Analyte), ]

    if (nrow(blank_dat) == 0) {
      blank_table$`Blanks (no.)`                           <- 0
      blank_table$`Years with blanks`                      <- 0
      blank_table$`Blanks above MRL (no.)`                 <- NA
      blank_table$`Blanks above MRL (%)`                   <- NA
      blank_table$`Are replicates >10% of samples?`        <- "\u274C"
      blank_table$`Are <10% of replicates high-variation?` <- "\u274C"

      table1 <- blank_table

    } else {
      blank_int <- plyr::ddply(blank_dat, c('ActivityTypeCode', 'CharacteristicName'), plyr::summarize,
                               `Blanks (no.)`            = length(na.omit(year)),
                               `Years with blanks`       = length(na.omit(unique(year))),
                               `Blanks above MRL (no.)`  = sum(exceedMDL, na.rm = TRUE),
                               `pct_above_MDL`           = sum(exceedMDL, na.rm = TRUE)/length(na.omit(exceedMDL))*100,
                               `Blanks above MRL (%)`    = paste0(round(pct_above_MDL,0), '%')
      )
      blank_int$`Sample Type` <- sapply(X = strsplit(blank_int$ActivityTypeCode, '-'), FUN = '[[', 2)
      blank_int$Analyte       <- blank_int$CharacteristicName
      # using plyr::rbind.fill preserves possible sonde analytes that have data points labeled as blanks. join_all would eliminate them
      blank_int            <- plyr::join_all(list(blank_table, blank_int[, c(3:ncol(blank_int))]), type = 'full')
      blank_int            <- blank_int[order(blank_int$`Sample Type`, blank_int$Analyte), ]
      blank_int$`Total observations` <- tot_samples$`Total observations`[ match(blank_int$Analyte, tot_samples$CharacteristicName)]
      blank_freq           <- blank_int$`Blanks (no.)` / blank_int$`Total observations`*100
      blank_freq[is.na(blank_freq)] <- 0
      blank_int$`Blank collection rate`    <- paste0(round(blank_freq, 1), '%')
      blank_int$`Blanks (no.)`[is.na(blank_int$`Blanks (no.)`)]           <- 0
      blank_int$`Years with blanks`[is.na(blank_int$`Years with blanks`)] <- 0
      blank_int$`Blanks above MRL (%)`[is.nan(blank_int$pct_above_MDL)]   <- 'MRL not reported'
      blank_int$`Is blank collection rate >10%?`  <- ifelse(blank_freq >= 10.0, "\u2705", "\u274C")
      blank_int$`Are <5% of blanks contaminated?` <- ifelse(blank_int$pct_above_MDL <= 5.0, "\u2705", "\u274C")
      blank_int$`Are <5% of blanks contaminated?`[is.nan(blank_int$pct_above_MDL)]  <- 'MRL not reported'
      blank_int$`Are <5% of blanks contaminated?`[blank_int$`Blanks (no.)` == 0]    <- 'Blanks not reported'

      # table1             <- blank_int[, c(1:5, 7, 9:11)]
      table1             <- blank_int[, c(1:3, 9:10, 5, 7, 11)]
    }



    ### blank performance plot
    plot1 <- ggplot(blank_perf, ggplot2::aes(x = date, y = ResultMeasureValue)) +
      ggplot2::geom_point(ggplot2::aes(fill = `Performance target met?`), pch = 21, size = 3) +
      ggplot2::scale_fill_manual(values = c("red", "green", "gray"),
                                 labels = levels(blank_perf$`Performance target met?`),
                                 limits = levels(blank_perf$`Performance target met?`)
                                 ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(. ~ CharacteristicName, scales = 'free_y') +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.margin = ggplot2::margin(r = 40, unit = "pt")) +
      ggplot2::labs(y = 'Field blank level', x = '') +
      ggplot2::theme(legend.position='bottom')
    if ((nrow(blank_perf) > 0) && (diff(range(blank_perf$year)) < 2)) {
      plot1 <- plot1 + ggplot2::scale_x_datetime(breaks = "3 month", date_labels = "%b-%Y", minor_breaks = "1 month")
    } else {
      plot1 <- plot1 + ggplot2::scale_x_datetime(breaks = "2 years", date_labels = "%Y")
    }

    ### blank collection rate plot
    summary_period_abbrev <- substr(summaryPeriod, 1, 2)

    param_yrs_wo_blank <- plyr::ddply(blank_dat[, c('CharacteristicName', 'ResultMeasureValue', summary_period_abbrev)],
                                      c('CharacteristicName', summary_period_abbrev), plyr::summarize,
                                      number_of_blanks       = sum(!is.na(ResultMeasureValue)))

    ### which of these year-parameter combos have data collected?
    blanks_expected  <- plyr::ddply(tmpDat[, c('CharacteristicName', 'ResultMeasureValue', summary_period_abbrev)],
                                   c('CharacteristicName', summary_period_abbrev), plyr::summarize,
                                   number_of_obs       = sum(!is.na(ResultMeasureValue)))
    if (nrow(param_yrs_wo_blank) == 0) {
      blanks_expected$number_of_blanks <- NA
    } else {
      blanks_expected <- plyr::join_all(list(blanks_expected, param_yrs_wo_blank))
    }
    blanks_expected$number_of_blanks[is.na(blanks_expected$number_of_blanks)] <- 0
    blanks_expected$ratio <- blanks_expected$number_of_blanks / blanks_expected$number_of_obs * 100
    blanks_expected$`Collection rate target met?` <- ifelse(blanks_expected$ratio < collection_rate_target, 'No', 'Yes')
    if (tolower(summaryPeriod) == 'month') {
      blanks_expected[, summary_period_abbrev] <- as.Date(paste0(blanks_expected[, summary_period_abbrev], '-15'), format = '%b-%Y-%d')
    } else if (tolower(summaryPeriod) == 'year') {
      blanks_expected[, summary_period_abbrev] <- as.Date(paste0(blanks_expected[, summary_period_abbrev], '-01-01'), format = '%Y-%m-%d')
    } else if (tolower(summaryPeriod) == 'day') {
      blanks_expected[, summary_period_abbrev] <- as.Date(blanks_expected[, summary_period_abbrev], format = '%d-%b-%Y')
    }

    plot2 <- ggplot2::ggplot(data = blanks_expected[!(blanks_expected$CharacteristicName %in% parameters_wo_blanks) & !is.na(blanks_expected$ratio), ],
                             ggplot2::aes(x = get(summary_period_abbrev), y = ratio)) +
      ggplot2::geom_point(ggplot2::aes(fill = `Collection rate target met?`), pch = 21, size = 3) +
      # ggplot2::scale_x_continuous(breaks = seq(1800, 2500, 2), minor_breaks = seq(1800, 2500, 1)) +
      ggplot2::scale_fill_manual(values = c("red", "green"),
                                 labels = c("No", "Yes"),
                                 limits = c("No", "Yes")) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(. ~ CharacteristicName) +
      ggplot2::geom_hline(yintercept = collection_rate_target, linetype = 2) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))  +
      ggplot2::ylab('Field blank collection rate') +
      ggplot2::xlab('') +
      ggplot2::theme(legend.position='bottom')
    if (diff(range(blanks_expected[!(blanks_expected$CharacteristicName %in% parameters_wo_blanks) & !is.na(blanks_expected$ratio), summary_period_abbrev])) < 2) {
      plot2 <- plot2 + ggplot2::scale_x_date(breaks = "3 month", date_labels = "%b-%Y", minor_breaks = "1 month")
      } else {
      plot2 <- plot2 + ggplot2::scale_x_date(breaks = "2 years", date_labels = "%Y")
    }
  }

  if (grepl(x = tolower(form), pattern = 'rep', fixed = TRUE)) {
    rep_data <- dataset$rep_proc
    rep_data$date <- as.POSIXct(rep_data$ActivityStartDate, format = '%Y-%m-%d')
    rep_data$year            <- as.numeric(substr(rep_data$ActivityStartDate, 1, 4))
    rep_data$month           <- as.numeric(substr(rep_data$ActivityStartDate, 1, 4)) + (as.numeric(substr(rep_data$ActivityStartDate, 6, 7)) / 13)
    rep_data$day             <- as.numeric(substr(rep_data$ActivityStartDate, 1, 4)) + (as.numeric(substr(rep_data$ActivityStartDate, 6, 7)) / 13) + (as.numeric(substr(rep_data$ActivityStartDate, 9, 10)) / 311)
    rep_data$ye <- format(rep_data$date, '%Y')
    rep_data$mo <- format(rep_data$date, '%b-%Y')
    rep_data$da <- format(rep_data$date, '%d-%b-%Y')

    if (field_only) {
      rep_data <- rep_data[grep(x = rep_data$ActivityTypeCode, pattern = 'Field'), ]
    }

    rep_data$above_target <- ifelse(rep_data$RPD > replicate_variation_threshold, 1, 0)

    above20_by_param <- plyr::ddply(rep_data, c('CharacteristicName', 'ActivityTypeCode'), plyr::summarize,
                                    `Number of replicate sets`       = sum(!is.na(RPD)),
                                    `Replicate sets with >20% variation`  = paste0(sum(above_target, na.rm = TRUE), ' (', paste0(round(100*(sum(above_target, na.rm = TRUE) / sum(!is.na(RPD)))), '%'), ')')
                                    # `Proportion above 20%`           = paste0(round(100*(sum(above_target, na.rm = TRUE) / sum(!is.na(RPD)))), '%')
    )
    if (any(grepl(x = above20_by_param[, 4], pattern = ')'))) {
      ### if there's a single actual value, add clarity to the first parenthetical percentage
      above20_by_param[grepl(x = above20_by_param[, 4], pattern = ')'), 4][1] <- gsub(x = above20_by_param[grepl(x = above20_by_param[, 4], pattern = ')'), 4][1], pattern = ')', replacement = ' of reps)')
    }

    if (nrow(above20_by_param) == 0) {
      ### this is annoying ddply behavior
      above20_by_param <- expand.grid(CharacteristicName = tot_samples$CharacteristicName,
                                      ActivityTypeCode   = ifelse(field_only, 'Field replicate',c('Field replicate', 'Lab duplicate')),
                                      `Number of replicate sets`       = 0,
                                      `Replicate sets with >20% variation`  = NA #,
                                      # `Proportion above 20%`           = NA
                                      )
    }
    above20_by_param$`Sample Type` <- above20_by_param$ActivityTypeCode
    # Lab Duplicate
    analytes_reps    <- tot_samples$CharacteristicName
    # if (length(analytes_reps) == 0) {
    #   ### table should still exist if no non-sonde data is collected
    #   analytes_reps    <- NA
    # }

    # fillout_params <- expand.grid('Sample Type' =
    #                                 ifelse(field_only, 'Field replicate',c('Field replicate', 'Lab duplicate')),
    #                               'CharacteristicName' = analytes_reps)
    fillout_params <- expand.grid('Sample Type' =
                                    c('Field replicate', 'Lab duplicate'),
                                  'CharacteristicName' = analytes_reps)
    ### This ensures field params aren't expected to have lab reps
    setA <- grep(x = fillout_params$`Sample Type`, pattern = 'Lab')
    setB <- which(fillout_params$CharacteristicName %in% parameters_wo_blanks)
    fillout_params <- fillout_params[-c(intersect(setA, setB)), ]
    if (field_only) {
      ### adjust for focusing only on field samples, if desired
      fillout_params <- fillout_params[grepl(x = fillout_params$`Sample Type`, pattern = 'Field'), ]
    }

    above20_by_param <- plyr::join_all(list(fillout_params, above20_by_param[, c(1, 3:ncol(above20_by_param))]), by = c('Sample Type', 'CharacteristicName'), type = 'full')
    above20_by_param$`Number of replicate sets`[is.na(above20_by_param$`Number of replicate sets`)] <- 0
    above20_by_param <- above20_by_param[order(above20_by_param$`Sample Type`),]

    if (any(!is.na(rep_data$RPD))) {
      above20_by_param$tot_samples               <- tot_samples$`Total observations`[match(above20_by_param$CharacteristicName, tot_samples$CharacteristicName)]
      above20_by_param$rep_freq                  <- above20_by_param$`Number of replicate sets`/ above20_by_param$tot_samples * 100
      above20_by_param$`Replicate collection rate (%)` <- paste0(round(above20_by_param$rep_freq, 1), '%')
      above20_by_param$`Are replicates >10% of samples?`        <- ifelse(above20_by_param$rep_freq >= 10, "\u2705", "\u274C")
      # above20_by_param[paste0('Are <',replicate_variation_tolerance, '% of replicates high-variation?')] <- ifelse(above20_by_param$`Proportion above 20%` <= replicate_variation_tolerance, "\u2705", "\u274C")
    } else {
      above20_by_param$tot_samples                     <- NA
      above20_by_param$rep_freq                        <- NA
      above20_by_param$`Replicate Collection rate (%)` <- NA
      above20_by_param$`Are replicates >10% of samples?`        <- "\u274C"
      # above20_by_param[paste0('Are <',replicate_variation_tolerance, '% of replicates high-variation?')] <- "\u274C"
    }
    above20_by_param[c(4, 8)][above20_by_param$`Number of replicate sets` == 0,] <- 'Reps not reported'
    names(above20_by_param)[grep(x = names(above20_by_param), pattern = 'CharacteristicName')] <- 'Analyte'

    ### sub out actual performance criterion in column headers
    names(above20_by_param)[4:5] <- gsub(x = names(above20_by_param)[4:5], pattern = '20', replacement = replicate_variation_threshold)

    table1 <- above20_by_param[!is.na(above20_by_param$Analyte), c(1:3, 7:8, 4)]


    ### make rep performance plot
    field_rep_plot_data <- rep_data[grepl(x = rep_data$ActivityTypeCode, pattern = 'Field replicate'), ]
    field_rep_plot_data$`Performance target met?` <- ifelse(field_rep_plot_data$RPD > replicate_variation_threshold, 'No', 'Yes')
    field_rep_plot_data$`Performance target met?`[is.na(field_rep_plot_data$`Performance target met?`)] <- 'NA'
    if (any(is.na(field_rep_plot_data$`Performance target met?`))) {
      field_rep_plot_data$`Performance target met?` <- ordered(x = field_rep_plot_data$`Performance target met?`, levels = c('No', 'Yes', 'NA'))
      value_cols <- c("red", "green", 'gray')
    } else {
      field_rep_plot_data$`Performance target met?` <- ordered(x = field_rep_plot_data$`Performance target met?`, levels = c('No', 'Yes'))
      value_cols <- c("red", "green")
    }


    ### issue: code doesn't interpret ResultDetectionConditionText == 'Present Above Quantification Limit' - these appear as NAs
    ### for SRST:
    # tmp <- dataset$rep_raw[grepl(x = dataset$rep_raw$ActivityStartDate, pattern = '2016-08-11|2017-08-10'), ]
    # tmp2 <- tmp[grepl(x = tmp$MonitoringLocationIdentifier, pattern = 'SRSTEPA-SW-GR-2016-03|SRSTEPA-SW-GR-460-01'), ]
    # tmp2[tmp2$CharacteristicName %in% 'E_coli', ] # $ResultMeasureValue

    # rep_plot1 <- ggplot(rep_data[grepl(x = rep_data$ActivityTypeCode, pattern = 'Field replicate'), ], aes(x = date, y = RPD)) +
    #   geom_line(linetype = 2) + geom_point() + scale_y_continuous(labels = function(x) paste0(x, "%")) +
    #   facet_wrap(. ~ CharacteristicName, scales = 'free_y') + theme_bw() +
    #   geom_hline(yintercept = 20, linetype = 3) +
    #   labs(y = paste0(dataset$rep_summary$variation_measure[1], ' (field replicates)'), x = '') # '(\u00b11SD)'
    plot1 <- ggplot2::ggplot(field_rep_plot_data, ggplot2::aes(x = date, y = RPD)) +
      ggplot2::geom_point(ggplot2::aes(fill = `Performance target met?`), pch = 21, size = 3) +
      ggplot2::scale_fill_manual(values = value_cols,
                                 labels = levels(field_rep_plot_data$`Performance target met?`),
                                 limits = levels(field_rep_plot_data$`Performance target met?`)) +
      ggplot2::theme_bw() + ggplot2::facet_wrap(. ~ CharacteristicName, scales = 'free_y') +
      ggplot2::theme_bw() + ggplot2::theme(plot.margin = ggplot2::margin(r = 40, unit = "pt")) +
      ggplot2::geom_hline(yintercept = replicate_variation_threshold, linetype = 2) +
      ggplot2::labs(y =  paste0(dataset$rep_summary$variation_measure[1], ' (field replicates)'), x = '') +
      ggplot2::theme(legend.position='bottom')

    if ((nrow(field_rep_plot_data) > 0) && (diff(range(field_rep_plot_data$year)) < 2)) {
      plot1 <- plot1 + ggplot2::scale_x_datetime(breaks = "3 month", date_labels = "%b-%Y", minor_breaks = "1 month")
    } else {
      plot1 <- plot1 + ggplot2::scale_x_datetime(breaks = "2 years", date_labels = "%Y")
    }
    ### collection rate plot
    ### which of these year-parameter combos have reps collected?

    param_yrs_wo_reps      <- plyr::ddply(rep_data[, c('CharacteristicName', 'aver', summary_period_abbrev)],
                                     c('CharacteristicName', summary_period_abbrev), plyr::summarize,
                                     number_of_reps       = sum(!is.na(aver)))
    if (nrow(param_yrs_wo_reps) == 0) {
      param_yrs_wo_reps[1, ] <- c(NA, NA, NA)
      param_yrs_wo_reps$number_of_reps <- 0
    }
    reps_expected <- plyr::ddply(tmpDat[, c('CharacteristicName', 'ResultMeasureValue', summary_period_abbrev)],
                                 c('CharacteristicName', summary_period_abbrev), plyr::summarize,
                                 number_of_obs       = sum(!is.na(ResultMeasureValue)))
    reps_expected <- plyr::join_all(list(reps_expected, param_yrs_wo_reps))
    reps_expected$number_of_reps[is.na(reps_expected$number_of_reps)] <- 0
    reps_expected$ratio <- reps_expected$number_of_reps / reps_expected$number_of_obs * 100
    reps_expected$`Collection rate target met?` <- ifelse(reps_expected$ratio < collection_rate_target, 'No', 'Yes')

    if (tolower(summaryPeriod) == 'month') {
      reps_expected[, summary_period_abbrev] <- as.Date(paste0(reps_expected[, summary_period_abbrev], '-15'), format = '%b-%Y-%d')
    } else if (tolower(summaryPeriod) == 'year') {
      reps_expected[, summary_period_abbrev] <- as.Date(paste0(reps_expected[, summary_period_abbrev], '-01-01'), format = '%Y-%m-%d')
    } else if (tolower(summaryPeriod) == 'day') {
      reps_expected[, summary_period_abbrev] <- as.Date(reps_expected[, summary_period_abbrev], format = '%d-%b-%Y')
    }

    plot2 <- ggplot2::ggplot(data = reps_expected[!is.na(reps_expected$ratio), ],
                             ggplot2::aes(x = get(summary_period_abbrev), y = ratio)) +
      ggplot2::geom_point(ggplot2::aes(fill = `Collection rate target met?`), pch = 21, size = 3)+
      # ggplot2::scale_x_continuous(breaks = seq(1800, 2500, 2), minor_breaks = seq(1800, 2500, 1)) +
      ggplot2::scale_fill_manual(values = c("red", "green"),
                                 limits = c('No', 'Yes'),
                                 labels = c('No', 'Yes')) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(. ~ CharacteristicName, scales = 'free_y') +
      ggplot2::geom_hline(yintercept = collection_rate_target, linetype = 2) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))  +
      ggplot2::ylab('Field replicate collection rate') +
      ggplot2::xlab('') +
      ggplot2::theme(legend.position='bottom') +
      ggplot2::theme(axis.text.x=ggplot2::element_text(size=ggplot2::rel(0.75)))
    if (diff(range(reps_expected[!(reps_expected$CharacteristicName %in% parameters_wo_blanks) & !is.na(reps_expected$ratio), summary_period_abbrev])) < 2) {
      plot2 <- plot2 + ggplot2::scale_x_date(breaks = "3 month", date_labels = "%b-%Y", minor_breaks = "1 month")
    } else {
      plot2 <- plot2 + ggplot2::scale_x_date(breaks = "2 years", date_labels = "%Y")
    }

  }

  return(list(table                = table1,
              collection_rate_plot = plot2,
              performance_plot     = plot1))
}
