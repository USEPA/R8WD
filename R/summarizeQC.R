#' Summarize quality control data in a WQP dataset
#'
#' @param data data downloaded from WQP and pre-processed using `preProcessResults`
#'
#' @return list of dataframes:  all rep data; lightly processed rep data; rep performance summarized (as RPD if there is a maximum of two replicates in a set, or as coefficents of variation for n>2) by Tribe, analyte, year; blank data (field and lab); blank performance summarized (as number of values above MDLs) by Tribe, analyte, year, field/lab; data with field reps averaged and blanks removed
#' @export
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarize
#' @importFrom plyr join_all
#' @importFrom plyr rbind.fill
#' @importFrom stats sd



summarizeQC <- function(data) {
  ### function summarizes replicates and blanks in WQX data
  ### function will always summarize rep performance by calendar year, organization, and analyte

  ### function returns a list with a bunch of dataframes:
  ### - all rep data
  ### - lightly processed rep data
  ### - rep performance summarized (as RPD or CV) by Tribe, analyte, year
  ### - blank data (field and lab)
  ### - blank performance summarized (as number of values above MDLs) by Tribe, analyte, year, field/lab
  ### - data with field reps averaged and blanks removed

  ### TODO: make this handle all varieties of blanks, not just lab and field.
  ### TODO: some organizations do appear to identify both reps as QC samples. They may do so inconsistently. handle these cases better. UTEMTN
  ### TODO: incorporate other QC measures (matrix spikes?)
  ### TODO: find a dataset with lab reps to see if the function mishandles anything (e.g., what is behavior if a field rep is also lab repped? How are lab reps linked to parent sample?)

  ### convert pH from log scale to molar [H+]
  if (length(data$ResultMeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')]) > 0) {
    data$ResultMeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')] <- 10^(-1*data$ResultMeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')])
    data$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')] <- 10^(-1*data$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')])
  }
  ### check that pH has no detection limit entered
  # data$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')]
  ### Replicate performance
  ### pull out dups, exclude blanks
  ### including ActivityDepthHeightMeasure.MeasureValue in the ID avoids counting depth profiles as replicates (sometimes n = 17)
  ### still consistently seeing more than n=2 reps
  tmpDat           <- data[!grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
  tmpDat$year      <- as.numeric(substr(tmpDat$ActivityStartDate, 1, 4))
  ### old approach: grouping reps by unique org-site-date-parameter-depth combos
  indicator_vector <- paste0(tmpDat$OrganizationFormalName,"__", tmpDat$OrganizationIdentifier, '__', tmpDat$MonitoringLocationIdentifier, "__", tmpDat$CharacteristicName, "__", tmpDat$ActivityDepthHeightMeasure.MeasureValue, "__",tmpDat$ActivityStartDate)

  ### new approach: use group-defined reps, find nearest non-rep observation in time
  rep_numbers <- grep(x = tmpDat$ActivityTypeCode, pattern = 'Quality Control Sample-Field Replicate')
  ### find paired sample for each designated replicate
  rep_ids          <- indicator_vector[rep_numbers] # routine samples should have the same id, then find the closest in time
  tmpDat$id        <- indicator_vector
  ### old approach: identify reps without relying on correct ActivityTypeCode
  # tmpDat           <- tmpDat[tmpDat$id %in% indicator_vector[duplicated(indicator_vector)], ]
  ### new approach: insist on ActivityTypeCode
  tmpDat           <- tmpDat[tmpDat$id %in% rep_ids, ]
  rep_table <- table(tmpDat$id)
  more_than_two_reps <- unique(tmpDat$id)[unique(tmpDat$id) %in% names(rep_table[rep_table > 2])] # reps with >2 samples. 134/1394
  # match(more_than_two_reps, more_than_two_reps[i])
  ### now identify rep using nearest timestamp. There are some NAs for times - in those cases, return everything and use the CV
  for (i in 1:length(more_than_two_reps)) {
    ### odd situation with Turtle Mountain Environmental Office__TURTLEMT__TURTLEMT-SHUTTEBEACH__Escherichia coli__NA__2019-06-19 (n = 3)
    sub_df <- tmpDat[tmpDat$id %in% more_than_two_reps[i], ]
    tmpDat <- tmpDat[!(tmpDat$id %in% more_than_two_reps[i]), ] # remove subset
    ### find the non-QC sample collected nearest in time to the QC sample
    if (any(is.na(sub_df$ActivityStartDateTime))) {
      ### if times aren't entered, we can't make a determination. leave everything
      sub_df <- sub_df
    } else {
      target_time <- sub_df$ActivityStartDateTime[grep(x = sub_df$ActivityTypeCode, pattern = 'Quality Control Sample-Field Replicate')][1]
      ### remove all but the most likely rep value
      sub_df <- sub_df[-c(which.max(abs(sub_df$ActivityStartDateTime - target_time))), ] # which.max(c(0, 10, 10)) selects the first max value encountered
    }
    # cat(i)
    tmpDat <- rbind(tmpDat, sub_df)
    rm(sub_df)
  }
  IDs_to_remove <- paste0(tmpDat$OrganizationFormalName,"__", tmpDat$OrganizationIdentifier, '__', tmpDat$MonitoringLocationIdentifier, "__", tmpDat$CharacteristicName, "__", tmpDat$ActivityDepthHeightMeasure.MeasureValue, "__",tmpDat$ActivityStartDate, "__", tmpDat$ActivityStartTime.Time)
  ### end new approach

  ### Identify lab reps
  lab.reps                <- tmpDat$id[grepl(x = tmpDat$ActivityTypeCode, pattern = 'Quality Control Sample-Lab Duplicate')]
  tmpDat$ActivityTypeCode <- ifelse(tmpDat$id %in% lab.reps, 'Lab replicate', 'Field replicate') # hesitant to overwrite this column but I think it makes sense to use the existing column.
  ###
  tmpDat           <- tmpDat[order(tmpDat$OrganizationFormalName, tmpDat$ActivityStartDate, tmpDat$MonitoringLocationIdentifier, tmpDat$CharacteristicName), ]
  tmpDat           <- tmpDat[!is.na(tmpDat$ResultMeasureValue), ]

  ### Create dataset with averaged reps instead of both reps (double-counting in any stats)
  # returnDat <- data[which(!duplicated(indicator_vector)), ]
  detailed_IDs <- paste0(data$OrganizationFormalName,"__", data$OrganizationIdentifier, '__', data$MonitoringLocationIdentifier, "__", data$CharacteristicName, "__", data$ActivityDepthHeightMeasure.MeasureValue, "__",data$ActivityStartDate, "__", data$ActivityStartTime.Time)
  returnDat    <- data[which(!detailed_IDs %in% IDs_to_remove), ]

  rep_rejects <- NULL

  ### determine whether to use RPD or CV (are all reps pairs or are there cases with n>2?)
  if (nrow(tmpDat) > 0) {
    largest_replicate_set <- max(table(tmpDat$id))
    ### These may be duplicate entries? Everything is identical, including the values:
    # tst <- data[!grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
    # tst$id <- paste0(tst$OrganizationFormalName,"__", tst$OrganizationIdentifier, '__', tst$MonitoringLocationIdentifier, "__", tst$CharacteristicName, "__", tst$ActivityDepthHeightMeasure.MeasureValue, "__",tst$ActivityStartDate)
    # tst[grep(x = tst$id, pattern = 'Turtle Mountain Environmental Office__TURTLEMT__TURTLEMT-WHEATBEACH__E_coli__NA__2018-07-31'), ]
    ### ISSUE: FUN not found in local environment
    if (largest_replicate_set == 2) {
      fieldReps_proc_tmp    <- plyr::ddply(tmpDat,
                                           c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id', 'ActivityStartDate', 'year'),
                                           # .fun = function(ResultMeasureValue) {FUN(as.numeric(ResultMeasureValue))})
                                           plyr::summarize,
                                           n     = sum(!is.na(as.numeric(ResultMeasureValue))),
                                           range_diff = diff(range(na.omit(ResultMeasureValue))), # largest difference between reps
                                           RPD   = rpd(as.numeric(ResultMeasureValue)),
                                           aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
                                           range_pctmean = range_diff / aver, # difference in range as percent of mean value
                                           range_pctMDL  = range_diff / mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE), # difference in range as percent of MDL
                                           MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
                                           MDLs_identical = as.logical(sd(na.omit(DetectionQuantitationLimitMeasure.MeasureValue)) == 0)
      ) # ,
      variation_measure <- 'Relative Percent Difference'
      # n = sum(!is.na(as.numeric(ResultMeasureValue)))) # zeroes = reps were identical. Suggestive of raw data not being included
    } else if (largest_replicate_set > 2) {
      fieldReps_proc_tmp    <- plyr::ddply(tmpDat,
                                           c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id',  'ActivityStartDate', 'year'),
                                           # .fun = function(ResultMeasureValue) {FUN(as.numeric(ResultMeasureValue))})
                                           plyr::summarize,
                                           n     = sum(!is.na(as.numeric(ResultMeasureValue))),
                                           range_diff = diff(range(na.omit(ResultMeasureValue))),
                                           RPD   = cv(as.numeric(ResultMeasureValue)),
                                           aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
                                           range_pctmean = range_diff / aver,
                                           range_pctMDL  = range_diff / mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
                                           MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
                                           MDLs_identical = as.logical(sd(na.omit(DetectionQuantitationLimitMeasure.MeasureValue)) == 0)
      )
      variation_measure <- 'Coefficient of Variation'
    }
    message('Replicate sets had a maximum n = ', largest_replicate_set,'; ', variation_measure, ' used as variation measure.')

    # fieldReps_proc_mean   <- plyr::ddply(tmpDat, c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id', 'year') ,
    #                                      plyr::summarize,
    #                                      aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
    #                                      MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE), # TODO: note if MDLs are not identical. For now, conservative behavior seems like reporting the highest MDL
    #                                      n     = sum(!is.na(as.numeric(ResultMeasureValue))),
    #                                      MDLs_identical = as.logical(sd(na.omit(DetectionQuantitationLimitMeasure.MeasureValue)) == 0)
    #                                      )
    # reps_all    <- plyr::join_all(list(fieldReps_proc_tmp, fieldReps_proc_mean), by = c('OrganizationFormalName', 'OrganizationIdentifier', 'year',  'CharacteristicName', 'ActivityTypeCode', 'id'))
    reps_all    <- fieldReps_proc_tmp[order(fieldReps_proc_tmp$OrganizationFormalName, fieldReps_proc_tmp$ActivityStartDate, fieldReps_proc_tmp$MonitoringLocationIdentifier, fieldReps_proc_tmp$CharacteristicName), ]

    ### remove singletons - of no use for evaluating data quality
    if (any(reps_all$n == 1)) {
      message('Singular replicates not part of an identifiable pair were found in the dataset and ignored for summary purposes:\n', paste0(apply(X= reps_all[reps_all$n == 1, c(3, 4, 7)], FUN = function(x) {paste0(x, collapse = ' ')}, MARGIN = 1), collapse = '\n'), '\n')
      rep_rejects <- reps_all[reps_all$n == 1, c(1, 3, 4, 7, 12)]
      names(rep_rejects)[4] <- 'ResultMeasureValue'
    }
    reps_all    <- reps_all[reps_all$n > 1, ]

    ### summarize rep performance by Tribe, analyte, year
    rep.summary <- plyr::ddply(reps_all, c('OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'year'),
                               plyr::summarize,
                               n            = sum(!is.na(aver)),
                               range.median = median(range_diff, na.rm = TRUE),
                               range.pctMDL.median = median(range_pctMDL, na.rm = TRUE),
                               RPD.median   = median(RPD, na.rm = TRUE),
                               RPD.IQR      = IQR(RPD, na.rm = TRUE)
    )
    rep.summary$variation_measure <- variation_measure
    ### tmpDat[tmpDat$id == reps_all$id[2], ] # strange
    ### todo: resolve warnings appropriately
    # Warning messages:
    #   1: In diff(x, na.rm = TRUE) : NAs introduced by coercion
    # confirm reps removed
    # indicator_vector2 <- paste0(returnDat$OrganizationIdentifier,"-", returnDat$MonitoringLocationIdentifier, "-", returnDat$CharacteristicName, "-", returnDat$ActivityStartDate)
    # any(duplicated(indicator_vector2))
    ### remove rep values (lose a lot of columns; this could be improved)
    ### rename and join data
    newDat        <- as.data.frame(do.call('rbind', strsplit(as.character(reps_all$id),'__',fixed=TRUE)))
    names(newDat) <- c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityDepthHeightMeasure.MeasureValue', 'ActivityStartDate')
    newDat$ResultMeasureValue <- reps_all$aver
    newDat$DetectionQuantitationLimitMeasure.MeasureValue <- reps_all$MDL
    newDat$ActivityStartDate <- as.Date(newDat$ActivityStartDate, format = '%Y-%m-%d')
    # newDat$units
    returnDat2 <- plyr::rbind.fill(list(returnDat, newDat))
  } else {
    returnDat2  <- returnDat
    reps_all    <- data.frame(OrganizationFormalName       = NA,
                              OrganizationIdentifier       = NA,
                              MonitoringLocationIdentifier = NA,
                              CharacteristicName = NA,
                              ActivityTypeCode   = NA,
                              id                 = NA,
                              ActivityStartDate  = NA,
                              year           = NA,
                              date           = NA,
                              n              = NA,
                              range_diff     = NA,
                              RPD            = NA,
                              aver           = NA,
                              range_pctmean  = NA,
                              range_pctMDL   = NA,
                              MDL            = NA,
                              MDLs_identical = NA
                              )
    rep.summary <- data.frame(OrganizationIdentifier = NA,
    CharacteristicName  = NA,
    ActivityTypeCode    = NA,
    year         = NA,
    n            = NA,
    range.median = NA,
    range.pctMDL.median = NA,
    RPD.median   = NA,
    RPD.IQR      = NA)
  }


  # nrow(returnDat2) == nrow(returnDat)+nrow(newDat) # should be TRUE
  ### remove blanks and all other QC data
  returnDat2 <- returnDat2[!grepl(x = returnDat2$ActivityTypeCode, pattern = 'Quality Control Sample'), ] # 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
  returnDat2 <- returnDat2[order(returnDat2$OrganizationFormalName, returnDat2$ActivityStartDate, returnDat2$MonitoringLocationIdentifier, returnDat2$CharacteristicName), ]

  ### Separate blanks
  blanks <- data[grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ] # pattern = 'Blank'), ] #
  ### pH entry mis-categorized as blank in umut
  ### TODO:
  # blanks      <- blanks[!grepl(x = blanks$CharacteristicName, pattern = 'pH'), ]
  blanks$year <- as.numeric(substr(x = blanks$ActivityStartDate, start = 1, 4))
  ### Summarize blank performance
  blanks$exceedMDL <- ifelse(blanks$ResultMeasureValue > blanks$DetectionQuantitationLimitMeasure.MeasureValue, 1, 0)
  blanks[blanks$CharacteristicName %in% 'pH', c('ResultMeasureValue', 'DetectionQuantitationLimitMeasure.MeasureValue', 'exceedMDL')]
  blank.summary <- plyr::ddply(blanks, c('OrganizationFormalName', 'OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'year'),
                               plyr::summarize,
                               n          = sum(!is.na(ResultMeasureValue)),
                               blank.mean = mean(ResultMeasureValue, na.rm = TRUE),
                               blank.sd   = sd(ResultMeasureValue, na.rm = TRUE),
                               above_MDL= sum(exceedMDL)
  )
  # blank.summary$loc <- paste0(OrganizationIdentifier, '__', MonitoringLocationIdentifier)

  ### sort dataframes
  blanks <- blanks[order(blanks$OrganizationFormalName, blanks$ActivityStartDate, blanks$CharacteristicName), ]

  ### convert pH from molar [H+] to log scale (including DetectionQuantitationLimitMeasure.MeasureValue)
  if (length(blanks$ResultMeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')]) > 0)     blanks$ResultMeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')]             <- -log10(blanks$ResultMeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')])
  if (length(blanks$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')]) > 0)     blanks$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')]             <- -log10(blanks$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')])
  if (length(blank.summary$blank.mean[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]) > 0)    blank.summary$blank.mean[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]  <- -log10(blank.summary$blank.mean[grep(x = blank.summary$CharacteristicName, pattern = 'pH')])
  if (length(blank.summary$blank.sd[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]) > 0) blank.summary$blank.sd[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]         <- -log10(blank.summary$blank.sd[grep(x = blank.summary$CharacteristicName, pattern = 'pH')])
  if (length(returnDat2$ResultMeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')]) > 0) returnDat2$ResultMeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')] <- -log10(returnDat2$ResultMeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')])
  if (length(returnDat2$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')]) > 0) returnDat2$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')] <- -log10(returnDat2$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')])
  if (length(tmpDat$ResultMeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')]) > 0)     tmpDat$ResultMeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')]             <- -log10(tmpDat$ResultMeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')])
  if (length(tmpDat$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')]) > 0)     tmpDat$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')]             <- -log10(tmpDat$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')])
  if (!all(is.na(reps_all)) && (length(reps_all$aver[grep(x = reps_all$CharacteristicName, pattern = 'pH')]) > 0)) { # if there are reps and they include pH
    reps_all$aver[grep(x = reps_all$CharacteristicName, pattern = 'pH')] <- -log10(reps_all$aver[grep(x = reps_all$CharacteristicName, pattern = 'pH')])
    reps_all$MDL[grep(x = reps_all$CharacteristicName, pattern = 'pH')] <- -log10(reps_all$MDL[grep(x = reps_all$CharacteristicName, pattern = 'pH')])

    }

  ### return data with averaged reps, rep summary
  invisible(list(rep_raw       = tmpDat,        # all rep data
                 rep_proc      = reps_all,      # lightly processed rep data
                 rep_summary   = rep.summary,   # rep performance summarized by Tribe, analyte, year
                 blank_proc    = blanks,        # blank data (field and lab)
                 blank_summary = blank.summary, # summarized blank data by Tribe, analyte, year, field/lab
                 data          = returnDat2,    # data with field reps averaged and blanks removed
                 unpaired_reps = rep_rejects    # unpaired reps, excluded from summary info
                 ))
}

