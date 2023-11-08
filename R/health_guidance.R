#' Data: PM2.5 health guidance
#'
#'
#' @format A dataframe with health guidance for various PM2.5 levels. Thresholds are based on in 40 CFR part 58 Appendix G (https://www.ecfr.gov/current/title-40/chapter-I/subchapter-C/part-58#Appendix-G-to-Part-58).
#' \itemize{
#'   \item pm_low category's lower-bound PM2.5 level
#'   \item pm_high category's upper-bound PM2.5 level
#'   \item fill_color Air Quality Index color associated with each category
#'   \item health_effects_statement statement of health effects guidance for each category, based on https://www.airnow.gov/aqi/aqi-calculator-concentration/ and 40 CFR 58
#'   \item cautionary_statement additional cautionary statement for each category
#'   \item levels_of_concern concise description of concern level, as appearing in https://www.airnow.gov/aqi/aqi-basics/
#'}
#' @docType data
#' @keywords aqi
#' @name health_guidance
#' @usage health_guidance
#' @examples
#' health_guidance
#'
NULL
