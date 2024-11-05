#' Data about flights in New York city in 2013
#'
#'
#'
#' @format ## `flights`
#' A data frame with 336,776 rows and 8 columns:
#' \describe{
#'   \item{year, month, day}{The date of flights}
#'   \item{dep_time, sched_dep_time , dep_delay }{The description about dep_time}
#'   \item{arr_time ,sched_arr_time, arr_delay}{The description about arr_time}
#'   ...
#' }
#' @source <https://github.com/tidyverse/nycflights13>
"flights"


#' Data about the airports in US
#'
#'
#' @format ## `airports`
#' A data frame with 1,458 rows and 8 columns:
#' \describe{
#'   \item{faa}{The unique ID of airport}
#'   \item{name}{Airport Name}
#'   \item{lat, lon}{The position of airports}
#'   ...
#' }
#' @source <https://github.com/tidyverse/nycflights13>
"airports"