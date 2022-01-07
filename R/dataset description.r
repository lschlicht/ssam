#' Full dataset of male blue tit start, end, and total activity, extra-pair gains and losses, and age
#'
#' @docType data
#'
#' @usage data(dataset)
#'
#' @format A data table with 35193 rows and 31 columns
#' \describe{
#' \item{ID}{Bird band number}
#' \item{season}{breeding season in which this data point was collected}
#' \item{firstEgg}{date of the first egg of this male's female. Used to calculate "rel_day" (see below)}
#' \item{date_}{date on which the specific data point was recorded}
#' \item{in_}{date and time of the entry (start of entry)}
#' \item{out_}{date and time of exit (start of exit)}
#' \item{in_duration}{duration of the entry (in seconds)}
#' \item{out_duration}{duration of the exit (in seconds)}
#' \item{rel_day}{days in relation to the social female's first egg date. 0 = day the first egg was laid; -1 = day before the first egg was laid; etc.}
#' \item{age}{male age as a two-level variable: yearling vs. older}
#' \item{EPP_gain}{extra-pair paternity gains as the number of EP offspring}
#' \item{EP_females}{extra-pair paternity gains as the number of EP females}
#' \item{EPP_loss}{extra-pair paternity losses as the number of EP offspring in the own nest}
#' \item{EPP_lossYN}{two-level variable: did this male have any extra-pair losses (0 = no, y = yes)}
#' \item{EPP_gainYN}{two-level variable: did this male have any extra-pair gains (0 = no, y = yes)}
#'
#' }
#' @keywords datasets
"x"
