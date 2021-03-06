
#' @title Numbers from the official statistics on governmental deficit, surplus and debt of Eurozone's countries between 1999 and 2019
#'
#' @description Compilation of all the numbers from Austria's, Belgium's, Cyprus', Finland's, France's, Germany's, Greece's, Ireland's, Italy's, Luxembourg's, Malta's, the Netherlands', Portugal's, Slovakia's, Slovenia's and Spain's official statistics on governmental deficit, surplus, debt and associated accounts from 1999 to 2019.
#'
#' @details Data was extracted from [Eurostat's database](https://ec.europa.eu/eurostat/data/database) in August 2020 through the directory: Database by themes -> Economy and finance -> Government statistics -> Government finance statistics -> Government deficit and debt -> Government deficit/surplus, debt and associated data. After sub-setting by country and selecting only data from 1999 to 2019, all the numbers from all the tables in the aforementioned category were pooled together. We included data from 1999 to 2019, with 1999 being the starting point because it is the year in which the Euro was introduced as book money. Only countries that joined the Eurozone prior to 2009 were selected, so that at least 10 years of data were available.
#'
#' @format A numeric vector of length 7336.
#'
#' @source \url{https://ec.europa.eu/eurostat/data/database}

"pooled_sample"

#' @title First digits from Eurozone countries' official statistics on government deficit, surplus and debt from 1999 to 2019
#'
#' @description First digits from Spain's official statistics on government deficit, surplus, debt and associated data from 1999 to 2019.
#'
#' @details Data was extracted from [Eurostat's database](https://ec.europa.eu/eurostat/data/database) in August 2020 through the directory: Database by themes -> Economy and finance -> Government statistics -> Government finance statistics -> Government deficit and debt -> Government deficit/surplus, debt and associated data. After sub-setting by country and selecting only data from 1999 to 2019, all the numbers from all the tables in this category were aggregated and then \code{\link[daubl]{msdigit}} was used to obtain the second digits.
#'
#' @format A numeric vector of length 7097.
#'
#' @source \url{https://ec.europa.eu/eurostat/data/database}

"pooled_sample_bl1"

#' @title Second digits from Eurozone countries' official statistics on government deficit, surplus and debt from 1999 to 2019
#'
#' @description Second digits from Spain's official statistics on government deficit, surplus, debt and associated data from 1999 to 2019.
#'
#' @details Data was extracted from [Eurostat's database](https://ec.europa.eu/eurostat/data/database) in August 2020 through the directory: Database by themes -> Economy and finance -> Government statistics -> Government finance statistics -> Government deficit and debt -> Government deficit/surplus, debt and associated data. After sub-setting by country and selecting only data from 1999 to 2019, all the numbers from all the tables in this category were aggregated and then \code{\link[daubl]{smsdigit}} was used to obtain the second digits.
#'
#' @format A numeric vector of length 7068.
#'
#' @source \url{https://ec.europa.eu/eurostat/data/database}

"pooled_sample_bl2"
