#' Title
#' Fetches and returns clean data from the Federal Reserces API using the FedR package
#' @param tag
#'
#' @return dt
#' @export
#'
#' @examples
#' fed_functs <- getFedData("DFF")
getFedData <- function(tag = "DFF"){


  if (tag == "DFF"){

  FF <- fred$series.observations(tag)

  dt <-  FF %>% select(date, value) %>% as_tibble() %>%
      mutate(Date = as.Date(date), Rate = as.numeric(value)/100) %>%
      group_by(Date = floor_date(Date, unit = "month")) %>% summarize(Rate = median(Rate))

  return(dt)
  }
}

