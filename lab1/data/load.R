# a function for loading the data
# be sure to load the packages from lab1.Rnw first!

loadDatesData <- function(path = "~/stat215a/lab1/data/") {
  
  # load the dates data
  dates_orig <- read.table("sonoma-dates-epochNums.txt", fill=TRUE)
  
  # Get epoch numbers
  epoch_nums <- dates_orig %>%
    slice(1:1) %>%
    gather("ID", "number") %>%
    slice(-(1:2)) %>%
    slice(-(13001:13005)) %>%
    select(number) 
  epoch_nums[1, ] <- "1"
  
  # Get Dates
  epoch_dates <- dates_orig %>%
    slice(2:2) %>%
    gather("ID", "date") %>%
    slice(-(1:7)) %>%
    select("date")
  epoch_dates[1, ] <- "Tue Apr 27 17:10:00 2004"
  
  # Get Days
  epoch_days <- dates_orig %>%
    slice(3:3) %>%
    gather("ID", "day") %>%
    slice(-(1:2)) %>%
    slice(-(13001:13005)) %>%
    select("day")
  epoch_days[1, ] <- "12536.0069444444"
  
  dates_orig <- data.frame(number = epoch_nums,
                           date = epoch_dates,
                           day = epoch_days)
  return(dates_orig)
}




loadRedwoodData <- function(path = "~/stat215a/lab1/data/", source = c("all", "log", "net")) {
  # Arguments:
  #   path: the path indicating the location of the `sonoma-data*` data files.
  #         Path should be relative to the lab1.Rnw file.
  #   source: a character indicating whether we want to load 
  #         "sonoma-data-all.csv" ("all"), "sonoma-data-log.csv" ("log"), or
  #         "sonoma-data-net.csv" ("net")
  # Returns:
  #   a data frame consisting of the specified dataset
  
  # load in the csv file
  sonoma <- read.csv(paste0(path, "sonoma-data-", source, ".csv"))
  return(sonoma)
}


loadMoteLocationData <- function(path = "data/") {
  # fill me in!
}