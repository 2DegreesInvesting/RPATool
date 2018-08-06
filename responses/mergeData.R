

library(readr)  # for read_csv()
library(dplyr)  # for mutate()
library(tidyr)  # for unnest()
library(purrr)  # for map(), reduce()

# if you are in the directory with the csv files
files <- dir(pattern = "*.csv")
files

data1 <- files %>%
    map(read_csv) %>%    # read in all the files individually, using
    # the function read_csv() from the readr package
    reduce(rbind)        # reduce with rbind into one dataframe

data1