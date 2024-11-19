au <- priceR:: (from = "EUR", to = "FJD",
                                        start_date = "2015-01-01", end_date = "2016-01-01")


test <- read.csv("https://data-api.ecb.europa.eu/service/data/EXR?format=csvdata")
