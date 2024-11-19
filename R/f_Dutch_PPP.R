#' A function to obtain the Dutch PPP factor values
#' @param year The year of which the PPP factor should be downloaded, multiple years are possible, default is the whole dataset
#' @keywords Generic, Costing Manual, Purchasing Power Parity
#' @export f_dutch_other_prices

  f_factor_EurDutch_to_IntDollar <- function(
    year = "all"){

    # Filter the data for the specified years
    update_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    new_filepath_ppp <- paste0("data/df_ppp_", update_year, ".rds")

    df_ppp <- readRDS(new_filepath_ppp)

    # Select the specified years, or all years if not specified
    if(year != "all"){
      ppp <- df_ppp |>
        dplyr::filter(Year %in% year) |>
        dplyr::pull(PPP)

      print(ppp)
    }else{

      return(df_ppp)
  }
  }
