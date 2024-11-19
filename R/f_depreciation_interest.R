func_round_euro <- function(x){
  ifelse(x < 100,
         paste0("€", format(round(x, 2), nsmall = 2)),
         paste0("€", round(x, 0)))
}
#' @description
#' A function to calculate the costs of medical equipment based on Section 3.3 of the Dutch EE guideline; k = annual depreciation and interest expense [jaarlijkse afschrijvings- en rentekosten]
#' @param v_replace_val V: vervangingswaarde; replacement value
#' @param r_salvage_val R: restwaarde; salvage value
#' @param n_amortisation_period N: afschrijvingstermijn,; amortization period
#' @param i_interest_rt i: rentepercentage; interest rate
#' @param output Default of output is a DataFrame with both the annuity factor and yearly deprecation and interest costs, but the values can be selected independently
#' @keywords Generic, costs equipment

#' @export f_depreciation_interest

f_depreciation_interest <- function(
    v_replace_val,
    r_salvage_val,
    n_amortisation_period = 10,
    i_interest_rt = 0.025,
    output = "DataFrame"){

  # Annuity factor

  a_annuity_fct <- (1 / i_interest_rt) * (1 - (1 / (1 + i_interest_rt)^n_amortisation_period))

  # Yearly depreciation and interest costs
  k_annual_depr_int_exp <- (v_replace_val - (r_salvage_val /
                                               (1 + i_interest_rt)^n_amortisation_period)
  ) / (a_annuity_fct)

  # If parameter output = data frame
  if(output == "DataFrame"){
    out <- data.frame(a_annuity_fct, k_annual_depr_int_exp) |>
      dplyr::rename("Annuity factor" = a_annuity_fct,
             "Yearly depreciation and interest costs" = k_annual_depr_int_exp)

    return(out)

    # if parameter output = "annuity_fct" (Annuity factor)
  }else if(output == "annuity_fct"){
    print(a_annuity_fct)

    # if parameter output = annual_cost (Yearly depreciation and interest costs)
  }else if(output == "annual_cost"){
    print(k_annual_depr_int_exp)
  }
}


