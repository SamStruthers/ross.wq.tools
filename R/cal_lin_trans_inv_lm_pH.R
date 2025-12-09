#' @title Linear Transition Between Inverse Linear Models (pH Specific)
#'
#' @description
#' Converts millivolt values back to pH units using temporally-weighted inverse
#' linear transformation between two consecutive calibrations. This function
#' applies two-segment inverse linear models with temporal interpolation to
#' account for sensor drift across the calibration window. Uses pH value
#' thresholds to select between low-range and high-range inverse transformations
#' based on the original pH observations.
#'
#' @param df Tibble containing sensor data bounded by two calibrations
#' @param obs_col Character string specifying the column name containing
#'   original pH observations for range selection logic (default: "mean")
#' @param mv_col Character string specifying the column name containing
#'   millivolt values from pH linear model transformation (default: "mean_mV_f")
#' @param lm_coefs_col Character string specifying the column name containing
#'   linear model coefficients (default: "calibration_coefs")
#' @param wt_col Character string specifying the column name containing temporal
#'   weight parameters (default: "wt")
#'
#' @seealso [cal_wt()]
#' @seealso [cal_lm_pH()]
#' @seealso [cal_three_point_drift_pH()]

cal_lin_trans_inv_lm_pH <- function(df = ., obs_col, mv_col = "mean_raw", slope_col, offset_col, wt_col = "wt") {

  # Create output column names for pH conversion
  ph_col <- paste0(str_split_1(mv_col, "_")[1], "_lm_trans")

  # Convert millivolts back to pH using temporally-weighted inverse transformation
  df <- df %>%
    mutate(
      # hardcoding `slope_lead` for now.
      slope_delta = !!sym(slope_col) - slope_lead,
      offset_delta = !!sym(offset_col)  - offset_lead,
      # Inverse transformation for low pH range: x = (y-(b_1-wt(b_1-b_2)))/(m_1-wt(m_1-m_2))
      !!ph_col := (.data[[mv_col]] - (!!sym(offset_col) - (.data[[wt_col]] * offset_delta))) / (!!sym(slope_col) - (.data[[wt_col]] * slope_delta))
    ) %>%
    select(-c(slope_delta, offset_delta))

  return(df)
}
