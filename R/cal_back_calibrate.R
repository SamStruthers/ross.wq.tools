#' @title Back Calibration Orchestrator
#' @export
#'
#' @description
#' Orchestrates the complete back calibration workflow for sensor data chunks
#' bounded by two calibrations. This function determines the appropriate
#' calibration pathway based on sensor parameter type and applies the
#' corresponding sequence of correction functions. Standard sensors follow a
#' five-step process, turbidity uses two-point drift correction, and pH sensors
#' require specialized three-point processing with unit conversions.
#' pH sensor calibration pathway (specialized three-point processing)
#' pH calibration requires special handling due to:
#' - Slopes in mV/pH units and offsets in mV units (not direct pH units)
#' - Three-point calibration creating two slope transitions
#' - Need for unit conversion from pH to mV for back-calibration
#'
#' @param prepped_snsr_cal_df Tibble containing prepared sensor calibration data
#'   chunk from prepare_sensor_calibration_data(), bounded by two calibrations
#'
#' @seealso [cal_wt()]
#' @seealso [cal_inv_lm()]
#' @seealso [cal_lin_trans_lm()]
#' @seealso [cal_one_point_drift()]
#' @seealso [cal_two_point_drift()]
#' @seealso [cal_lm_pH()]
#' @seealso [cal_lin_trans_inv_lm_pH()]
#' @seealso [cal_three_point_drift_pH()]
#' @seealso [cal_check()]

cal_back_calibrate <- function(prepped_snsr_cal_df) {

  # Identify sensor parameter type for calibration pathway selection ----
  parameter <- unique(prepped_snsr_cal_df$parameter)

  # Calculate temporal weights for calibration interpolation ====
  chunk_w_time <- prepped_snsr_cal_df %>%
    cal_wt(df = ., dt_col = "DT_round")

  # Apply temporally-weighted linear transformation between calibrations ====
  # Standard sensor calibration pathway
  if (parameter %in% c("Chl-a Fluorescence", "FDOM Fluorescence", "ORP", "Pressure", "Specific Conductivity", "RDO", "Turbidity")) {
    back_calibrated_chunk <- chunk_w_time %>%
      cal_inv_lm(
        df = .,
        obs_col = "mean",
        slope_col = "slope_final",
        offset_col = "offset_final"
      ) %>%
      cal_lin_trans_lm(
        df = .,
        raw_col = "mean_raw",
        slope_col = "slope_final",
        offset_col = "offset_final",
        wt_col = "wt"
      )
  }

  # pH sensor calibration pathway
  if (parameter == "pH"){
    back_calibrated_chunk <- chunk_w_time %>%
      cal_lm_pH(
        df = .,
        obs_col = "mean",
        slope_col = "slope_final",
        offset_col = "offset_final"
      ) %>%
      cal_lin_trans_inv_lm_pH(
        df = .,
        mv_col = "mean_raw",
        slope_col = "slope_final",
        offset_col = "offset_final",
        wt_col = "wt"
      ) %>%
      group_by(DT_join) %>%
      filter(is.na(mean) | (mean < 7 & point == 1) | (mean >= 7 & point == 2)) %>%
      slice_min(point) %>%
      ungroup() %>%
      arrange(DT_round)
  }

  # Validate calibration results and create final calibrated values ====
  checked_df <- back_calibrated_chunk  %>%
    cal_check(df = ., obs_col = "mean", lm_trans_col = "mean_lm_trans")

  # Reorder the final columns to make post hoc analysis easier ====
  final_df <- checked_df %>%
    select(
      # DT sensor reading columns
      DT_round, DT_join,
      # Field ID columns
      site, sonde_serial, parameter, units,
      # Sensor reading transformation columns
      mean, mean_raw, mean_lm_trans, mean_cal, cal_check,
      # DT calibration information columns
      file_date, sonde_date, sensor_date_lag, sensor_date, sensor_date_lead,
      # Calibration information columns
      correct_calibration, slope_lag, offset_lag, slope, offset, slope_final, offset_final, slope_lead, offset_lead,
      # other
      flag
      # Remove everything else
    )

  # Return ====
  return(final_df)
}


