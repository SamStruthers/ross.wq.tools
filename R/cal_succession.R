#' @title Calibration succession determination
#' @export

cal_succession <- function(df) {

  # Split the data between good and bad calibrations and then bind them again
  # Check if we have any good calibrations
  good_calibrations <- df %>%
    dplyr::filter(correct_calibration)

  if (nrow(good_calibrations) > 0) {
    good_calibrations <- good_calibrations %>%
      dplyr::arrange(sensor_date) %>%
      dplyr::mutate(
        sensor_date_lead = dplyr::lead(sensor_date, 1),
        slope_lead = dplyr::lead(slope, 1),
        offset_lead = dplyr::lead(offset, 1),
        sensor_date_lag = dplyr::lag(sensor_date, 1),
        slope_lag = dplyr::lag(slope, 1),
        offset_lag = dplyr::lag(offset, 1),
      )
  } else {
    # If no good calibrations, return the df as is
    return(df)
  }

  # Check if we have any bad calibrations
  bad_calibrations <- df %>%
    dplyr::filter(!correct_calibration)

  # Combine them
  calibrations <- dplyr::bind_rows(good_calibrations, bad_calibrations) %>%
    dplyr::arrange(sensor_date) %>%
    # forward fill the next good calibration
    tidyr::fill(
      sensor_date_lead, slope_lead, offset_lead,
      .direction = "down"
    ) %>%
    # backward fill the last good calibration
    tidyr::fill(
      sensor_date_lag, slope_lag, offset_lag,
      .direction = "up"
    )

  return(calibrations)
}
