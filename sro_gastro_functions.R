med_iqr <- function(item, digits = 0, na.rm = FALSE) {
  quartiles <- stats::quantile(
    item,
    probs = seq(0.25, 0.75, 0.25),
    na.rm = na.rm
  )
  out <- sprintf(
    "%2$.*1$f (%3$.*1$f to %4$.*1$f)",
    digits,
    quartiles[2],
    quartiles[1],
    quartiles[3]
  )
  out[out == "NA (NA - NA)"] <- ""
  out
}