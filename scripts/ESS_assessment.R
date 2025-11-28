cat("\nEFFECTIVE SAMPLE SIZE ASSESSMENT\n")

#' Calculate ESS efficiency rates by parameter type
ess_summary_by_type <- ess_diagnostics %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(
    n_params = dplyr::n(),
    mean_ess = round(mean(ESS)),
    min_ess = round(min(ESS)),
    q1_ess = round(quantile(ESS, 0.25)),  # 1st quartile
    median_ess = round(median(ESS)),
    q3_ess = round(quantile(ESS, 0.75)),  # 3rd quartile
    adequacy_rate = scales::percent(mean(ESS_Adequate), accuracy = 0.1),
    mean_efficiency = scales::percent(mean(ESS_Rate), accuracy = 0.1),
    .groups = "drop"
  )

cat("ESS Summary by Parameter Type:\n")
print(ess_summary_by_type)

#' Identify parameters with very low ESS
low_ess_params <- ess_diagnostics %>%
  dplyr::filter(ESS < 100) %>%
  dplyr::arrange(ESS)

if (nrow(low_ess_params) > 0) {
  cat("\nParameters with critically low ESS (< 100):\n")
  print(head(low_ess_params[c("Parameter", "ESS", "Type")], 10))
}