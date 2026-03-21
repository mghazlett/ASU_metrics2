library(haven)
library(dplyr)
library(readr)

ple <- read_dta("C:/PLE/data/ple_dataset.dta")
auth <- read_csv("C:/PLE/data/authoritarian_episodes_refined.csv", show_col_types = FALSE)

cat("PLE dataset: n =", nrow(ple), "| years:", min(ple$year), "-", max(ple$year), "\n")
cat("Countries:", n_distinct(ple$iso), "\n\n")

# For each authoritarian episode, check:
# 1. Is the country (iso) in the PLE dataset?
# 2. How many years of fstgdp data exist before treatment (start_yr)?
# 3. How many years after treatment?

check_coverage <- function(iso_code, start, end, min_pre = 5, min_post = 5) {
  sub <- ple %>% filter(iso == iso_code, !is.na(fstgdp))
  pre_yrs  <- sub %>% filter(year < start) %>% nrow()
  post_yrs <- sub %>% filter(year >= start, year <= end + 15) %>% nrow()
  in_ple   <- iso_code %in% ple$iso
  list(in_ple = in_ple, pre_yrs = pre_yrs, post_yrs = post_yrs,
       ok = in_ple & pre_yrs >= min_pre & post_yrs >= min_post)
}

results <- auth %>%
  rowwise() %>%
  mutate(
    cov = list(check_coverage(iso, start_yr, end_yr)),
    in_ple   = cov$in_ple,
    pre_yrs  = cov$pre_yrs,
    post_yrs = cov$post_yrs,
    scm_ok   = cov$ok
  ) %>%
  select(-cov) %>%
  ungroup()

cat("=== Data coverage summary ===\n")
cat("Episodes with sufficient data (>=5 pre, >=5 post):", sum(results$scm_ok), "\n")
cat("Episodes not in PLE panel:", sum(!results$in_ple), "\n\n")

cat("--- Episodes NOT suitable for SCM (insufficient data) ---\n")
print(results %>%
        filter(!scm_ok) %>%
        select(country, leader, start_yr, end_yr, in_ple, pre_yrs, post_yrs, auth_type),
      n = 50)

cat("\n--- SCM-viable episodes (", sum(results$scm_ok), " total) ---\n")
print(results %>%
        filter(scm_ok) %>%
        select(country, leader, start_yr, end_yr, tenure_yrs, auth_type, is_also_populist),
      n = 100)

# Save viable list
results %>%
  filter(scm_ok) %>%
  mutate(is_also_populist = as.integer(is_also_populist)) %>%
  write_csv("C:/PLE/data/authoritarian_episodes_scm_viable.csv")

cat("\nSaved SCM-viable list to data/authoritarian_episodes_scm_viable.csv\n")
