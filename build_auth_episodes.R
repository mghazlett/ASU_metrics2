library(dplyr)
library(readr)

df <- read_csv("C:/PLE/data/authoritarian_episodes_final.csv", show_col_types = FALSE)

# -----------------------------------------------------------------------
# Step 1: Resolve ONLY true duplicates — rows for the same leader where
# date ranges overlap (Archigos entry + manually extended entry).
# Non-consecutive separate tenures (e.g., Velasco Ibarra's 5 presidencies)
# are kept as distinct episodes.
# -----------------------------------------------------------------------
df <- df %>%
  group_by(country, leader) %>%
  arrange(start_yr) %>%
  mutate(
    # Does this row overlap with the next row for the same leader?
    next_start = lead(start_yr),
    next_end   = lead(end_yr),
    overlaps_next = !is.na(next_start) & next_start <= end_yr
  ) %>%
  ungroup()

# Identify overlap groups: rows that overlap with the next one should be
# merged with it. Strategy: flag the "base" row of each overlap pair and
# drop the shorter row, merging its gwf_type and dates into the base.
# Only handle pairwise overlaps (all real duplicates are exactly 2 rows).
df_merged <- df %>%
  group_by(country, leader) %>%
  arrange(start_yr) %>%
  mutate(
    group_id = cumsum(lag(overlaps_next, default = FALSE) == FALSE)
  ) %>%
  group_by(country, leader, group_id) %>%
  summarise(
    iso              = first(iso),
    cid              = first(cid),
    cowcode          = first(cowcode),
    start_yr         = min(start_yr),
    end_yr           = max(end_yr),
    tenure_yrs       = max(end_yr) - min(start_yr),
    regime_vdem_type = last(regime_vdem_type),
    gwf_type         = first(gwf_type[!is.na(gwf_type)]),
    auth_yrs_vdem    = suppressWarnings(max(auth_yrs_vdem, na.rm = TRUE)),
    is_also_populist = any(is_also_populist),
    .groups = "drop"
  ) %>%
  select(-group_id)

# -----------------------------------------------------------------------
# Step 2: Remove Western-democracy countries
# -----------------------------------------------------------------------
exclude_countries <- c(
  "United States", "United Kingdom", "Canada",
  "Netherlands", "Sweden", "Ireland", "Belgium", "Norway"
)
df_merged <- df_merged %>% filter(!country %in% exclude_countries)

# -----------------------------------------------------------------------
# Step 3: Remove occupation governments, post-dictatorship democrats,
# and other clear false positives
# -----------------------------------------------------------------------
remove_specific <- data.frame(
  country = c(
    "France", "France", "France",
    "Japan",
    "Brazil", "Brazil",
    "Italy",
    "Peru", "Philippines",
    "Spain",
    "India"
  ),
  leader = c(
    "Petain", "Laval", "De Gaulle",
    "Douglas MacArthur",
    "Dutra", "Sarnay",
    "de Gasperi",
    "Belaunde", "Aquino",
    "Suarez Gonzalez",
    "Nehru"
  ),
  stringsAsFactors = FALSE
)
df_merged <- df_merged %>% anti_join(remove_specific, by = c("country", "leader"))

# -----------------------------------------------------------------------
# Step 4: Substantive authoritarianism filter
# -----------------------------------------------------------------------
df_auth <- df_merged %>%
  filter(
    regime_vdem_type == "Closed autocracy" |
    (!is.na(gwf_type) & grepl("personal|military", gwf_type, ignore.case = TRUE))
  )

# -----------------------------------------------------------------------
# Step 5: Minimum tenure >= 5 years per episode
# -----------------------------------------------------------------------
df_auth <- df_auth %>% filter(tenure_yrs >= 5)

# -----------------------------------------------------------------------
# Step 6: Assign readable authoritarian type label
# -----------------------------------------------------------------------
df_auth <- df_auth %>%
  mutate(
    auth_type = case_when(
      grepl("personal", gwf_type, ignore.case = TRUE) &
        grepl("military", gwf_type, ignore.case = TRUE) ~ "Military-personal",
      grepl("personal", gwf_type, ignore.case = TRUE)  ~ "Personalist",
      grepl("military", gwf_type, ignore.case = TRUE)  ~ "Military",
      grepl("party",    gwf_type, ignore.case = TRUE)  ~ "Single-party",
      grepl("monarchy|monarch", gwf_type, ignore.case = TRUE) ~ "Monarchy",
      grepl("oligarch", gwf_type, ignore.case = TRUE)  ~ "Oligarchy",
      regime_vdem_type == "Closed autocracy"            ~ "Closed (unclassified)",
      TRUE ~ NA_character_
    )
  )

# -----------------------------------------------------------------------
# Step 7: Final sort and save
# -----------------------------------------------------------------------
df_auth <- df_auth %>%
  arrange(country, start_yr) %>%
  select(country, iso, cid, cowcode, leader, start_yr, end_yr, tenure_yrs,
         auth_type, gwf_type, regime_vdem_type, is_also_populist)

cat("=== Final Authoritarian Episodes ===\n")
cat("Total episodes:", nrow(df_auth), "\n")
cat("Countries:", n_distinct(df_auth$country), "\n\n")

cat("--- By authoritarian type ---\n")
print(table(df_auth$auth_type))

cat("\n--- Overlap with FST populism paper (", sum(df_auth$is_also_populist), "episodes) ---\n")
print(df_auth %>%
        filter(is_also_populist) %>%
        select(country, leader, start_yr, end_yr, tenure_yrs, auth_type))

cat("\n--- Full episode list ---\n")
print(
  df_auth %>% select(country, leader, start_yr, end_yr, tenure_yrs, auth_type, is_also_populist),
  n = 200
)

# Write is_also_populist as 0/1 so Stata reads it as numeric
df_auth <- df_auth %>% mutate(is_also_populist = as.integer(is_also_populist))
write_csv(df_auth, "C:/PLE/data/authoritarian_episodes_refined.csv")
cat("\nSaved: C:/PLE/data/authoritarian_episodes_refined.csv\n")
cat("Episodes:", nrow(df_auth), "| Countries:", n_distinct(df_auth$country), "\n")
