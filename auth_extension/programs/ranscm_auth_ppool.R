# ranscm_auth_ppool.R
# Authoritarian Leaders Extension — Partially Pooled Synthetic Controls
# Analogue of FST (2023) Figure 10 for authoritarian leaders
#
# Uses augsynth::multisynth for partially pooled SC estimates.
# Builds a pseudo-panel where each episode is a unique "unit" (country_year).
# One unit per episode; all other country-years serve as donors.
#
# Run from: C:\PLE\auth_extension\programs\
# Outputs:
#   figures/FigureA_10.pdf  — Partially pooled SC GDP paths

renv::restore(prompt = FALSE)
devtools::install_github('ebenmichael/augsynth', upgrade = "never")

library(haven)
library(data.table)
library(dplyr)
library(patchwork)
library(ggplot2)
library(readr)
library(augsynth)

suppressPackageStartupMessages(library(plyr))

rm(list = ls(all = TRUE))
setwd('..')

auth_data <- read_dta("data/auth_dataset.dta")
auth_data$lgfstgdp <- log(auth_data$fstgdp)

episodes <- read_csv("data/authoritarian_episodes_scm_viable.csv",
                     show_col_types = FALSE)
episodes <- episodes %>%
  filter(is_also_populist == 0) %>%
  filter(!is.na(cid))

type_sp <- "Single-party"
type_mi <- c("Military", "Military-personal")

cat("Episodes:", nrow(episodes), "\n")

try({

  # ============================================================
  # Build pseudo-panel: each episode = unique country string
  # ============================================================

  base <- auth_data[, c("cid", "country", "year", "lgfstgdp")]

  # For episodes with same country, multiple spells, create suffixed clones
  pseudo_list <- list()
  pseudo_list[[1]] <- base  # all actual country-years

  treat_df <- data.frame(country_label = character(),
                         treat_year    = integer(),
                         atype         = character(),
                         stringsAsFactors = FALSE)

  for (k in seq_len(nrow(episodes))) {
    ep    <- episodes[k, ]
    Oldc  <- ep$cid
    Year  <- ep$start_yr
    iso   <- ep$iso
    atype_raw <- ep$auth_type
    auth_group <- if (atype_raw %in% type_sp) "Single-party" else
                  if (atype_raw %in% type_mi) "Military"      else
                  "Personalist"

    label <- paste0(iso, "_", Year)
    country_rows <- base[base$cid == Oldc, ]
    country_rows$country <- label
    pseudo_list[[length(pseudo_list) + 1]] <- country_rows

    treat_df <- rbind(treat_df,
      data.frame(country_label = label, treat_year = Year,
                 atype = auth_group, stringsAsFactors = FALSE))
  }

  pseudo <- rbindlist(pseudo_list, fill = TRUE) %>%
    mutate(treatedyear = 0)

  # Mark treatment: each cloned unit is treated from its episode year onward
  for (k in seq_len(nrow(treat_df))) {
    lab  <- treat_df$country_label[k]
    yr   <- treat_df$treat_year[k]
    pseudo$treatedyear[pseudo$country == lab & pseudo$year >= yr] <- 1
  }

  # Drop rows with missing lgfstgdp
  pseudo <- pseudo %>% filter(!is.na(lgfstgdp))

  cat("Pseudo-panel rows:", nrow(pseudo), "\n")
  cat("Unique units:", n_distinct(pseudo$country), "\n")

  # ============================================================
  # Run multisynth
  # ============================================================

  cat("Running multisynth (may take 30-60 minutes)...\n")
  ppool_syn <- tryCatch(
    multisynth(lgfstgdp ~ treatedyear, country, year, pseudo,
               fixedeff = TRUE, alpha = 0.1,
               n_lags = 15, n_leads = 16, nu = 0.5),
    error = function(e) { cat("multisynth error:", e$message, "\n"); NULL }
  )

  if (is.null(ppool_syn)) stop("multisynth failed")

  ppool_sum <- summary(ppool_syn)
  ppool <- data.frame(ppool_sum$att)
  cat("ppool cols:", paste(names(ppool), collapse = ", "), "\n")
  cat("ppool rows (raw):", nrow(ppool), "\n")
  saveRDS(ppool, file.path("data", "_work", "ppool_att.rds"))

  # Normalise time column name (augsynth may use 'Time' or 'time')
  if (!"Time" %in% names(ppool) && "time" %in% names(ppool))
    names(ppool)[names(ppool) == "time"] <- "Time"

  ppool <- ppool[!is.na(ppool$Time) & !is.na(ppool$Estimate), ]
  ppool <- ppool %>% filter(Time >= -15, Time <= 15)
  ppool <- ppool[ppool$Level != "Average", ]
  cat("ppool rows (filtered):", nrow(ppool), "\n")
  cat("Level sample:", paste(head(ppool$Level, 3), collapse = ", "), "\n")
  cat("treat_df label sample:", paste(head(treat_df$country_label, 3), collapse = ", "), "\n")

  # Join auth_group
  ppool$auth_group <- treat_df$atype[match(ppool$Level, treat_df$country_label)]
  cat("auth_group NA count:", sum(is.na(ppool$auth_group)), "/", nrow(ppool), "\n")

  ppool$all <- 1
  ppool$sp  <- as.integer(ppool$auth_group == "Single-party")
  ppool$mi  <- as.integer(ppool$auth_group == "Military")
  ppool$pe  <- as.integer(ppool$auth_group == "Personalist")

  make_avg <- function(df, grp) {
    df[!is.na(df[[grp]]) & df[[grp]] == 1, ] %>%
      group_by(Time) %>%
      dplyr::summarise(Estimate = mean(Estimate,     na.rm = TRUE),
                       lower    = mean(lower_bound,  na.rm = TRUE),
                       upper    = mean(upper_bound,  na.rm = TRUE),
                       .groups = "drop")
  }

  dp_all <- make_avg(ppool, "all")
  dp_sp  <- make_avg(ppool, "sp")
  dp_mi  <- make_avg(ppool, "mi")
  dp_pe  <- make_avg(ppool, "pe")

  if (.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times = windowsFont("Times New Roman"))
  })

  base_theme <- theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title   = element_text(hjust = 0.5, size = 10),
          aspect.ratio = 3/4.25,
          plot.margin  = unit(c(0.06, 0.06, 0.06, 0.06), "cm"),
          panel.border = element_rect(linewidth = 0.3),
          axis.ticks   = element_line(linewidth = 0.3),
          legend.position = "bottom", legend.text = element_text(size = 7),
          legend.margin = margin(-18, 5, 0, -5),
          text = element_text(family = "Times"), axis.text = element_text(size = 6))

  make_ppool_plot <- function(dp, title_str) {
    ggplot(dp) +
      geom_ribbon(aes(ymin = lower, ymax = upper, x = Time,
                      fill = "90% CI"), alpha = 0.3) +
      geom_line(aes(y = Estimate, x = Time,
                    colour = "Authoritarian avg.",
                    linetype = "Authoritarian avg.",
                    linewidth = "Authoritarian avg.")) +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2) +
      scale_colour_manual(name = '',
        values = c("Authoritarian avg." = "blue")) +
      scale_fill_manual(name = '',
        values = c("90% CI" = "steelblue")) +
      scale_linetype_manual(name = '',
        values = c("Authoritarian avg." = "solid")) +
      scale_linewidth_manual(name = '',
        values = c("Authoritarian avg." = 0.4)) +
      scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
      scale_y_continuous(limits = c(-0.40, 0.60),
                         breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.60),
                         labels = c("-40%", "-20%", "0%", "+20%", "+40%", "+60%"),
                         expand = c(0.02, 0.02)) +
      labs(title = title_str, x = "", y = "") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2) +
      guides(color = guide_legend(ncol = 1, nrow = 2, keyheight = 0.7,
                                  override.aes = list(fill = NA))) +
      base_theme
  }

  p_all <- make_ppool_plot(dp_all, "All authoritarians")
  p_sp  <- make_ppool_plot(dp_sp,  "Single-party")
  p_mi  <- make_ppool_plot(dp_mi,  "Military")
  p_pe  <- make_ppool_plot(dp_pe,  "Personalist")

  pdf(file.path("figures", "FigureA_10.pdf"), width = 18 / 2.54, height = 6 / 2.54)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
  print(p_sp, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_mi, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_pe, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
  dev.off()
  cat("FigureA_10.pdf saved.\n")

}, silent = FALSE)

cat("ranscm_auth_ppool.R complete.\n")
