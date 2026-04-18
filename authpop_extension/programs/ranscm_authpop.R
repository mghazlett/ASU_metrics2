# ranscm_authpop.R
# Authoritarian-Populist Subset Extension — Synthetic Control Estimation
#
# Replicates FST (2023) ranscm.R logic on six subsets drawn from the
# original 28 populist episodes (plus Hungary/Orbán added to broad):
#
#   strict           — 9 episodes in electoral/closed autocracies at takeover
#   broad            — 14 episodes (strict + became authoritarian during tenure + HUN)
#   strict_noecuador — strict minus 3 Ecuador episodes (robustness check)
#   broad_noecuador  — broad minus 3 Ecuador episodes (robustness check)
#   nonauthpop       — 15 non-authoritarian populist episodes (pure_pop)
#   fst_full         — all 28 original FST episodes (authpop + nonauthpop, excl. HUN)
#
# All SCM parameters are identical to FST ranscm.R.
# Episode metadata (oid, nid, fr1/fr2/fr3) are read from authpop_episodes.csv.
#
# Run from: C:\PLE\authpop_extension\programs\
# (script does setwd('..') to reach authpop_extension/ root)
#
# Inputs:
#   ../data/ple_dataset.dta       — FST master panel (unchanged)
#   data/authpop_episodes.csv     — episode classification list
#
# Outputs (in figures/):
#   FigureAP6_*.pdf  — main SCM, 15-year post window (6 subsets)
#   FigureAP7_*.pdf  — main SCM, sims=10 robustness (6 subsets)
#   FigureAP8_*.pdf  — main SCM, 5-year post window (6 subsets)
#   FigureAP9.pdf    — three-group comparison: FST full / authpop broad / pure-pop
#   FigureAP12_*.pdf through FigureAP14_*.pdf — alternative outcomes (4 subsets)
#
# Pre-period note: ep_period_pre = seq(0, fr3, 1) is episode-specific.
# Early episodes (BOL/CHL/ECU 1952, BRA 1951) have fr3 < 15 due to
# limited pre-treatment GDP data; using episode-specific pre-period
# avoids a period overlap bug that caused those episodes to fail silently.

# ============================================================
# Setup
# ============================================================

renv::restore(prompt = FALSE)
devtools::install_github('ebenmichael/augsynth', upgrade = "never")
install.packages("MASS", repos = "http://lib.stat.cmu.edu/R/CRAN", ask = FALSE)

library(LowRankQP)
library(devtools)
library(zoo)
library(haven)
library(data.table)
library(scpi)
library(augsynth)
library(gsynth)
library(plyr)
library(purrr)
library(dplyr)
library(patchwork)
library(tidyr)
library(reshape2)
library(ggplot2)
library(tibble)
library(CVXR)
library(Rmpfr)
library(readr)

rm(list = ls(all = TRUE))
setwd('..')   # authpop_extension/ is the working root

# ============================================================
# Load data
# ============================================================

ple_data <- read_dta("../data/ple_dataset.dta")
episodes <- read_csv("data/authpop_episodes.csv", show_col_types = FALSE)

cat("Episodes loaded:", nrow(episodes), "\n")
cat("  auth_strict:", sum(episodes$auth_strict), "\n")
cat("  auth_broad: ", sum(episodes$auth_broad),  "\n")

# ============================================================
# SCM parameters — identical to FST ranscm.R
# ============================================================

cov.adj           <- NULL
features          <- NULL
constant          <- FALSE
rho               <- 'type-1'
rho.max           <- 1
u.order           <- 0
e.order           <- 0
u.lags            <- 0
e.lags            <- 0
u.sigma           <- "HC1"
e.sigma           <- "HC1"
u.missp           <- TRUE
u.alpha           <- 0.1
e.alpha           <- 0.1
cointegrated.data <- TRUE
cores             <- 1
sims              <- 200
e.method          <- "gaussian"
w.constr          <- list(lb = 0, dir = "==", p = "L1", Q = 1)
sta               <- 15

period.pre  <- seq(from = 0, to = 15, by = 1)
period.post <- 16:30

if (.Platform$OS.type == "windows") {
  windowsFonts(Times = windowsFont("Times New Roman"))
}

# ============================================================
# Soft-fail CI extractor (mirrors ranscm_auth.R)
# ============================================================

extract_ci <- function(result) {
  tryCatch(
    list(
      scl.gauss    = result$inference.results$CI.all.gaussian[, 1, drop = FALSE],
      scr.gauss    = result$inference.results$CI.all.gaussian[, 2, drop = FALSE],
      scl.insample = result$inference.results$CI.in.sample[,  1, drop = FALSE],
      scr.insample = result$inference.results$CI.in.sample[,  2, drop = FALSE]
    ),
    error = function(e) NULL
  )
}

# ============================================================
# SCM loop: run all episodes in ep_subset, return finaldata
# ============================================================

run_scm_subset <- function(ep_subset, subset_label, post_years = 15) {

  n_ep <- nrow(ep_subset)
  cat("\n--- Running subset:", subset_label, "(N =", n_ep, ", post_years =", post_years, ") ---\n")

  all_series <- list()

  for (k in seq_len(n_ep)) {

    ep   <- ep_subset[k, ]
    Oldc <- ep$oid
    Year <- ep$year
    Left <- ep$left
    Case <- paste(ep$nid, ep$year, sep = ".")
    fr1  <- ep$fr1
    fr2  <- ep$fr2
    fr3  <- ep$fr3
    fr4  <- if ("fr4" %in% names(ep) && !is.na(ep$fr4)) ep$fr4 else 30
    # Episode-specific pre-period: ends at fr3 (not hardcoded 15)
    # Fixes overlap bug for early episodes (BOL/CHL/ECU 1952, BRA 1951) where fr3 < 15
    ep_period_pre  <- seq(0, fr3, 1)
    # Episode-specific post window: post_years determines horizon (5 or 15)
    ep_fr4_run     <- min(fr3 + post_years, fr4)   # cap at data availability
    ep_period_post <- (fr3 + 1):ep_fr4_run

    cat("  [", k, "/", n_ep, "]", ep$iso, Year, ep$leader, "\n")

    try({

      # --- Build data window ---
      data <- ple_data
      data <- data[data$year >= Year - sta & data$year <= Year + 15, ]

      # Treated country
      taker <- data %>% filter(cid == Oldc)

      # Donors: exclude countries with a populist takeover in the same year
      # (uses original atakeover from ple_dataset — identical to FST logic)
      donors <- data %>% filter(cid != Oldc)
      donors <- donors %>%
        mutate(simul  = ifelse(atakeover == 1 & year == Year, 1, 0)) %>%
        group_by(cid) %>%
        mutate(msimul = max(as.numeric(simul))) %>%
        filter(msimul != 1) %>%
        select(-simul, -msimul)

      data <- rbind(taker, donors)

      # Drop donor countries with any missing GDP in this window
      data <- data %>%
        group_by(cid) %>%
        filter(all(!is.na(fstgdp) | cid == Oldc))

      data$lgfstgdp <- log(data$fstgdp)

      # Index to takeover year (identical to FST)
      tysub <- data[data$year == Year, c("cid", "country", "lgfstgdp")]
      names(tysub)[names(tysub) == "lgfstgdp"] <- "ilgfstgdp"
      data  <- merge(data, tysub)
      data  <- data %>%
        group_by(cid) %>%
        mutate(d = lgfstgdp - ilgfstgdp, t = year - Year + 15)
      data  <- transform(data, index = as.numeric(factor(country)))
      data  <- data %>% mutate(d = replace(d, war == 1, NA))

      # Dynamic treated-unit index (robust to varying alphabetical ordering)
      Trea <- data$index[data$cid == Oldc][1]

      # --- Fit SCM ---
      df <- scdata(
        df = data, features = features, constant = constant,
        cov.adj = cov.adj, cointegrated.data = cointegrated.data,
        id.var = "index", time.var = "t", outcome.var = "d",
        period.pre = ep_period_pre, period.post = ep_period_post,
        unit.tr = Trea, unit.co = unique(data$index)[-Trea]
      )

      result <- scpi(
        data = df,
        u.order = u.order, u.lags = u.lags, u.sigma = u.sigma,
        u.missp = u.missp, e.order = e.order, e.lags = e.lags,
        u.alpha = u.alpha, e.alpha = e.alpha,
        rho = rho, rho.max = rho.max, sims = sims,
        w.constr = w.constr, cores = cores, e.method = e.method
      )

      # --- Extract results ---
      y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
      yfit  <- data.frame(t = c(fr1:fr2, fr3:ep_fr4_run), yfit = c(y.fit))

      y.act <- rbind(result$data$Y.pre, result$data$Y.post)
      yact  <- data.frame(
        t    = c(ep_period_pre, ep_period_post),
        yact = c(y.act),
        case = Case,
        left = Left
      )

      ys <- merge(yact, yfit, by = "t", all = TRUE)

      ci <- extract_ci(result)
      if (!is.null(ci)) {
        cis <- data.frame(
          t            = ep_period_post,
          sclinsample  = c(ci$scl.insample),
          scrinsample  = c(ci$scr.insample),
          sclgauss     = c(ci$scl.gauss),
          scrgauss     = c(ci$scr.gauss)
        )
      } else {
        cis <- data.frame(
          t            = ep_period_post,
          sclinsample  = NA_real_,
          scrinsample  = NA_real_,
          sclgauss     = NA_real_,
          scrgauss     = NA_real_
        )
      }

      series <- merge(ys, cis, by = "t", all = TRUE)
      all_series[[k]] <- series
      cat("    OK\n")

    })
  }

  all_series <- Filter(Negate(is.null), all_series)
  n_converged <- length(all_series)
  cat("  Converged:", n_converged, "/", n_ep, "\n")

  if (n_converged == 0) {
    cat("  No converged episodes — skipping figure.\n")
    return(NULL)
  }

  finaldata <- rbindlist(all_series, fill = TRUE)
  finaldata <- finaldata %>% group_by(case) %>% mutate(ti = t - 15)

  # Zero out CI at event year (convention from FST)
  for (v in c("sclinsample", "scrinsample", "sclgauss", "scrgauss")) {
    finaldata[[v]][finaldata$ti == 0] <- 0
  }

  finaldata$all   <- 1
  finaldata$right <- ifelse(finaldata$left == 1, 0, 1)

  finaldata
}

# ============================================================
# Aggregation helpers (mirrors FST ranscm.R)
# ============================================================

make_avg <- function(fd, group_col, suffix) {
  # Pre-filter before ddply to avoid get() evaluation issues with plyr/dplyr masking
  g <- fd[fd[[group_col]] == 1, ]
  if (nrow(g) == 0) {
    # Return a data frame of NAs so downstream plotting handles missing groups gracefully
    ti_vals <- sort(unique(fd$ti))
    result <- data.frame(ti = ti_vals)
    for (v in c("yfit_","yact_","scrinsample_","sclinsample_","scrgauss_","sclgauss_")) {
      result[[paste0(v, suffix)]] <- NA_real_
    }
    return(result)
  }
  Reduce(
    function(x, y) merge(x, y, all = TRUE),
    list(
      ddply(g, .(ti), summarise, yfit        = mean(yfit,        na.rm = TRUE)),
      ddply(g, .(ti), summarise, yact        = mean(yact,        na.rm = TRUE)),
      ddply(g, .(ti), summarise, scrinsample = mean(scrinsample, na.rm = TRUE)),
      ddply(g, .(ti), summarise, sclinsample = mean(sclinsample, na.rm = TRUE)),
      ddply(g, .(ti), summarise, scrgauss    = mean(scrgauss,    na.rm = TRUE)),
      ddply(g, .(ti), summarise, sclgauss    = mean(sclgauss,    na.rm = TRUE))
    )
  ) %>%
    setNames(c("ti",
               paste0("yfit_",        suffix),
               paste0("yact_",        suffix),
               paste0("scrinsample_", suffix),
               paste0("sclinsample_", suffix),
               paste0("scrgauss_",    suffix),
               paste0("sclgauss_",    suffix)))
}

# ============================================================
# Single-panel plot builders (trends / gap)
# ============================================================

# Shared theme — identical to FST ranscm.R
scm_theme <- function() {
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(hjust = 0.5, vjust = 0, size = 10),
      aspect.ratio     = 3 / 4.25,
      plot.margin      = unit(c(0.06, 0.06, 0.06, 0.06), "cm"),
      panel.border     = element_rect(size = 0.3),
      axis.ticks       = element_line(size = 0.3),
      legend.position  = "bottom",
      legend.text      = element_text(size = 7),
      legend.margin    = margin(-18, +5, 0, -5),
      text             = element_text(family = "Times"),
      axis.text        = element_text(size = 6)
    )
}

make_trends_panel <- function(df, suffix, col, title_str) {
  yfit_v        <- paste0("yfit_",        suffix)
  yact_v        <- paste0("yact_",        suffix)
  scrgauss_v    <- paste0("scrgauss_",    suffix)
  sclgauss_v    <- paste0("sclgauss_",    suffix)

  # Count non-NA actual observations for N label
  n_obs <- sum(!is.na(df[[yact_v]]) & df$ti == 1)

  ggplot(df) +
    geom_ribbon(aes(
      ymin  = .data[[sclgauss_v]],
      ymax  = .data[[scrgauss_v]],
      x     = ti,
      fill  = "90% CI (out-of-sample uncertainty)"
    ), alpha = 1) +
    geom_line(aes(x = ti, y = .data[[sclgauss_v]],
                  color   = "90% CI (out-of-sample uncertainty)",
                  linetype = "90% CI (out-of-sample uncertainty)",
                  size    = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x = ti, y = .data[[scrgauss_v]],
                  color   = "90% CI (out-of-sample uncertainty)",
                  linetype = "90% CI (out-of-sample uncertainty)",
                  size    = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y = .data[[yfit_v]], x = ti,
                  colour  = "Doppelganger avg.",
                  fill    = "Doppelganger avg.",
                  linetype = "Doppelganger avg.",
                  size    = "Doppelganger avg.")) +
    geom_line(aes(y = .data[[yact_v]], x = ti,
                  colour  = "Populist avg.",
                  fill    = "Populist avg.",
                  linetype = "Populist avg.",
                  size    = "Populist avg.")) +
    scale_colour_manual(name = '', values = c(
      "Populist avg."                        = col,
      "Doppelganger avg."                    = col,
      "90% CI (out-of-sample uncertainty)"   = "grey95"
    )) +
    scale_fill_manual(name = '', values = c(
      "Populist avg."                        = col,
      "Doppelganger avg."                    = col,
      "90% CI (out-of-sample uncertainty)"   = "grey95"
    )) +
    scale_linetype_manual(name = '', values = c(
      "Populist avg."                        = "solid",
      "Doppelganger avg."                    = "longdash",
      "90% CI (out-of-sample uncertainty)"   = "solid"
    )) +
    scale_size_manual(name = '', values = c(
      "Populist avg."                        = 0.4,
      "Doppelganger avg."                    = 0.4,
      "90% CI (out-of-sample uncertainty)"   = 0.4
    )) +
    scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
    scale_y_continuous(
      limits = c(-0.40, 0.60),
      breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.60),
      labels = c("-40%", "-20%", "0%", "+20%", "+40%", "+60%"),
      expand = c(0.02, 0.02)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    labs(title = title_str, x = "", y = "") +
    guides(color = guide_legend(ncol = 1, nrow = 3, keyheight = 0.7,
                                override.aes = list(fill = NA))) +
    scm_theme()
}

make_gap_panel <- function(df, suffix, col, title_str) {
  yfit_v     <- paste0("yfit_",     suffix)
  yact_v     <- paste0("yact_",     suffix)
  scrgauss_v <- paste0("scrgauss_", suffix)
  sclgauss_v <- paste0("sclgauss_", suffix)

  ggplot(df) +
    geom_ribbon(aes(
      ymin  = (.data[[sclgauss_v]] - .data[[yact_v]]) * (-1),
      ymax  = (.data[[scrgauss_v]] - .data[[yact_v]]) * (-1),
      x     = ti,
      fill  = "90% CI (out-of-sample uncertainty)"
    ), alpha = 1) +
    geom_line(aes(
      x     = ti,
      y     = (.data[[sclgauss_v]] - .data[[yact_v]]) * (-1),
      color = "90% CI (out-of-sample uncertainty)",
      size  = "90% CI (out-of-sample uncertainty)"
    )) +
    geom_line(aes(
      x     = ti,
      y     = (.data[[scrgauss_v]] - .data[[yact_v]]) * (-1),
      color = "90% CI (out-of-sample uncertainty)",
      size  = "90% CI (out-of-sample uncertainty)"
    )) +
    geom_line(aes(
      y     = (.data[[yfit_v]] - .data[[yact_v]]) * (-1),
      x     = ti,
      colour = "Doppelganger gap (avg.)",
      fill  = "Doppelganger gap (avg.)",
      size  = "Doppelganger gap (avg.)"
    )) +
    scale_colour_manual(name = '', values = c(
      "Doppelganger gap (avg.)"              = col,
      "90% CI (out-of-sample uncertainty)"   = "grey95"
    )) +
    scale_fill_manual(name = '', values = c(
      "Doppelganger gap (avg.)"              = col,
      "90% CI (out-of-sample uncertainty)"   = "grey95"
    )) +
    scale_size_manual(name = '', values = c(
      "Doppelganger gap (avg.)"              = 0.4,
      "90% CI (out-of-sample uncertainty)"   = 0.4
    )) +
    scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
    scale_y_continuous(
      limits = c(-0.30, 0.10),
      breaks = c(-0.30, -0.25, -0.20, -0.15, -0.10, -0.05, 0, 0.05, 0.10),
      labels = c("-30%", "-25%", "-20%", "-15%", "-10%", "-5%", "0%", "+5%", "+10%"),
      expand = c(0.02, 0.02)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
    labs(title = title_str, x = "", y = "") +
    guides(color = guide_legend(ncol = 1, nrow = 2, keyheight = 0.7,
                                override.aes = list(fill = NA))) +
    scm_theme()
}

# ============================================================
# Save figure for one subset using grid::grid.layout
# (avoids patchwork & operator which triggers == data frame error)
# ============================================================

save_subset_figure <- function(outpath, finaldata) {
  n_all   <- length(unique(finaldata$case))
  n_left  <- length(unique(finaldata$case[finaldata$left  == 1]))
  n_right <- length(unique(finaldata$case[finaldata$right == 1]))

  dfp_all   <- make_avg(finaldata, "all",   "all")
  dfp_left  <- make_avg(finaldata, "left",  "left")
  dfp_right <- make_avg(finaldata, "right", "right")

  title_all   <- paste0("All auth-populists (N=", n_all,   ")")
  title_left  <- paste0("Left-wing (N=",          n_left,  ")")
  title_right <- paste0("Right-wing (N=",         n_right, ")")

  p_trends_all   <- make_trends_panel(dfp_all,   "all",   "blue",    title_all)
  p_trends_left  <- make_trends_panel(dfp_left,  "left",  "darkred", title_left)
  p_trends_right <- make_trends_panel(dfp_right, "right", "black",   title_right)
  p_gap_all      <- make_gap_panel(dfp_all,   "all",   "blue",    title_all)
  p_gap_left     <- make_gap_panel(dfp_left,  "left",  "darkred", title_left)
  p_gap_right    <- make_gap_panel(dfp_right, "right", "black",   title_right)

  # 2-row x 3-col grid: row 1 = trends, row 2 = gap
  pdf(outpath, width = 23 / 2.54, height = 16 / 2.54)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 3)))
  print(p_trends_all,   vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_trends_left,  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_trends_right, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
  print(p_gap_all,      vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_gap_left,     vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(p_gap_right,    vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 3))
  dev.off()
}

# ============================================================
# Define the four subsets and run
# ============================================================

subsets <- list(
  strict           = filter(episodes, auth_strict == 1),
  broad            = filter(episodes, auth_broad  == 1),
  strict_noecuador = filter(episodes, auth_strict == 1, iso != "ECU"),
  broad_noecuador  = filter(episodes, auth_broad  == 1, iso != "ECU"),
  nonauthpop       = filter(episodes, auth_broad  == 0),
  fst_full         = filter(episodes, !(iso == "HUN" & year == 2010))
)

# Store AP6 finaldata for comparison figure AP9
finaldata_ap6 <- list()

for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\n========================================\n")
  cat("Subset:", sname, "| Episodes:", nrow(ep_sub), "\n")
  cat("Countries:", paste(ep_sub$iso, ep_sub$year, sep = "_", collapse = ", "), "\n")
  cat("========================================\n")

  finaldata <- run_scm_subset(ep_sub, sname)

  if (is.null(finaldata)) {
    cat("SKIPPED (no converged episodes)\n")
    next
  }

  # Store for AP9 comparison figure
  finaldata_ap6[[sname]] <- finaldata

  # Save finaldata as CSV for diagnostics
  write_csv(finaldata, file.path("data", paste0("scm_results_", sname, ".csv")))

  # Build and save figure using grid layout
  outpath <- file.path("figures", paste0("FigureAP6_", sname, ".pdf"))
  tryCatch({
    save_subset_figure(outpath, finaldata)
    fsize <- file.size(outpath)
    cat("Saved:", outpath, "(", fsize, "bytes)\n")
  }, error = function(e) {
    cat("Figure error for", sname, ":", conditionMessage(e), "\n")
  })
}

cat("\n=== Figure AP6 complete ===\n")

# ============================================================
# FIGURE AP7 — Main SCM, sims = 10 (quick robustness check)
# Same as Figure AP6 but sims=10 to verify results hold with
# fewer simulation draws. Mirrors FST Figure 7.
# ============================================================

cat("\n\n========== FIGURE AP7 (sims=10) ==========\n")

sims_saved <- sims
sims       <- 10

for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\nSubset:", sname, "(N =", nrow(ep_sub), ")\n")

  finaldata <- run_scm_subset(ep_sub, paste0(sname, "_fig7"))

  if (is.null(finaldata)) { cat("SKIPPED\n"); next }

  outpath <- file.path("figures", paste0("FigureAP7_", sname, ".pdf"))
  tryCatch({
    save_subset_figure(outpath, finaldata)
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

sims <- sims_saved   # restore

# ============================================================
# FIGURE AP8 — Main SCM, 5-year post window
# Motivated by Dornbusch & Edwards (1991): economic damage from
# populist macro policy typically peaks in years 3-5. Authpop
# strict leaders also have a median spell of 5 years, making
# this the most policy-relevant horizon for that subset.
# Same parameters as AP6 but ep_period_post truncated to 5 years.
# ============================================================

cat("\n\n========== FIGURE AP8 (5-year SCM) ==========\n")

for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\nSubset:", sname, "(N =", nrow(ep_sub), ")\n")

  finaldata <- run_scm_subset(ep_sub, paste0(sname, "_5yr"), post_years = 5)

  if (is.null(finaldata)) { cat("SKIPPED\n"); next }

  outpath <- file.path("figures", paste0("FigureAP8_", sname, ".pdf"))
  tryCatch({
    save_subset_figure(outpath, finaldata)
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

cat("\n=== Figure AP8 complete ===\n")

# ============================================================
# ============================================================
# FIGURE AP9 — Four-group comparison (body figure)
#
# Main paper figure: plots average GDP gap (actual − doppelganger) for
# four groups simultaneously. Each group's doppelganger is estimated
# independently. Normalized to 0 at takeover year (ti = 0).
# Groups:
#   strict     (darkred / solid)    — 9 strict authpop episodes
#   broad      (red / longdash)     — 14 broad authpop episodes
#   nonauthpop (steelblue / dotdash)— 15 non-authoritarian populist episodes
#   fst_full   (grey50 / twodash)   — all 28 original FST episodes
# ============================================================

# Helper: build comparison figure from a named list of group specs
build_ap9_figure <- function(group_specs, title_str) {
  gap_list <- list()
  for (spec in group_specs) {
    fd <- finaldata_ap6[[spec$key]]
    if (is.null(fd)) {
      cat("  Skipping", spec$key, "— no finaldata available\n")
      next
    }
    gd <- fd %>%
      group_by(ti) %>%
      dplyr::summarise(gap = mean(yact - yfit, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = spec$label)
    gap_list[[spec$key]] <- gd
    cat("  Built gap for", spec$key, ": ti range", min(gd$ti), "to", max(gd$ti), "\n")
  }

  if (length(gap_list) < 2) {
    cat("  Fewer than 2 groups available — skipping.\n")
    return(NULL)
  }

  gap_all <- bind_rows(gap_list)

  # Build named aesthetic vectors from specs that actually appear in data
  present_specs <- Filter(function(s) s$key %in% names(gap_list), group_specs)
  col_vals  <- setNames(sapply(present_specs, `[[`, "col"),  sapply(present_specs, `[[`, "label"))
  lty_vals  <- setNames(sapply(present_specs, `[[`, "lty"),  sapply(present_specs, `[[`, "label"))
  size_vals <- setNames(rep(0.55, length(present_specs)),     sapply(present_specs, `[[`, "label"))

  # Force factor order so legend matches line order top-to-bottom
  gap_all$group <- factor(gap_all$group, levels = sapply(present_specs, `[[`, "label"))

  p <- ggplot(gap_all, aes(x = ti, y = gap * 100,
                            colour   = group,
                            linetype = group,
                            linewidth = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.2) +
    geom_line() +
    scale_colour_manual(name = NULL, values = col_vals) +
    scale_linetype_manual(name = NULL, values = lty_vals) +
    scale_linewidth_manual(name = NULL, values = size_vals) +
    scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
    scale_y_continuous(
      breaks = c(-30, -20, -10, 0, 10, 20),
      labels = c("-30%", "-20%", "-10%", "0%", "+10%", "+20%"),
      expand = c(0.02, 0.02)
    ) +
    labs(
      title = title_str,
      x     = "Years relative to takeover",
      y     = "Average Doppelganger Gap"
    ) +
    guides(
      colour    = guide_legend(ncol = 1, keyheight = 1.4,
                               override.aes = list(fill = NA, linewidth = 0.7)),
      linetype  = guide_legend(ncol = 1, keyheight = 1.4),
      linewidth = "none"
    ) +
    scm_theme() +
    theme(
      legend.position  = "right",
      legend.text      = element_text(size = 7),
      legend.key.width = unit(2.0, "cm"),
      legend.spacing.y = unit(0.4, "cm"),
      plot.title       = element_text(size = 9, hjust = 0.5),
      axis.title       = element_text(size = 7)
    )

  p
}

cat("\n\n========== FIGURE AP9 (four-group comparison, main paper) ==========\n")

tryCatch({

  main_specs <- list(
    list(key = "strict",     label = paste0("Strict auth-pop (N=",  nrow(filter(episodes, auth_strict == 1)),               ")"), col = "darkred",   lty = "solid"),
    list(key = "broad",      label = paste0("Broad auth-pop (N=",   nrow(filter(episodes, auth_broad  == 1)),               ")"), col = "red",        lty = "longdash"),
    list(key = "nonauthpop", label = paste0("Non-auth-pop (N=",     nrow(filter(episodes, auth_broad  == 0)),               ")"), col = "steelblue",  lty = "dotdash"),
    list(key = "fst_full",   label = paste0("FST full (N=",         nrow(filter(episodes, !(iso == "HUN" & year == 2010))), ")"), col = "grey50",     lty = "twodash")
  )

  p_main <- build_ap9_figure(main_specs,
               "Average doppelganger gap by subset")

  if (!is.null(p_main)) {
    outpath_ap9 <- file.path("figures", "FigureAP9.pdf")
    pdf(outpath_ap9, width = 13 / 2.54, height = 10 / 2.54)
    print(p_main)
    dev.off()
    cat("Saved:", outpath_ap9, "(", file.size(outpath_ap9), "bytes)\n")
  }

}, error = function(e) cat("AP9 main error:", conditionMessage(e), "\n"))

cat("\n\n========== FIGURE AP9-noECU (no-Ecuador robustness, appendix) ==========\n")

tryCatch({

  noecu_specs <- list(
    list(key = "strict_noecuador",     label = paste0("Strict, no Ecuador (N=",        nrow(filter(episodes, auth_strict == 1, iso != "ECU")), ")"), col = "darkred",  lty = "solid"),
    list(key = "broad_noecuador",      label = paste0("Broad, no Ecuador (N=",         nrow(filter(episodes, auth_broad  == 1, iso != "ECU")), ")"), col = "red",       lty = "longdash"),
    list(key = "nonauthpop",           label = paste0("Non-auth-pop (N=",              nrow(filter(episodes, auth_broad  == 0)),               ")"), col = "steelblue", lty = "dotdash"),
    list(key = "fst_full",             label = paste0("FST full (N=",                  nrow(filter(episodes, !(iso == "HUN" & year == 2010))), ")"), col = "grey50",    lty = "twodash")
  )

  p_noecu <- build_ap9_figure(noecu_specs,
               "Average doppelganger gap — no Ecuador robustness")

  if (!is.null(p_noecu)) {
    outpath_ap9noecu <- file.path("figures", "FigureAP9_noecuador.pdf")
    pdf(outpath_ap9noecu, width = 13 / 2.54, height = 10 / 2.54)
    print(p_noecu)
    dev.off()
    cat("Saved:", outpath_ap9noecu, "(", file.size(outpath_ap9noecu), "bytes)\n")
  }

}, error = function(e) cat("AP9 noECU error:", conditionMessage(e), "\n"))

cat("\n=== Figure AP9 complete ===\n")

# ============================================================
# FIGURES AP11–AP14 — Alternative Outcome Variables
# Runs the same scpi SCM estimation but on non-GDP outcomes.
# AP11: Gini index + Labor share (income distribution)
# AP12: Trade outcomes (koftrade, tradegdp, global)
# AP13: Macro stability (debtgdp, inflation)
# AP14: Institutional quality (institutions)
# Mirrors FST Figures 11–14.
# ============================================================

cat("\n\n========== FIGURES AP11–AP14 (alternative outcomes) ==========\n")

# Generic SCM runner for an alternative outcome variable
run_altoutcome_subset <- function(ep_subset, outcome_var, subset_label) {
  n_ep <- nrow(ep_subset)
  cat("\n--- alt-outcome:", outcome_var, "subset:", subset_label, "(N =", n_ep, ") ---\n")

  ple <- ple_data
  if (!(outcome_var %in% names(ple))) {
    cat("  Variable", outcome_var, "not found in dataset — skipping.\n")
    return(NULL)
  }
  ple$var <- ple[[outcome_var]]

  all_series <- list()

  for (k in seq_len(n_ep)) {
    ep    <- ep_subset[k, ]
    Oldc  <- ep$oid
    Year  <- ep$year
    Left  <- ep$left
    Case  <- paste(ep$nid, ep$year, sep = ".")
    fr1   <- ep$fr1
    fr2   <- ep$fr2
    fr3   <- ep$fr3
    fr4   <- if ("fr4" %in% names(ep) && !is.na(ep$fr4)) ep$fr4 else 30
    ep_period_pre  <- seq(0, fr3, 1)
    ep_fr4_run     <- min(fr3 + 15, fr4)   # alt-outcomes always use 15-year horizon
    ep_period_post <- (fr3 + 1):ep_fr4_run

    try({
      data <- ple[ple$year >= Year - sta & ple$year <= Year + 15, ]
      taker  <- data %>% filter(cid == Oldc)
      donors <- data %>% filter(cid != Oldc) %>%
        mutate(simul = ifelse(atakeover == 1 & year == Year, 1, 0)) %>%
        group_by(cid) %>% mutate(msimul = max(as.numeric(simul))) %>%
        filter(msimul == 0) %>% select(-simul, -msimul)
      data <- rbind(taker, donors)

      # Drop donors missing outcome in the pre-period (year <= Year)
      # Allow post-period gaps — only pre-period completeness is required for SCM matching
      data <- data %>% group_by(cid) %>%
        filter(cid == Oldc | all(!is.na(var[year <= Year])))

      if (nrow(data[data$cid != Oldc, ]) < 5) {
        cat("    Insufficient donors for", outcome_var, "in episode", k, "— skip\n")
        next
      }

      # Index to takeover year
      tysub <- data[data$year == Year, c("cid", "country", "var")]
      names(tysub)[names(tysub) == "var"] <- "ivar"
      data  <- merge(data, tysub)
      data  <- data %>% group_by(cid) %>%
        mutate(d = var - ivar, t = year - Year + 15)
      data  <- transform(data, index = as.numeric(factor(country)))

      # Dynamic treated-unit index
      Trea <- data$index[data$cid == Oldc][1]

      df <- scdata(
        df = data, features = features, constant = constant,
        cov.adj = cov.adj, cointegrated.data = cointegrated.data,
        id.var = "index", time.var = "t", outcome.var = "d",
        period.pre = ep_period_pre, period.post = ep_period_post,
        unit.tr = Trea, unit.co = unique(data$index)[-Trea]
      )
      result <- scpi(
        data = df, u.order = u.order, u.lags = u.lags,
        u.sigma = u.sigma, u.missp = u.missp,
        e.order = e.order, e.lags = e.lags,
        u.alpha = u.alpha, e.alpha = e.alpha,
        rho = rho, rho.max = rho.max, sims = sims,
        w.constr = w.constr, cores = cores, e.method = e.method
      )

      y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
      yfit  <- data.frame(t = c(fr1:fr2, fr3:ep_fr4_run), yfit = c(y.fit))
      y.act <- rbind(result$data$Y.pre, result$data$Y.post)
      yact  <- data.frame(
        t = c(ep_period_pre, ep_period_post), yact = c(y.act),
        case = Case, left = Left
      )
      ys <- merge(yact, yfit, by = "t", all = TRUE)

      ci <- extract_ci(result)
      cis <- if (!is.null(ci)) {
        data.frame(
          t = ep_period_post,
          sclgauss = c(ci$scl.gauss), scrgauss = c(ci$scr.gauss)
        )
      } else {
        data.frame(t = ep_period_post,
                   sclgauss = NA_real_, scrgauss = NA_real_)
      }
      series <- merge(ys, cis, by = "t", all = TRUE)
      all_series[[k]] <- series
      cat("    OK\n")
    })
  }

  all_series <- Filter(Negate(is.null), all_series)
  if (length(all_series) == 0) return(NULL)

  finaldata <- rbindlist(all_series, fill = TRUE)
  finaldata <- finaldata %>% group_by(case) %>% mutate(ti = t - 15)
  for (v in c("sclgauss", "scrgauss"))
    if (v %in% names(finaldata)) finaldata[[v]][finaldata$ti == 0] <- 0
  finaldata$all <- 1
  finaldata
}

# Simple 2-panel trends figure for an alternative outcome
make_altoutcome_panel <- function(fd, outcome_label, col = "blue", ylims = NULL, ylabs = NULL) {
  avg <- fd[fd$all == 1, ] %>%
    group_by(ti) %>%
    summarise(
      yfit     = mean(yfit,     na.rm = TRUE),
      yact     = mean(yact,     na.rm = TRUE),
      sclgauss = if ("sclgauss" %in% names(.)) mean(sclgauss, na.rm = TRUE) else NA_real_,
      scrgauss = if ("scrgauss" %in% names(.)) mean(scrgauss, na.rm = TRUE) else NA_real_,
      .groups  = "drop"
    )
  n_obs <- length(unique(fd$case))

  if (is.null(ylims)) ylims <- range(c(avg$yact, avg$yfit), na.rm = TRUE)
  if (is.null(ylabs)) ylabs <- waiver()

  ggplot(avg) +
    geom_ribbon(aes(x = ti, ymin = sclgauss, ymax = scrgauss,
                    fill = "90% CI"), alpha = 1) +
    geom_line(aes(x = ti, y = yfit,
                  colour = "Doppelganger avg.", linetype = "Doppelganger avg.",
                  size = "Doppelganger avg.")) +
    geom_line(aes(x = ti, y = yact,
                  colour = "Auth-pop avg.", linetype = "Auth-pop avg.",
                  size = "Auth-pop avg.")) +
    scale_colour_manual(name = '', values = c("Auth-pop avg." = col,
                                               "Doppelganger avg." = col,
                                               "90% CI" = "grey95")) +
    scale_fill_manual(name = '', values = c("Auth-pop avg." = col,
                                             "Doppelganger avg." = col,
                                             "90% CI" = "grey95")) +
    scale_linetype_manual(name = '', values = c("Auth-pop avg." = "solid",
                                                 "Doppelganger avg." = "longdash",
                                                 "90% CI" = "solid")) +
    scale_size_manual(name = '', values = c("Auth-pop avg." = 0.4,
                                             "Doppelganger avg." = 0.4,
                                             "90% CI" = 0.4)) +
    scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
    scale_y_continuous(limits = ylims, labels = ylabs, expand = c(0.02, 0.02)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    labs(title = paste0(outcome_label, " (N=", n_obs, ")"), x = "", y = "") +
    guides(color = guide_legend(ncol = 1, nrow = 3, keyheight = 0.7,
                                override.aes = list(fill = NA))) +
    scm_theme()
}

# Alt-outcomes run on the original 4 authpop subsets only
# (nonauthpop and fst_full are not needed for the alternative-outcome appendix)
subsets_altout <- subsets[c("strict", "broad", "strict_noecuador", "broad_noecuador")]

# Figure AP12: Trade outcomes
for (sname in names(subsets_altout)) {
  ep_sub <- subsets[[sname]]
  cat("\nFigureAP12 subset:", sname, "\n")
  tryCatch({
    fd_trade  <- run_altoutcome_subset(ep_sub, "koftrade", paste0(sname, "_koftrade"))
    fd_tradeg <- run_altoutcome_subset(ep_sub, "tradegdp", paste0(sname, "_tradegdp"))
    fd_global <- run_altoutcome_subset(ep_sub, "global",   paste0(sname, "_global"))

    outpath <- file.path("figures", paste0("FigureAP12_", sname, ".pdf"))
    pdf(outpath, width = 23 / 2.54, height = 8 / 2.54)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
    if (!is.null(fd_trade))
      print(make_altoutcome_panel(fd_trade,  "Trade openness (KOF)"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    if (!is.null(fd_tradeg))
      print(make_altoutcome_panel(fd_tradeg, "Trade/GDP"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    if (!is.null(fd_global))
      print(make_altoutcome_panel(fd_global, "Financial openness"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
    dev.off()
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

# Figure AP13: Macro stability
for (sname in names(subsets_altout)) {
  ep_sub <- subsets[[sname]]
  cat("\nFigureAP13 subset:", sname, "\n")
  tryCatch({
    fd_debt  <- run_altoutcome_subset(ep_sub, "debtgdp",   paste0(sname, "_debt"))
    fd_infl  <- run_altoutcome_subset(ep_sub, "inflation",  paste0(sname, "_infl"))

    outpath <- file.path("figures", paste0("FigureAP13_", sname, ".pdf"))
    pdf(outpath, width = 16 / 2.54, height = 8 / 2.54)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
    if (!is.null(fd_debt))
      print(make_altoutcome_panel(fd_debt, "Debt/GDP"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    if (!is.null(fd_infl))
      print(make_altoutcome_panel(fd_infl, "Inflation",
                                  col = "darkred"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    dev.off()
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

# Figure AP13-noVEN: Macro stability, Venezuela excluded (removes hyperinflation outlier)
subsets_noven <- lapply(subsets_altout, function(ep_sub) ep_sub[ep_sub$iso != "VEN", ])
for (sname in names(subsets_noven)) {
  ep_sub <- subsets_noven[[sname]]
  cat("\nFigureAP13noVEN subset:", sname, "\n")
  tryCatch({
    fd_debt  <- run_altoutcome_subset(ep_sub, "debtgdp",   paste0(sname, "_debt_noven"))
    fd_infl  <- run_altoutcome_subset(ep_sub, "inflation",  paste0(sname, "_infl_noven"))

    outpath <- file.path("figures", paste0("FigureAP13noVEN_", sname, ".pdf"))
    pdf(outpath, width = 16 / 2.54, height = 8 / 2.54)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
    if (!is.null(fd_debt))
      print(make_altoutcome_panel(fd_debt, "Debt/GDP"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    if (!is.null(fd_infl))
      print(make_altoutcome_panel(fd_infl, "Inflation (excl. Venezuela)",
                                  col = "darkred"),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    dev.off()
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

# Figure AP14: Institutional quality
for (sname in names(subsets_altout)) {
  ep_sub <- subsets[[sname]]
  cat("\nFigureAP14 subset:", sname, "\n")
  tryCatch({
    fd_inst <- run_altoutcome_subset(ep_sub, "institutions", paste0(sname, "_inst"))

    if (!is.null(fd_inst)) {
      outpath <- file.path("figures", paste0("FigureAP14_", sname, ".pdf"))
      pdf(outpath, width = 10 / 2.54, height = 8 / 2.54)
      print(make_altoutcome_panel(fd_inst, "Institutional quality", col = "darkred"))
      dev.off()
      cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
    }
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

cat("\n=== ranscm_authpop.R complete (figures AP6–AP9, AP12–AP14, AP13-noVEN) ===\n")
