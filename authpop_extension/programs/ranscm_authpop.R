# ranscm_authpop.R
# Authoritarian-Populist Subset Extension — Synthetic Control Estimation
#
# Replicates FST (2023) ranscm.R logic on four subsets drawn from the
# original 28 populist episodes:
#
#   strict          — 9 episodes in electoral/closed autocracies at takeover
#   broad           — 13 episodes (strict + became authoritarian during tenure)
#   strict_noecuador — strict minus 3 Ecuador episodes (robustness check)
#   broad_noecuador  — broad minus 3 Ecuador episodes (robustness check)
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
#   FigureAP6_strict.pdf, FigureAP6_broad.pdf, FigureAP6_strict_noecuador.pdf, FigureAP6_broad_noecuador.pdf
#   FigureAP7_*.pdf (sims=10 robustness)
#   FigureAP8_*.pdf (augsynth, 10-period pre-window)
#   FigureAP9_*.pdf (penalized SCM / LowRankQP)
#   FigureAP10_*.pdf (pooled multisynth)
#   FigureAP11_*.pdf through FigureAP14_*.pdf (alternative outcomes)

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

run_scm_subset <- function(ep_subset, subset_label) {

  n_ep <- nrow(ep_subset)
  cat("\n--- Running subset:", subset_label, "(N =", n_ep, ") ---\n")

  all_series <- list()

  for (k in seq_len(n_ep)) {

    ep   <- ep_subset[k, ]
    Oldc <- ep$oid
    Trea <- ep$nid
    Year <- ep$year
    Left <- ep$left
    Case <- paste(ep$nid, ep$year, sep = ".")
    fr1  <- ep$fr1
    fr2  <- ep$fr2
    fr3  <- ep$fr3

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

      # --- Fit SCM ---
      df <- scdata(
        df = data, features = features, constant = constant,
        cov.adj = cov.adj, cointegrated.data = cointegrated.data,
        id.var = "index", time.var = "t", outcome.var = "d",
        period.pre = period.pre, period.post = period.post,
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
      yfit  <- data.frame(t = c(fr1:fr2, fr3:30), yfit = c(y.fit))

      y.act <- rbind(result$data$Y.pre, result$data$Y.post)
      yact  <- data.frame(
        t    = c(period.pre, period.post),
        yact = c(y.act),
        case = Case,
        left = Left
      )

      ys <- merge(yact, yfit, by = "t", all = TRUE)

      ci <- extract_ci(result)
      if (!is.null(ci)) {
        cis <- data.frame(
          t            = period.post,
          sclinsample  = c(ci$scl.insample),
          scrinsample  = c(ci$scr.insample),
          sclgauss     = c(ci$scl.gauss),
          scrgauss     = c(ci$scr.gauss)
        )
      } else {
        cis <- data.frame(
          t            = period.post,
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
  broad_noecuador  = filter(episodes, auth_broad  == 1, iso != "ECU")
)

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
# FIGURE AP8 — Augmented Synthetic Control (augsynth)
# Uses Bhatt et al. augsynth package which augments the
# traditional SCM with an outcome model ridge regression.
# Pre-period 0:10 (not 0:15) — same as FST Figure 8.
# Mirrors FST Figure 8.
# ============================================================

cat("\n\n========== FIGURE AP8 (augsynth) ==========\n")

run_augsynth_subset <- function(ep_subset, subset_label) {
  n_ep <- nrow(ep_subset)
  cat("\n--- augsynth:", subset_label, "(N =", n_ep, ") ---\n")

  period.pre.aug  <- seq(from = 0, to = 10, by = 1)
  period.post.aug <- 11:30
  all_series <- list()

  for (k in seq_len(n_ep)) {
    ep   <- ep_subset[k, ]
    Oldc <- ep$oid
    Trea <- ep$nid
    Year <- ep$year
    Left <- ep$left
    Case <- paste(ep$nid, ep$year, sep = ".")
    fr1  <- ep$fr1
    fr2  <- min(ep$fr2, 10)   # augsynth uses 10-period pre-window
    fr3  <- max(ep$fr3, 11)

    cat("  [", k, "/", n_ep, "]", ep$iso, Year, "\n")

    try({
      data <- ple_data
      data <- data[data$year >= Year - sta & data$year <= Year + 15, ]

      taker  <- data %>% filter(cid == Oldc)
      donors <- data %>% filter(cid != Oldc) %>%
        mutate(simul = ifelse(atakeover == 1 & year == Year, 1, 0)) %>%
        group_by(cid) %>% mutate(msimul = max(as.numeric(simul))) %>%
        filter(msimul != 1) %>% select(-simul, -msimul)
      data <- rbind(taker, donors)
      data <- data %>% group_by(cid) %>%
        filter(all(!is.na(fstgdp) | cid == Oldc))

      data$lgfstgdp <- log(data$fstgdp)
      tysub <- data[data$year == Year, c("cid", "country", "lgfstgdp")]
      names(tysub)[names(tysub) == "lgfstgdp"] <- "ilgfstgdp"
      data  <- merge(data, tysub)
      data  <- data %>% group_by(cid) %>%
        mutate(d = lgfstgdp - ilgfstgdp, t = year - Year + 15)
      data  <- transform(data, index = as.numeric(factor(country)))
      data  <- data %>% mutate(d = replace(d, war == 1, NA))

      df <- scdata(
        df = data, features = features, constant = constant,
        cov.adj = cov.adj, cointegrated.data = cointegrated.data,
        id.var = "index", time.var = "t", outcome.var = "d",
        period.pre = period.pre.aug, period.post = period.post.aug,
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
      t_pre_fit <- period.pre.aug[!is.na(c(result$data$Y.pre))]
      yfit  <- data.frame(t = c(t_pre_fit, period.post.aug), yfit = c(y.fit))
      y.act <- rbind(result$data$Y.pre, result$data$Y.post)
      yact  <- data.frame(
        t    = c(period.pre.aug, period.post.aug),
        yact = c(y.act),
        case = Case,
        left = Left
      )
      ys <- merge(yact, yfit, by = "t", all = TRUE)

      ci <- extract_ci(result)
      if (!is.null(ci)) {
        cis <- data.frame(
          t            = period.post.aug,
          sclinsample  = c(ci$scl.insample),
          scrinsample  = c(ci$scr.insample),
          sclgauss     = c(ci$scl.gauss),
          scrgauss     = c(ci$scr.gauss)
        )
      } else {
        cis <- data.frame(
          t = period.post.aug,
          sclinsample = NA_real_, scrinsample = NA_real_,
          sclgauss    = NA_real_, scrgauss    = NA_real_
        )
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
  for (v in c("sclinsample","scrinsample","sclgauss","scrgauss"))
    finaldata[[v]][finaldata$ti == 0] <- 0
  finaldata$all   <- 1
  finaldata$right <- ifelse(finaldata$left == 1, 0, 1)
  finaldata
}

for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\nSubset:", sname, "(N =", nrow(ep_sub), ")\n")

  finaldata <- run_augsynth_subset(ep_sub, sname)
  if (is.null(finaldata)) { cat("SKIPPED\n"); next }

  outpath <- file.path("figures", paste0("FigureAP8_", sname, ".pdf"))
  tryCatch({
    save_subset_figure(outpath, finaldata)
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

# ============================================================
# FIGURE AP9 — Penalized Synthetic Control (LowRankQP)
# Cross-validates penalty parameter lambda over grid using
# pre-treatment RMSE and bias. Plots results for three lambda
# values: 0, 0.1, and optimal. Mirrors FST Figure 9.
# ============================================================

cat("\n\n========== FIGURE AP9 (penalized SCM) ==========\n")

wsoll1 <- function(X0, X1, V, pen = 0.0) {
  n     <- ncol(X0)
  Delta <- diag(t(X0 - matrix(rep(1, n), ncol = n) %x% X1) %*% V %*%
                (X0 - matrix(rep(1, n), ncol = n) %x% X1))
  P     <- 2 * t(X0) %*% V %*% X0
  q     <- t(-2 * t(X0) %*% V %*% X1 + pen * Delta)
  sol   <- LowRankQP(Vmat = P, dvec = q,
                     Amat = matrix(1, ncol = n), bvec = 1,
                     uvec = rep(1, n), method = "LU")
  sol$alpha
}

TZero <- function(x, tol = 1e-6, scale = TRUE) {
  if (!all(x > 0)) stop("Some elements are negative!")
  y <- ifelse(x < tol, 0, x)
  if (scale) y <- y / sum(y)
  y
}

regsynth <- function(X0, X1, Y0, Y1, V, pen, tol = 1e-6) {
  sol <- wsoll1(X0, X1, V, pen)
  sol <- TZero(sol, tol)
  round(sol, 3)
}

lambda <- c(0, .00001, .01, .1, .15, seq(.25, 5, .1))

run_pensynth_subset <- function(ep_subset, subset_label) {
  n_ep <- nrow(ep_subset)
  cat("\n--- pensynth:", subset_label, "(N =", n_ep, ") ---\n")

  pdata <- ple_data
  pdata$lgfstgdp <- log(pdata$fstgdp)
  pdata <- pdata[which(pdata$year <= 1913 |
                       (pdata$year >= 1919 & pdata$year <= 1938) |
                        pdata$year >= 1946), ]

  s <- ep_subset %>%
    mutate(minus = -15, plus = 15) %>%
    as.data.frame()
  # Meciar 1990 (cid=47) special case
  s$minus[s$oid == 47 & s$year == 1990] <- -5

  curve_RMSE <- matrix(NA, nrow = n_ep, ncol = length(lambda))
  curve_bias <- matrix(NA, nrow = n_ep, ncol = length(lambda))

  for (l in seq_along(lambda)) {
    for (k in seq_len(n_ep)) {
      tryCatch({
        i    <- s$oid[k]
        date <- s$year[k]
        other_pop <- s$oid[s$year == date & s$oid != i]
        scm_data  <- pdata[pdata$year >= date + s$minus[k] &
                             pdata$year <= date + s$plus[k], ]
        scm_data  <- scm_data[!(scm_data$cid %in% other_pop), ]
        na_cid    <- unique(scm_data$cid[is.na(scm_data$lgfstgdp)])
        scm_data  <- scm_data[!(scm_data$cid %in% na_cid) | scm_data$cid == i, ]
        scm_data  <- scm_data %>% group_by(cid) %>%
          mutate(Y = lgfstgdp - lgfstgdp[which(year == date)])
        scm_x <- scm_data[scm_data$year < date, c("cid", "year", "Y")]
        scm_y <- scm_data[, c("cid", "year", "Y")]
        scm_y_wide <- spread(scm_y, year, Y)
        scm_x_wide <- dcast(setDT(scm_x), cid ~ year, value.var = "Y", sep = "")
        X_0 <- t(as.matrix(scm_x_wide[scm_x_wide$cid != i, -1]))
        X_1 <- as.numeric(t(as.matrix(scm_x_wide[scm_x_wide$cid == i, -1])))
        Y_0 <- as.matrix(scm_y_wide[scm_y_wide$cid != i, -1])
        Y_1 <- as.numeric(scm_y_wide[scm_y_wide$cid == i, -1])
        V   <- diag(nrow(X_0))
        W   <- regsynth(X_0, X_1, Y_0, Y_1, V, pen = lambda[l])
        Y_0hat <- as.numeric(t(W) %*% Y_0)
        times  <- as.numeric(colnames(scm_y_wide)[-1]) - date
        tau    <- Y_1 - Y_0hat
        tau_pre <- tau[times <= 0 & times >= -15]
        curve_RMSE[k, l] <- sqrt(mean(tau_pre^2))
        curve_bias[k, l] <- abs(mean(tau_pre))
      }, error = function(e) NULL)
    }
  }

  mean_RMSE     <- apply(curve_RMSE, 2, mean, na.rm = TRUE)
  lambda_opt    <- min(lambda[which(mean_RMSE == min(mean_RMSE, na.rm = TRUE))])
  lambda_final  <- c(0, 0.1, lambda_opt)

  # Run final estimates for each lambda
  all_runs <- list()

  for (li in seq_along(lambda_final)) {
    Y_synth <- data.frame()
    for (k in seq_len(n_ep)) {
      tryCatch({
        i    <- s$oid[k]
        date <- s$year[k]
        Left <- s$left[k]
        Case <- paste(s$nid[k], date, sep = ".")
        other_pop <- s$oid[s$year == date & s$oid != i]
        scm_data  <- pdata[pdata$year >= date + s$minus[k] &
                             pdata$year <= date + s$plus[k], ]
        scm_data  <- scm_data[!(scm_data$cid %in% other_pop), ]
        na_cid    <- unique(scm_data$cid[is.na(scm_data$lgfstgdp)])
        scm_data  <- scm_data[!(scm_data$cid %in% na_cid) | scm_data$cid == i, ]
        scm_data  <- scm_data %>% group_by(cid) %>%
          mutate(Y = lgfstgdp - lgfstgdp[which(year == date)])
        scm_x <- scm_data[scm_data$year < date, c("cid", "year", "Y")]
        scm_y <- scm_data[, c("cid", "year", "Y")]
        scm_y_wide <- spread(scm_y, year, Y)
        scm_x_wide <- dcast(setDT(scm_x), cid ~ year, value.var = "Y", sep = "")
        X_0 <- t(as.matrix(scm_x_wide[scm_x_wide$cid != i, -1]))
        X_1 <- as.numeric(t(as.matrix(scm_x_wide[scm_x_wide$cid == i, -1])))
        Y_0 <- as.matrix(scm_y_wide[scm_y_wide$cid != i, -1])
        Y_1 <- as.numeric(scm_y_wide[scm_y_wide$cid == i, -1])
        V   <- diag(nrow(X_0))
        W   <- regsynth(X_0, X_1, Y_0, Y_1, V, pen = lambda_final[li])
        Y_0hat <- as.numeric(t(W) %*% Y_0)
        times  <- as.numeric(colnames(scm_y_wide)[-1]) - date
        df_k <- data.frame(
          ti   = times,
          yfit = Y_0hat,
          yact = Y_1,
          case = Case,
          left = Left,
          lam  = lambda_final[li]
        )
        Y_synth <- rbind(Y_synth, df_k)
      }, error = function(e) NULL)
    }
    all_runs[[li]] <- Y_synth
  }

  list(all_runs = all_runs, lambda_final = lambda_final, n_ep = n_ep)
}

save_pensynth_figure <- function(outpath, pen_results, subset_label) {
  all_runs     <- pen_results$all_runs
  lambda_final <- pen_results$lambda_final
  n_ep         <- pen_results$n_ep

  panel_list <- list()
  lam_labels <- c("No penalty (\u03BB=0)", "\u03BB=0.1", paste0("Optimal \u03BB=", round(lambda_final[3], 3)))

  for (li in seq_along(lambda_final)) {
    fd <- all_runs[[li]]
    if (is.null(fd) || nrow(fd) == 0) { panel_list[[li]] <- ggplot() + labs(title = "No data"); next }

    fd$all <- 1
    avg_all <- ddply(fd[fd$all == 1, ], .(ti), summarise,
                     yfit = mean(yfit, na.rm = TRUE),
                     yact = mean(yact, na.rm = TRUE))
    gap <- avg_all$yact - avg_all$yfit

    p <- ggplot(avg_all) +
      geom_line(aes(x = ti, y = yfit, colour = "Doppelganger avg.",
                    linetype = "Doppelganger avg.", size = "Doppelganger avg.")) +
      geom_line(aes(x = ti, y = yact, colour = "Auth-pop avg.",
                    linetype = "Auth-pop avg.", size = "Auth-pop avg.")) +
      scale_colour_manual(name = '', values = c("Auth-pop avg." = "blue",
                                                 "Doppelganger avg." = "blue")) +
      scale_linetype_manual(name = '', values = c("Auth-pop avg." = "solid",
                                                   "Doppelganger avg." = "longdash")) +
      scale_size_manual(name = '', values = c("Auth-pop avg." = 0.4,
                                               "Doppelganger avg." = 0.4)) +
      scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
      scale_y_continuous(limits = c(-0.40, 0.60),
                         breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.60),
                         labels = c("-40%", "-20%", "0%", "+20%", "+40%", "+60%"),
                         expand = c(0.02, 0.02)) +
      geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
      labs(title = paste0(lam_labels[li], " (N=", n_ep, ")"), x = "", y = "") +
      scm_theme()
    panel_list[[li]] <- p
  }

  pdf(outpath, width = 23 / 2.54, height = 8 / 2.54)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
  for (li in 1:3) {
    tryCatch(
      print(panel_list[[li]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = li)),
      error = function(e) NULL
    )
  }
  dev.off()
}

for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\nSubset:", sname, "(N =", nrow(ep_sub), ")\n")
  tryCatch({
    pen_res <- run_pensynth_subset(ep_sub, sname)
    outpath <- file.path("figures", paste0("FigureAP9_", sname, ".pdf"))
    save_pensynth_figure(outpath, pen_res, sname)
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

# ============================================================
# FIGURE AP10 — Pooled SCM / Augmented multisynth
# Uses multisynth() from the augsynth package.
# Creates pseudopanel for countries with multiple episodes
# (Argentina has up to 2 episodes, Ecuador up to 3).
# Mirrors FST Figure 10.
# ============================================================

cat("\n\n========== FIGURE AP10 (multisynth) ==========\n")

run_multisynth_subset <- function(ep_subset, subset_label) {
  n_ep <- nrow(ep_subset)
  cat("\n--- multisynth:", subset_label, "(N =", n_ep, ") ---\n")

  df_base <- ple_data[, c("country", "year", "cid")]
  df_base$lgfstgdp <- log(ple_data$fstgdp)

  # Build pseudopanel: for countries with multiple episodes,
  # create duplicate rows with episode-specific country names
  multi_countries <- ep_subset %>% group_by(iso) %>%
    filter(n() > 1) %>% ungroup()

  pseudo_list <- list(df_base)  # start with original data

  if (nrow(multi_countries) > 0) {
    for (k in seq_len(nrow(multi_countries))) {
      ep    <- multi_countries[k, ]
      # Find country name from ple_data
      cname <- unique(ple_data$country[ple_data$cid == ep$oid])[1]
      dup   <- df_base[df_base$country == cname, ]
      dup$country <- paste0(cname, "_", ep$year)
      pseudo_list[[length(pseudo_list) + 1]] <- dup
    }
  }

  data <- do.call(rbind, pseudo_list)
  data <- data %>% mutate(treatedyear = 0)

  for (k in seq_len(n_ep)) {
    ep    <- ep_subset[k, ]
    cname <- unique(ple_data$country[ple_data$cid == ep$oid])[1]
    # Check if this is a "duplicate" country (multi-episode)
    is_multi <- nrow(ep_subset[ep_subset$iso == ep$iso, ]) > 1
    if (is_multi) {
      label <- paste0(cname, "_", ep$year)
    } else {
      label <- cname
    }
    data$treatedyear[data$country == label & data$year >= ep$year] <- 1
  }

  tryCatch({
    ppool_syn <- multisynth(lgfstgdp ~ treatedyear, country, year, data,
                            fixedeff = TRUE, alpha = 0.1,
                            n_lags = 15, n_leads = 16, nu = 0.5)
    ppool <- data.frame(summary(ppool_syn)$att)

    ppool <- ppool %>%
      filter(!is.na(Time), !is.na(Estimate),
             Time >= -15, Time <= 15,
             Level != "Average")

    # Map levels back to takeover years
    ppool <- ppool %>% mutate(takyear = 0)
    for (k in seq_len(n_ep)) {
      ep    <- ep_subset[k, ]
      cname <- unique(ple_data$country[ple_data$cid == ep$oid])[1]
      is_multi <- nrow(ep_subset[ep_subset$iso == ep$iso, ]) > 1
      label <- if (is_multi) paste0(cname, "_", ep$year) else cname
      ppool$takyear[ppool$Level == label] <- ep$year
    }
    ppool <- ppool %>% filter(takyear > 0)

    # Attach actual lgfstgdp, index to takeover year
    ppool <- ppool %>% mutate(calyear = takyear + Time)
    ppool_actual <- ple_data %>%
      select(cid, year, country) %>%
      mutate(lgfstgdp = log(ple_data$fstgdp))

    # Average across episodes
    ppool_avg <- ppool %>%
      group_by(Time) %>%
      summarise(
        Estimate    = mean(Estimate,    na.rm = TRUE),
        lower_bound = mean(lower_bound, na.rm = TRUE),
        upper_bound = mean(upper_bound, na.rm = TRUE),
        .groups = "drop"
      )

    ppool_avg
  }, error = function(e) {
    cat("  multisynth error:", conditionMessage(e), "\n")
    NULL
  })
}

save_multisynth_figure <- function(outpath, ppool_avg, subset_label) {
  if (is.null(ppool_avg) || nrow(ppool_avg) == 0) {
    cat("  No multisynth data — skipping figure\n")
    return(invisible(NULL))
  }

  p <- ggplot(ppool_avg) +
    geom_ribbon(aes(x = Time, ymin = lower_bound, ymax = upper_bound),
                fill = "grey85", alpha = 0.8) +
    geom_line(aes(x = Time, y = Estimate,
                  colour = "Auth-pop gap (avg.)",
                  linetype = "Auth-pop gap (avg.)",
                  size    = "Auth-pop gap (avg.)")) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    scale_colour_manual(name = '', values = c("Auth-pop gap (avg.)" = "blue")) +
    scale_linetype_manual(name = '', values = c("Auth-pop gap (avg.)" = "solid")) +
    scale_size_manual(name = '', values = c("Auth-pop gap (avg.)" = 0.4)) +
    scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
    labs(title = paste0("Pooled SCM: ", subset_label), x = "", y = "") +
    scm_theme()

  pdf(outpath, width = 10 / 2.54, height = 8 / 2.54)
  print(p)
  dev.off()
}

for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\nSubset:", sname, "(N =", nrow(ep_sub), ")\n")
  tryCatch({
    ppool_avg <- run_multisynth_subset(ep_sub, sname)
    outpath <- file.path("figures", paste0("FigureAP10_", sname, ".pdf"))
    save_multisynth_figure(outpath, ppool_avg, sname)
    cat("Saved:", outpath, "\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

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
    Trea  <- ep$nid
    Year  <- ep$year
    Left  <- ep$left
    Case  <- paste(ep$nid, ep$year, sep = ".")
    fr1   <- ep$fr1
    fr2   <- ep$fr2
    fr3   <- ep$fr3

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

      df <- scdata(
        df = data, features = features, constant = constant,
        cov.adj = cov.adj, cointegrated.data = cointegrated.data,
        id.var = "index", time.var = "t", outcome.var = "d",
        period.pre = period.pre, period.post = period.post,
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
      yfit  <- data.frame(t = c(fr1:fr2, fr3:30), yfit = c(y.fit))
      y.act <- rbind(result$data$Y.pre, result$data$Y.post)
      yact  <- data.frame(
        t = c(period.pre, period.post), yact = c(y.act),
        case = Case, left = Left
      )
      ys <- merge(yact, yfit, by = "t", all = TRUE)

      ci <- extract_ci(result)
      cis <- if (!is.null(ci)) {
        data.frame(
          t = period.post,
          sclgauss = c(ci$scl.gauss), scrgauss = c(ci$scr.gauss)
        )
      } else {
        data.frame(t = period.post,
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

# Figure AP11: Gini + Labor share
for (sname in names(subsets)) {
  ep_sub <- subsets[[sname]]
  cat("\nFigureAP11 subset:", sname, "\n")
  tryCatch({
    fd_gini  <- run_altoutcome_subset(ep_sub, "gini",       paste0(sname, "_gini"))
    fd_labor <- run_altoutcome_subset(ep_sub, "laborshare", paste0(sname, "_labor"))

    outpath <- file.path("figures", paste0("FigureAP11_", sname, ".pdf"))
    pdf(outpath, width = 16 / 2.54, height = 8 / 2.54)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
    if (!is.null(fd_gini))
      print(make_altoutcome_panel(fd_gini, "Gini index",
                                  ylims = c(-4, 4),
                                  ylabs = c("-4 pt", "-3 pt", "-2 pt", "-1 pt",
                                            "0", "+1 pt", "+2 pt", "+3 pt", "+4 pt")),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    if (!is.null(fd_labor))
      print(make_altoutcome_panel(fd_labor, "Labor share",
                                  ylims = c(-4, 4)),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    dev.off()
    cat("Saved:", outpath, "(", file.size(outpath), "bytes)\n")
  }, error = function(e) cat("Error:", conditionMessage(e), "\n"))
}

# Figure AP12: Trade outcomes
for (sname in names(subsets)) {
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
for (sname in names(subsets)) {
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

# Figure AP14: Institutional quality
for (sname in names(subsets)) {
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

cat("\n=== ranscm_authpop.R complete (all figures AP6–AP14) ===\n")
