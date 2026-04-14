# ranscm_auth.R
# Authoritarian Leaders Extension — Synthetic Control Estimation
# Analogue of ranscm.R from FST (2023) for authoritarian leaders
#
# Run from: C:\PLE\auth_extension\programs\
# (script does setwd('..') to get to auth_extension/ root)
#
# Inputs:
#   ../data/auth_dataset.dta    — PLE panel with auth treatment vars
#   data/authoritarian_episodes_scm_viable.csv — episode list
# Outputs:
#   figures/FigureA_6.pdf   — All auth SCM GDP paths (avg + CI)
#   figures/FigureA_7.pdf   — By auth type (single-party / military / personalist)

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
library(LowRankQP)
library(reshape2)
library(ggplot2)
library(tibble)
library(CVXR)
library(Rmpfr)
library(readr)

rm(list = ls(all = TRUE))
setwd('..')

# ============================================================
# Load data and episode list
# ============================================================

auth_data <- read_dta("data/auth_dataset.dta")
episodes  <- read_csv("data/authoritarian_episodes_scm_viable.csv",
                      show_col_types = FALSE)

# Keep only non-populist episodes; cid comes from the CSV directly
episodes <- episodes %>%
  filter(is_also_populist == 0) %>%
  filter(!is.na(cid))

cat("Auth episodes for SCM:", nrow(episodes), "\n")

# ============================================================
# SCM parameters (same as FST ranscm.R)
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
sta               <- 15   # pre-treatment window length

# Auth type grouping
type_sp <- "Single-party"
type_mi <- c("Military", "Military-personal")
type_pe <- c("Personalist", "Oligarchy", "Monarchy")

##############################################################
# Figure A_6: SCM GDP paths — all auth episodes
##############################################################

try({

  all_series <- list()

  for (k in seq_len(nrow(episodes))) {

    ep    <- episodes[k, ]
    Oldc  <- ep$cid
    Year  <- ep$start_yr
    atype <- ep$auth_type

    # Compute type group
    auth_group <- if (atype %in% type_sp) "Single-party" else
                  if (atype %in% type_mi) "Military"      else
                  "Personalist"

    # Load data window: up to sta years pre + 15 years post
    data <- auth_data
    data <- data[data$year >= Year - sta & data$year <= Year + 15, ]

    # Treated country
    taker  <- data %>% filter(cid == Oldc)

    # Donors: exclude countries with an auth takeover in the same year
    donors <- data %>% filter(cid != Oldc)
    donors <- donors %>%
      mutate(simul = ifelse(atakeover_auth == 1 & year == Year, 1, 0)) %>%
      group_by(cid) %>%
      mutate(msimul = max(as.numeric(simul))) %>%
      filter(msimul != 1) %>%
      select(-simul, -msimul) %>%
      ungroup()

    data <- rbind(taker, donors)

    # Require complete fstgdp for donors
    data <- data %>%
      group_by(cid) %>%
      filter(all(!is.na(fstgdp)) | cid == Oldc) %>%
      ungroup()

    nd <- n_distinct(data$cid)
    if (nd < 5) { cat(sprintf("  Skip (donors=%d): %s %d\n", nd, ep$iso, Year)); next }

    data$lgfstgdp <- log(data$fstgdp)

    # Normalise to takeover year = 0
    tysub <- data %>% filter(year == Year) %>%
      select(cid, lgfstgdp) %>%
      rename(ilgfstgdp = lgfstgdp)
    data <- merge(data, tysub)
    data <- data %>%
      group_by(cid) %>%
      mutate(d = lgfstgdp - ilgfstgdp,
             t = year - Year + sta) %>%
      ungroup()
    data <- transform(data, index = as.numeric(factor(cid)))

    # Remove wartime observations
    data <- data %>% mutate(d = replace(d, war == 1, NA))

    # Actual pre-treatment window available
    avail_pre <- sum(data$year < Year & data$cid == Oldc & !is.na(data$d))
    if (avail_pre < 5) { cat(sprintf("  Skip (pre=%d): %s %d\n", avail_pre, ep$iso, Year)); next }

    # Adjust periods based on actual availability
    fr1 <- max(0, sta - avail_pre)
    fr2 <- sta - 1
    fr3 <- sta

    period.pre  <- seq(from = fr1, to = fr2, by = 1)
    period.post <- seq(from = fr3, to = sta + 15, by = 1)

    # Get treatment unit index
    Trea <- unique(data$index[data$cid == Oldc])
    if (length(Trea) == 0) { cat(sprintf("  Skip (no Trea): %s %d\n", ep$iso, Year)); next }

    series <- tryCatch({
      df  <- scdata(df = data, features = features, constant = constant,
                    cov.adj = cov.adj, cointegrated.data = cointegrated.data,
                    id.var = "index", time.var = "t", outcome.var = "d",
                    period.pre = period.pre, period.post = period.post,
                    unit.tr = Trea,
                    unit.co = unique(data$index[data$cid != Oldc]))
      res <- scpi(data = df, u.order = u.order, u.lags = u.lags,
                  u.sigma = u.sigma, u.missp = u.missp,
                  e.order = e.order, e.lags = e.lags,
                  u.alpha = u.alpha, e.alpha = e.alpha,
                  rho = rho, rho.max = rho.max,
                  sims = sims, w.constr = w.constr,
                  cores = cores, e.method = e.method)

      # Force Y.pre/Y.post to matrix (scpi may return 1D vector for single obs)
      y.pre.act  <- as.matrix(res$data$Y.pre)
      y.post.act <- as.matrix(res$data$Y.post)
      y.pre.fit  <- as.matrix(res$est.results$Y.pre.fit)
      y.post.fit <- as.matrix(res$est.results$Y.post.fit)
      n_pre_r  <- NROW(y.pre.act)
      n_post_r <- NROW(y.post.act)
      if (n_pre_r < 1L || n_post_r < 1L) stop("Empty Y from scpi")

      # Time indices from row names; fall back to theoretical periods
      t_pre_r  <- suppressWarnings(as.numeric(rownames(y.pre.act)))
      t_post_r <- suppressWarnings(as.numeric(rownames(y.post.act)))
      if (length(t_pre_r)  == 0 || any(is.na(t_pre_r)))
        t_pre_r  <- period.pre[seq_len(n_pre_r)]
      if (length(t_post_r) == 0 || any(is.na(t_post_r)))
        t_post_r <- period.post[seq_len(n_post_r)]

      yact <- data.frame(
        t     = c(t_pre_r, t_post_r),
        yact  = c(y.pre.act, y.post.act),
        case  = paste0(ep$iso, Year),
        atype = auth_group,
        iso   = ep$iso)
      yfit <- data.frame(
        t    = c(t_pre_r, t_post_r),
        yfit = c(y.pre.fit, y.post.fit))

      # CI extraction — soft-fail: include episode without CIs if extraction fails
      extract_ci <- function(m) {
        m2 <- tryCatch(as.matrix(m), error = function(e) NULL)
        if (is.null(m2) || NROW(m2) == 0 || NCOL(m2) < 2) return(NULL)
        m2
      }
      ci_g <- extract_ci(res$inference.results$CI.all.gaussian)
      ci_i <- extract_ci(res$inference.results$CI.in.sample)

      if (!is.null(ci_g) && !is.null(ci_i)) {
        n_ci <- min(NROW(ci_g), NROW(ci_i), n_post_r)
        t_ci <- t_post_r[seq_len(n_ci)]
        cis  <- data.frame(
          t           = t_ci,
          sclinsample = as.numeric(ci_i[seq_len(n_ci), 1]),
          scrinsample = as.numeric(ci_i[seq_len(n_ci), 2]),
          sclgauss    = as.numeric(ci_g[seq_len(n_ci), 1]),
          scrgauss    = as.numeric(ci_g[seq_len(n_ci), 2]))
      } else {
        # No usable CIs — include paths without uncertainty bands
        cis <- data.frame(
          t           = t_post_r,
          sclinsample = NA_real_, scrinsample = NA_real_,
          sclgauss    = NA_real_, scrgauss    = NA_real_)
      }

      ys  <- merge(yact, yfit, by = "t", all = TRUE)
      merge(ys, cis, by = "t", all = TRUE)

    }, error = function(e) {
      cat(sprintf("  Error: %s %d: %s\n", ep$iso, Year, conditionMessage(e)))
      NULL
    })

    if (is.null(series)) next

    all_series[[length(all_series) + 1]] <- series
    cat(sprintf("  Done: %s %d (%s)\n", ep$iso, Year, auth_group))
  }

  cat(sprintf("Episodes completed: %d\n", length(all_series)))
  if (length(all_series) == 0) stop("all_series is empty — no episodes succeeded")

  finaldata <- rbindlist(all_series, fill = TRUE)
  finaldata <- finaldata %>%
    group_by(case) %>%
    mutate(ti = t - sta) %>%
    ungroup()

  # Zero out CI at t=0
  for (col in c("sclinsample","scrinsample","sclgauss","scrgauss")) {
    finaldata[[col]][finaldata$ti == 0] <- 0
  }

  finaldata$all <- 1
  finaldata$sp  <- as.integer(finaldata$atype == "Single-party")
  finaldata$mi  <- as.integer(finaldata$atype == "Military")
  finaldata$pe  <- as.integer(finaldata$atype == "Personalist")

  # Helper: aggregate average
  agg <- function(df, group_col = "all") {
    g <- df %>% filter(get(group_col) == 1)
    Reduce(function(x, y) merge(x, y, all = TRUE), list(
      ddply(g, .(ti), summarise, yfit = mean(yfit, na.rm = TRUE)),
      ddply(g, .(ti), summarise, yact = mean(yact, na.rm = TRUE)),
      ddply(g, .(ti), summarise, scrgauss    = mean(scrgauss,    na.rm = TRUE)),
      ddply(g, .(ti), summarise, sclgauss    = mean(sclgauss,    na.rm = TRUE)),
      ddply(g, .(ti), summarise, scrinsample = mean(scrinsample, na.rm = TRUE)),
      ddply(g, .(ti), summarise, sclinsample = mean(sclinsample, na.rm = TRUE))
    ))
  }

  dp_all <- agg(finaldata, "all")
  dp_sp  <- agg(finaldata, "sp")
  dp_mi  <- agg(finaldata, "mi")
  dp_pe  <- agg(finaldata, "pe")

  if (.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times = windowsFont("Times New Roman"))
  })

  # Shared plot theme
  base_theme <- theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title   = element_text(hjust = 0.5, size = 10),
          aspect.ratio = 3/4.25,
          plot.margin  = unit(c(0.06, 0.06, 0.06, 0.06), "cm"),
          panel.border = element_rect(linewidth = 0.3),
          axis.ticks   = element_line(linewidth = 0.3),
          legend.position = "bottom",
          legend.text  = element_text(size = 7),
          legend.margin = margin(-18, 5, 0, -5),
          text = element_text(family = "Times"),
          axis.text = element_text(size = 6))

  make_scm_plot <- function(dp, title_str) {
    ggplot(dp) +
      geom_ribbon(aes(ymin = sclgauss, ymax = scrgauss, x = ti,
                      fill = "90% CI (out-of-sample uncertainty)"), alpha = 1) +
      geom_line(aes(x = ti, y = sclgauss,
                    color = "90% CI (out-of-sample uncertainty)",
                    linetype = "90% CI (out-of-sample uncertainty)",
                    linewidth = "90% CI (out-of-sample uncertainty)")) +
      geom_line(aes(x = ti, y = scrgauss,
                    color = "90% CI (out-of-sample uncertainty)",
                    linetype = "90% CI (out-of-sample uncertainty)",
                    linewidth = "90% CI (out-of-sample uncertainty)")) +
      geom_line(aes(y = yfit, x = ti,
                    colour = "Doppelganger avg.",
                    fill   = "Doppelganger avg.",
                    linetype = "Doppelganger avg.",
                    linewidth = "Doppelganger avg.")) +
      geom_line(aes(y = yact, x = ti,
                    colour = "Authoritarian avg.",
                    fill   = "Authoritarian avg.",
                    linetype = "Authoritarian avg.",
                    linewidth = "Authoritarian avg.")) +
      scale_colour_manual(name = '',
        values = c("Authoritarian avg." = "blue",
                   "Doppelganger avg."  = "blue",
                   "90% CI (out-of-sample uncertainty)" = "grey95")) +
      scale_fill_manual(name = '',
        values = c("Authoritarian avg." = "blue",
                   "Doppelganger avg."  = "blue",
                   "90% CI (out-of-sample uncertainty)" = "grey95")) +
      scale_linetype_manual(name = '',
        values = c("Authoritarian avg." = "solid",
                   "Doppelganger avg."  = "longdash",
                   "90% CI (out-of-sample uncertainty)" = "solid")) +
      scale_linewidth_manual(name = '',
        values = c("Authoritarian avg." = 0.4,
                   "Doppelganger avg."  = 0.4,
                   "90% CI (out-of-sample uncertainty)" = 0.4)) +
      scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
      scale_y_continuous(limits = c(-0.40, 0.60),
                         breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.60),
                         labels = c("-40%","-20%","0%","+20%","+40%","+60%"),
                         expand = c(0.02, 0.02)) +
      labs(title = title_str, x = "", y = "") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2) +
      guides(color = guide_legend(ncol = 1, nrow = 3, keyheight = 0.7,
                                  override.aes = list(fill = NA))) +
      base_theme
  }

  p_all <- make_scm_plot(dp_all, "All authoritarians")
  p_sp  <- make_scm_plot(dp_sp,  "Single-party")
  p_mi  <- make_scm_plot(dp_mi,  "Military")
  p_pe  <- make_scm_plot(dp_pe,  "Personalist")

  # Figure A_6: All authoritarians
  ggsave(file.path("figures", "FigureA_6.pdf"),
         p_all, width = 8, height = 6, units = "cm")
  cat("FigureA_6.pdf saved.\n")

  # Figure A_7: By type (three panels)
  pdf(file.path("figures", "FigureA_7.pdf"), width = 18 / 2.54, height = 6 / 2.54)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
  print(p_sp, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_mi, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_pe, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
  dev.off()
  cat("FigureA_7.pdf saved.\n")

  rm(list = ls(pattern = "^_"))

}, silent = FALSE)

# Note: do NOT remove data/auth_dataset.dta — it lives in auth_extension/data/
cat("ranscm_auth.R complete.\n")
