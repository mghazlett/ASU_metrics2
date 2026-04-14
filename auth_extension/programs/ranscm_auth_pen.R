# ranscm_auth_pen.R
# Authoritarian Leaders Extension — Penalization Robustness
# Analogue of FST (2023) Figure 9 for authoritarian leaders
#
# Shows GDP paths with no penalty, fixed penalty (0.1), and optimal penalty
# (lambda minimizing average pre-period RMSE across episodes).
# Does NOT use scpi — uses penalized ridge-style SCM via LowRankQP.
#
# Run from: C:\PLE\auth_extension\programs\
# Outputs:
#   figures/FigureA_9.pdf   — GDP paths under optimal penalty (all + by type)

renv::restore(prompt = FALSE)

library(LowRankQP)
library(haven)
library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(plyr)
library(patchwork)
library(ggplot2)
library(tibble)
library(readr)

rm(list = ls(all = TRUE))
setwd('..')

# ============================================================
# Penalized SCM helpers (identical to FST ranscm.R Figure 9)
# ============================================================

wsoll1 <- function(X0, X1, V, pen = 0.0) {
  n     <- ncol(X0)
  Delta <- diag(t(X0 - matrix(rep(1, n), ncol = n) %x% X1) %*%
                V %*%
                (X0 - matrix(rep(1, n), ncol = n) %x% X1))
  P <- 2 * t(X0) %*% V %*% X0
  q <- t(-2 * t(X0) %*% V %*% X1 + pen * Delta)
  sol <- LowRankQP(Vmat = P, dvec = q, Amat = matrix(1, ncol = n),
                   bvec = 1, uvec = rep(1, n), method = "LU")
  return(sol$alpha)
}

TZero <- function(x, tol = 1e-6, scale = TRUE) {
  if (!all(x > 0)) stop("Some elements are negative!")
  y <- ifelse(x < tol, 0, x)
  if (scale) y <- y / sum(y)
  return(y)
}

regsynth <- function(X0, X1, Y0, Y1, V, pen, tol = 1e-6) {
  sol <- wsoll1(X0, X1, V, pen)
  sol <- TZero(sol, tol)
  sol <- round(sol, 3)
  return(sol)
}

# ============================================================
# Load data and episodes
# ============================================================

auth_data <- read_dta("data/auth_dataset.dta")
auth_data$lgfstgdp <- log(auth_data$fstgdp)

episodes <- read_csv("data/authoritarian_episodes_scm_viable.csv",
                     show_col_types = FALSE)
episodes <- episodes %>%
  filter(is_also_populist == 0) %>%
  filter(!is.na(cid))

type_sp <- "Single-party"
type_mi <- c("Military", "Military-personal")
type_pe <- c("Personalist", "Oligarchy", "Monarchy")

cat("Episodes:", nrow(episodes), "\n")

# ============================================================
# Lambda grid search
# ============================================================

try({

  lambda <- c(0, .00001, .01, .1, .15, seq(.25, 5, .1))
  n_ep   <- nrow(episodes)

  curve_RMSE <- matrix(NA, nrow = n_ep, ncol = length(lambda))
  curve_bias <- matrix(NA, nrow = n_ep, ncol = length(lambda))

  cat("Running lambda grid...\n")

  for (l in seq_along(lambda)) {
    for (k in seq_len(n_ep)) {

      ep   <- episodes[k, ]
      i    <- ep$cid
      date <- ep$start_yr

      # Other auth takeovers in same year
      other_auth <- episodes$cid[episodes$start_yr == date & episodes$cid != i]

      scm_data <- auth_data[auth_data$year >= date - 15 &
                             auth_data$year <= date + 15, ]
      scm_data <- scm_data[!(scm_data$cid %in% other_auth), ]

      # Drop donors with any missing lgfstgdp
      na_cid <- scm_data$cid[is.na(scm_data$lgfstgdp)]
      na_cid <- unique(na_cid[na_cid != i])
      scm_data <- scm_data[!(scm_data$cid %in% na_cid) | scm_data$cid == i, ]

      if (n_distinct(scm_data$cid) < 5) next

      # Normalise to takeover year
      scm_data <- scm_data %>%
        group_by(cid) %>%
        mutate(Y = lgfstgdp - lgfstgdp[year == date][1]) %>%
        ungroup()

      scm_pre <- scm_data[scm_data$year < date, c("cid", "year", "Y")]
      if (nrow(scm_pre) == 0) next

      scm_all <- scm_data[, c("cid", "year", "Y")]
      tryCatch({
        scm_mat_x <- as.matrix(dcast(setDT(scm_pre),
                                     cid ~ year, value.var = "Y"))
        scm_mat_y <- as.matrix(
          spread(scm_all, year, Y) %>% arrange(cid))

        X_0 <- t(subset(scm_mat_x, scm_mat_x[, "cid"] != i))[- 1, , drop = FALSE]
        X_1 <- t(subset(scm_mat_x, scm_mat_x[, "cid"] == i))[-1]
        Y_0 <- subset(scm_mat_y, scm_mat_y[, "cid"] != i)[, -1, drop = FALSE]
        Y_1 <- subset(scm_mat_y, scm_mat_y[, "cid"] == i)[-1]

        if (ncol(X_0) < 2) next
        if (any(is.na(X_0)) || any(is.na(X_1))) next

        V   <- diag(nrow(X_0))
        W   <- regsynth(X_0, X_1, Y_0, Y_1, V, pen = lambda[l])
        Y_0hat <- t(t(W) %*% Y_0)

        # Pre-period indices: columns of Y_1 with year < date
        years_all <- as.numeric(colnames(scm_mat_y)[-1])
        pre_idx <- which(years_all < date)
        if (length(pre_idx) == 0) next

        tau <- Y_1[pre_idx] - Y_0hat[pre_idx]
        curve_RMSE[k, l] <- sqrt(mean(tau^2, na.rm = TRUE))
        curve_bias[k, l] <- abs(mean(tau, na.rm = TRUE))
      }, error = function(e) NULL)
    }
    cat(sprintf("  lambda[%d]=%.4f done\n", l, lambda[l]))
  }

  mean_RMSE     <- apply(curve_RMSE, 2, mean, na.rm = TRUE)
  mean_bias     <- apply(curve_bias, 2, mean, na.rm = TRUE)
  lambda_opt    <- min(lambda[which(mean_RMSE == min(mean_RMSE, na.rm = TRUE))])
  lambda_fixed  <- 0.1
  lambda_values <- c(0, lambda_fixed, lambda_opt)
  cat(sprintf("Optimal lambda (RMSE): %.4f\n", lambda_opt))

  # ============================================================
  # Compute GDP paths under three lambdas
  # ============================================================

  build_paths <- function(lam) {
    Y_synth <- data.frame(Y0hat = numeric(), Y1 = numeric(),
                          time = numeric(), atype = character())
    for (k in seq_len(n_ep)) {
      ep   <- episodes[k, ]
      i    <- ep$cid
      date <- ep$start_yr
      atype_raw <- ep$auth_type
      auth_group <- if (atype_raw %in% type_sp) "Single-party" else
                    if (atype_raw %in% type_mi) "Military"      else
                    "Personalist"

      other_auth <- episodes$cid[episodes$start_yr == date & episodes$cid != i]
      scm_data <- auth_data[auth_data$year >= date - 15 &
                             auth_data$year <= date + 15, ]
      scm_data <- scm_data[!(scm_data$cid %in% other_auth), ]
      na_cid <- unique(scm_data$cid[is.na(scm_data$lgfstgdp) &
                                    scm_data$cid != i])
      scm_data <- scm_data[!(scm_data$cid %in% na_cid) | scm_data$cid == i, ]
      if (n_distinct(scm_data$cid) < 5) next

      scm_data <- scm_data %>%
        group_by(cid) %>%
        mutate(Y = lgfstgdp - lgfstgdp[year == date][1]) %>%
        ungroup()

      scm_pre <- scm_data[scm_data$year < date, c("cid", "year", "Y")]
      scm_all <- scm_data[, c("cid", "year", "Y")]

      tryCatch({
        scm_mat_x <- as.matrix(dcast(setDT(scm_pre), cid ~ year, value.var = "Y"))
        scm_mat_y <- as.matrix(spread(scm_all, year, Y) %>% arrange(cid))
        X_0 <- t(subset(scm_mat_x, scm_mat_x[,"cid"] != i))[-1, , drop=FALSE]
        X_1 <- t(subset(scm_mat_x, scm_mat_x[,"cid"] == i))[-1]
        Y_0 <- subset(scm_mat_y, scm_mat_y[,"cid"] != i)[, -1, drop=FALSE]
        Y_1 <- subset(scm_mat_y, scm_mat_y[,"cid"] == i)[-1]
        if (ncol(X_0) < 2 || any(is.na(X_0)) || any(is.na(X_1))) next
        V     <- diag(nrow(X_0))
        W     <- regsynth(X_0, X_1, Y_0, Y_1, V, pen = lam)
        Y_0hat <- t(t(W) %*% Y_0)
        years <- as.numeric(colnames(scm_mat_y)[-1])
        time  <- years - date
        Y <- data.frame(Y0hat = as.numeric(Y_0hat),
                        Y1    = as.numeric(Y_1),
                        time  = time,
                        atype = auth_group)
        Y_synth <- rbind(Y_synth, Y)
      }, error = function(e) NULL)
    }
    Y_synth
  }

  cat("Building paths: lambda=0\n");        paths_none <- build_paths(0)
  cat("Building paths: lambda=0.1\n");      paths_fixed <- build_paths(0.1)
  cat(sprintf("Building paths: lambda=%.4f\n", lambda_opt))
  paths_opt  <- build_paths(lambda_opt)

  # Use optimal-penalty paths for the figure
  paths_opt$all <- 1
  paths_opt$sp  <- as.integer(paths_opt$atype == "Single-party")
  paths_opt$mi  <- as.integer(paths_opt$atype == "Military")
  paths_opt$pe  <- as.integer(paths_opt$atype == "Personalist")

  avg <- function(df, grp) {
    df_g <- df[!is.na(df[[grp]]) & df[[grp]] == 1, ]
    merge(
      ddply(df_g, .(time), plyr::summarise, Y0hat = mean(Y0hat, na.rm = TRUE)),
      ddply(df_g, .(time), plyr::summarise, Y1    = mean(Y1,    na.rm = TRUE)),
      by = "time", all = TRUE
    )
  }
  dp_all <- avg(paths_opt, "all")
  dp_sp  <- avg(paths_opt, "sp")
  dp_mi  <- avg(paths_opt, "mi")
  dp_pe  <- avg(paths_opt, "pe")

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

  make_pen_plot <- function(dp, title_str) {
    ggplot(dp) +
      geom_line(aes(y = Y0hat, x = time,
                    colour = "Doppelganger avg.", fill = "Doppelganger avg.",
                    linetype = "Doppelganger avg.", linewidth = "Doppelganger avg.")) +
      geom_line(aes(y = Y1, x = time,
                    colour = "Authoritarian avg.", fill = "Authoritarian avg.",
                    linetype = "Authoritarian avg.", linewidth = "Authoritarian avg.")) +
      scale_colour_manual(name = '',
        values = c("Authoritarian avg." = "blue", "Doppelganger avg." = "blue")) +
      scale_fill_manual(name = '',
        values = c("Authoritarian avg." = "blue", "Doppelganger avg." = "blue")) +
      scale_linetype_manual(name = '',
        values = c("Authoritarian avg." = "solid", "Doppelganger avg." = "longdash")) +
      scale_linewidth_manual(name = '',
        values = c("Authoritarian avg." = 0.4, "Doppelganger avg." = 0.4)) +
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

  p_all <- make_pen_plot(dp_all, sprintf("All auth. (λ=%.3f)", lambda_opt))
  p_sp  <- make_pen_plot(dp_sp,  "Single-party")
  p_mi  <- make_pen_plot(dp_mi,  "Military")
  p_pe  <- make_pen_plot(dp_pe,  "Personalist")

  pdf(file.path("figures", "FigureA_9.pdf"), width = 18 / 2.54, height = 6 / 2.54)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
  print(p_sp, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_mi, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_pe, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
  dev.off()
  cat("FigureA_9.pdf saved.\n")

  rm(list = ls(pattern = "^_"))

}, silent = FALSE)

cat("ranscm_auth_pen.R complete.\n")
