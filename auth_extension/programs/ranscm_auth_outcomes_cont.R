# ranscm_auth_outcomes_cont.R
# Continuation: FigureA_13 and FigureA_14 only
# (A_11 and A_12 already produced)
#
# Run from: C:\PLE\auth_extension\programs\
# Outputs:
#   figures/FigureA_13.pdf  — Debt/GDP + inflation + banking crisis
#   figures/FigureA_14.pdf  — Institutional quality

renv::restore(prompt = FALSE)

library(zoo)
library(haven)
library(data.table)
library(scpi)
library(plyr)
library(purrr)
library(dplyr)
library(patchwork)
library(tidyr)
library(reshape2)
library(ggplot2)
library(tibble)
library(readr)

rm(list = ls(all = TRUE))
setwd('..')

# ============================================================
# Shared setup
# ============================================================

auth_data <- read_dta("data/auth_dataset.dta")
episodes  <- read_csv("data/authoritarian_episodes_scm_viable.csv",
                      show_col_types = FALSE)
episodes <- episodes %>%
  filter(is_also_populist == 0) %>%
  filter(!is.na(cid))

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

# ============================================================
# Helper: run SCM for one outcome variable
# ============================================================

extract_ci <- function(m) {
  m2 <- tryCatch(as.matrix(m), error = function(e) NULL)
  if (is.null(m2) || NROW(m2) == 0 || NCOL(m2) < 2) return(NULL)
  m2
}

run_outcome_scm <- function(varname) {
  all_series <- list()
  for (k in seq_len(nrow(episodes))) {
    ep    <- episodes[k, ]
    Oldc  <- ep$cid
    Year  <- ep$start_yr

    data <- auth_data
    data <- data[data$year >= Year - sta & data$year <= Year + 15, ]
    if (!varname %in% names(data)) next
    data$var <- data[[varname]]

    taker  <- data %>% filter(cid == Oldc)
    donors <- data %>% filter(cid != Oldc)
    donors <- donors %>%
      mutate(simul = ifelse(atakeover_auth == 1 & year == Year, 1, 0)) %>%
      group_by(cid) %>%
      mutate(msimul = max(as.numeric(simul))) %>%
      filter(msimul != 1) %>%
      select(-simul, -msimul) %>%
      ungroup()

    data <- rbind(taker, donors)

    data <- data %>%
      group_by(cid) %>%
      filter((all(!is.na(var)) & all(!is.na(fstgdp))) | cid == Oldc) %>%
      ungroup()

    if (n_distinct(data$cid) < 5) next

    taker_var <- data %>% filter(cid == Oldc) %>% pull(var)
    if (sum(!is.na(taker_var)) < 5) next

    tysub <- data %>%
      filter(year == Year) %>%
      select(cid, var) %>%
      rename(ivar = var)
    if (nrow(tysub) == 0 || is.na(tysub$ivar[tysub$cid == Oldc])) next
    data <- merge(data, tysub)
    data <- data %>%
      group_by(cid) %>%
      mutate(d = var - ivar,
             t = year - Year + sta) %>%
      ungroup()
    data <- transform(data, index = as.numeric(factor(cid)))
    data <- data %>% mutate(d = replace(d, war == 1, NA))

    avail_pre <- sum(data$year < Year & data$cid == Oldc & !is.na(data$d))
    if (avail_pre < 5) next

    fr1 <- max(0, sta - avail_pre)
    fr2 <- sta - 1
    fr3 <- sta
    period.pre  <- seq(fr1, fr2)
    period.post <- seq(fr3, sta + 15)

    Trea <- unique(data$index[data$cid == Oldc])
    if (length(Trea) == 0) next

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

      y.pre.act  <- as.matrix(res$data$Y.pre)
      y.post.act <- as.matrix(res$data$Y.post)
      n_pre_r  <- NROW(y.pre.act)
      n_post_r <- NROW(y.post.act)
      if (n_pre_r < 1L || n_post_r < 1L) stop("Empty Y from scpi")

      t_pre_r  <- suppressWarnings(as.numeric(rownames(y.pre.act)))
      t_post_r <- suppressWarnings(as.numeric(rownames(y.post.act)))
      if (length(t_pre_r)  == 0 || any(is.na(t_pre_r)))
        t_pre_r  <- period.pre[seq_len(n_pre_r)]
      if (length(t_post_r) == 0 || any(is.na(t_post_r)))
        t_post_r <- period.post[seq_len(n_post_r)]

      y.pre.fit  <- as.matrix(res$est.results$Y.pre.fit)
      y.post.fit <- as.matrix(res$est.results$Y.post.fit)

      yact <- data.frame(t    = c(t_pre_r, t_post_r),
                         yact = c(y.pre.act,  y.post.act),
                         case = paste0(ep$iso, Year))
      yfit <- data.frame(t    = c(t_pre_r, t_post_r),
                         yfit = c(y.pre.fit, y.post.fit))

      ci_g <- extract_ci(res$inference.results$CI.all.gaussian)
      if (!is.null(ci_g)) {
        n_ci <- min(NROW(ci_g), n_post_r)
        cis  <- data.frame(t        = t_post_r[seq_len(n_ci)],
                           sclgauss = as.numeric(ci_g[seq_len(n_ci), 1]),
                           scrgauss = as.numeric(ci_g[seq_len(n_ci), 2]))
      } else {
        cis <- data.frame(t        = t_post_r,
                          sclgauss = NA_real_,
                          scrgauss = NA_real_)
      }

      ys <- merge(yact, yfit, by = "t", all = TRUE)
      merge(ys, cis, by = "t", all = TRUE)

    }, error = function(e) {
      cat(sprintf("  Error [%s] %s %d: %s\n", varname, ep$iso, Year,
                  conditionMessage(e)))
      NULL
    })

    if (is.null(series)) next
    all_series[[length(all_series) + 1]] <- series
    cat(sprintf("  Done [%s]: %s %d\n", varname, ep$iso, Year))
  }
  all_series
}

# ============================================================
# Shared plot helpers
# ============================================================

if (.Platform$OS.type == "windows") withAutoprint({
  windowsFonts(Times = windowsFont("Times New Roman"))
})

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

make_gap_plot <- function(all_series, title_str, ylim, ybreaks, ylabels) {
  if (length(all_series) == 0) return(NULL)
  fd <- rbindlist(all_series, fill = TRUE) %>%
    group_by(case) %>%
    mutate(ti = t - sta) %>%
    ungroup()
  fd$sclgauss[fd$ti == 0] <- 0
  fd$scrgauss[fd$ti == 0] <- 0

  dp <- Reduce(function(x, y) merge(x, y, all = TRUE), list(
    ddply(fd, .(ti), summarise, yfit = mean(yfit, na.rm = TRUE)),
    ddply(fd, .(ti), summarise, yact = mean(yact, na.rm = TRUE)),
    ddply(fd, .(ti), summarise, scrgauss = mean(scrgauss, na.rm = TRUE)),
    ddply(fd, .(ti), summarise, sclgauss = mean(sclgauss, na.rm = TRUE))
  ))

  ggplot(dp) +
    geom_ribbon(aes(ymin = ((yfit + sclgauss) - yact) * (-1),
                    ymax = ((yfit + scrgauss) - yact) * (-1),
                    x = ti, fill = "90% CI (out-of-sample uncertainty)"),
                alpha = 1) +
    geom_line(aes(x = ti,
                  y = ((yfit + sclgauss) - yact) * (-1),
                  color = "90% CI (out-of-sample uncertainty)",
                  linewidth = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x = ti,
                  y = ((yfit + scrgauss) - yact) * (-1),
                  color = "90% CI (out-of-sample uncertainty)",
                  linewidth = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y = (yfit - yact) * (-1), x = ti,
                  colour = "Doppelganger gap (avg.)",
                  linewidth = "Doppelganger gap (avg.)")) +
    scale_colour_manual(name = '',
      values = c("Doppelganger gap (avg.)" = "blue",
                 "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',
      values = c("Doppelganger gap (avg.)" = "blue",
                 "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linewidth_manual(name = '',
      values = c("Doppelganger gap (avg.)" = 0.4,
                 "90% CI (out-of-sample uncertainty)" = 0.4)) +
    scale_x_continuous(breaks = seq(-15, 15, 5), expand = c(0.02, 0.02)) +
    scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels,
                       expand = c(0.02, 0.02)) +
    labs(title = title_str, x = "", y = "") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2) +
    guides(color = guide_legend(ncol = 1, nrow = 2, keyheight = 0.7,
                                override.aes = list(fill = NA))) +
    base_theme
}

##############################################################
# Figure A_13: Debt/GDP + Inflation + Banking crisis
##############################################################

try({
  cat("\n=== FigureA_13: debtgdp + inflation + bankcrisis ===\n")
  s_debt <- run_outcome_scm("debtgdp")
  s_inf  <- run_outcome_scm("inflation")
  s_bank <- run_outcome_scm("bankcrisis")
  cat(sprintf("debtgdp: %d, inflation: %d, bankcrisis: %d\n",
              length(s_debt), length(s_inf), length(s_bank)))

  p_debt <- make_gap_plot(s_debt, "Debt/GDP",
    ylim   = c(-0.20, 0.20),
    ybreaks = seq(-0.20, 0.20, 0.10),
    ylabels = c("-20 pp", "-10 pp", "0 pp", "+10 pp", "+20 pp"))

  p_inf <- make_gap_plot(s_inf, "Inflation",
    ylim   = c(-0.10, 0.10),
    ybreaks = seq(-0.10, 0.10, 0.05),
    ylabels = c("-10 pp", "-5 pp", "0 pp", "+5 pp", "+10 pp"))

  p_bank <- make_gap_plot(s_bank, "Banking crisis",
    ylim   = c(-0.20, 0.20),
    ybreaks = seq(-0.20, 0.20, 0.10),
    ylabels = c("-20 pp", "-10 pp", "0 pp", "+10 pp", "+20 pp"))

  plots13 <- Filter(Negate(is.null), list(p_debt, p_inf, p_bank))
  if (length(plots13) >= 1) {
    n_panels <- length(plots13)
    pdf(file.path("figures", "FigureA_13.pdf"),
        width = (if (n_panels == 3) 18 else 15) / 2.54, height = 6 / 2.54)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, n_panels)))
    for (j in seq_len(n_panels))
      print(plots13[[j]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = j))
    dev.off()
    cat("FigureA_13.pdf saved.\n")
  }
}, silent = FALSE)

##############################################################
# Figure A_14: Institutional quality
##############################################################

try({
  cat("\n=== FigureA_14: institutions ===\n")
  s_inst <- run_outcome_scm("institutions")
  cat(sprintf("institutions: %d episodes\n", length(s_inst)))

  p_inst <- make_gap_plot(s_inst, "Institutional quality",
    ylim   = c(-0.30, 0.30),
    ybreaks = seq(-0.30, 0.30, 0.10),
    ylabels = c("-30 pp", "-20 pp", "-10 pp", "0 pp", "+10 pp", "+20 pp", "+30 pp"))

  if (!is.null(p_inst)) {
    ggsave(file.path("figures", "FigureA_14.pdf"),
           p_inst, width = 8, height = 6, units = "cm")
    cat("FigureA_14.pdf saved.\n")
  }
}, silent = FALSE)

cat("ranscm_auth_outcomes_cont.R complete.\n")
