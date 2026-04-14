# ranscm_auth_placebo.R
# Authoritarian Leaders Extension — Time Placebo Tests
# Analogue of FST (2023) Figure 8 for authoritarian leaders
#
# Uses a 10-year pre-window (t=-15 to t=-5) and tests from t=-5 onward.
# If SCM is valid, the "placebo post-period" (t=-5 to t=0) should show no gap.
#
# Run from: C:\PLE\auth_extension\programs\
# Outputs:
#   figures/FigureA_8.pdf  — Time placebo all auth + by type (3 panels)

renv::restore(prompt = FALSE)
devtools::install_github('ebenmichael/augsynth', upgrade = "never")

library(LowRankQP)
library(devtools)
library(zoo)
library(haven)
library(data.table)
library(scpi)
library(augsynth)
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
setwd('..')

auth_data <- read_dta("data/auth_dataset.dta")
episodes  <- read_csv("data/authoritarian_episodes_scm_viable.csv",
                      show_col_types = FALSE)
episodes <- episodes %>%
  filter(is_also_populist == 0) %>%
  filter(!is.na(cid))

cat("Episodes for placebo:", nrow(episodes), "\n")

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
sta               <- 15   # full pre-window (used for data load)

type_sp <- "Single-party"
type_mi <- c("Military", "Military-personal")
type_pe <- c("Personalist", "Oligarchy", "Monarchy")

##############################################################
# Figure A_8: Time placebo (pre-window = t=-15 to t=-5)
##############################################################

try({

  all_series <- list()

  for (k in seq_len(nrow(episodes))) {

    ep    <- episodes[k, ]
    Oldc  <- ep$cid
    Year  <- ep$start_yr
    atype <- ep$auth_type

    auth_group <- if (atype %in% type_sp) "Single-party" else
                  if (atype %in% type_mi) "Military"      else
                  "Personalist"

    # Placebo: use t=-15 to t=-5 as pre, t=-5 to t=+15 as post
    # Load same data window as main figure
    data <- auth_data
    data <- data[data$year >= Year - sta & data$year <= Year + 15, ]

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
      filter(all(!is.na(fstgdp)) | cid == Oldc) %>%
      ungroup()

    if (n_distinct(data$cid) < 5) next

    data$lgfstgdp <- log(data$fstgdp)

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
    data <- data %>% mutate(d = replace(d, war == 1, NA))

    # Placebo: pre-period = t=0..10 (years -15 to -5 in relative time)
    #          post-period = t=11..30 (years -4 to +15)
    avail_pre_all <- sum(data$cid == Oldc & !is.na(data$d) &
                         data$t >= 0 & data$t <= 10)
    if (avail_pre_all < 5) next

    period.pre.pl  <- seq(0, 10)   # t-15 to t-5 in absolute time
    period.post.pl <- seq(11, 30)  # t-4 onwards

    Trea <- unique(data$index[data$cid == Oldc])
    if (length(Trea) == 0) next

    series <- tryCatch({
      df  <- scdata(df = data, features = features, constant = constant,
                    cov.adj = cov.adj, cointegrated.data = cointegrated.data,
                    id.var = "index", time.var = "t", outcome.var = "d",
                    period.pre = period.pre.pl, period.post = period.post.pl,
                    unit.tr = Trea,
                    unit.co = unique(data$index[data$cid != Oldc]))
      res <- scpi(data = df, u.order = u.order, u.lags = u.lags,
                  u.sigma = u.sigma, u.missp = u.missp,
                  e.order = e.order, e.lags = e.lags,
                  u.alpha = u.alpha, e.alpha = e.alpha,
                  rho = rho, rho.max = rho.max,
                  sims = sims, w.constr = w.constr,
                  cores = cores, e.method = e.method)

      y.pre.act  <- res$data$Y.pre
      y.post.act <- res$data$Y.post
      n_pre_r  <- nrow(y.pre.act)
      n_post_r <- nrow(y.post.act)
      if (n_pre_r < 1 || n_post_r < 1) stop("Empty Y from scpi")
      t_pre_r  <- suppressWarnings(as.numeric(rownames(y.pre.act)))
      t_post_r <- suppressWarnings(as.numeric(rownames(y.post.act)))
      if (any(is.na(t_pre_r)))  t_pre_r  <- period.pre.pl[seq_len(n_pre_r)]
      if (any(is.na(t_post_r))) t_post_r <- period.post.pl[seq_len(n_post_r)]

      yact <- data.frame(t = c(t_pre_r, t_post_r),
                         yact  = c(rbind(y.pre.act, y.post.act)),
                         case  = paste0(ep$iso, Year),
                         atype = auth_group,
                         iso   = ep$iso)
      yfit <- data.frame(t = c(t_pre_r, t_post_r),
                         yfit = c(rbind(res$est.results$Y.pre.fit,
                                        res$est.results$Y.post.fit)))

      ci_g <- as.matrix(res$inference.results$CI.all.gaussian)
      if (is.null(ci_g) || ncol(ci_g) < 2) stop("CI.all.gaussian bad dims")
      n_ci <- min(nrow(ci_g), n_post_r)
      if (n_ci < 1) stop("No CI rows")
      cis <- data.frame(t        = t_post_r[seq_len(n_ci)],
                        sclgauss = c(ci_g[seq_len(n_ci), 1]),
                        scrgauss = c(ci_g[seq_len(n_ci), 2]))

      ys <- merge(yact, yfit, by = "t", all = TRUE)
      merge(ys, cis, by = "t", all = TRUE)

    }, error = function(e) {
      cat(sprintf("  Error: %s %d: %s\n", ep$iso, Year, conditionMessage(e)))
      NULL
    })

    if (is.null(series)) next
    all_series[[length(all_series) + 1]] <- series
    cat(sprintf("  Done: %s %d (%s)\n", ep$iso, Year, auth_group))
  }

  cat(sprintf("Placebo episodes completed: %d\n", length(all_series)))
  if (length(all_series) == 0) stop("No placebo episodes succeeded")

  finaldata <- rbindlist(all_series, fill = TRUE)
  finaldata <- finaldata %>%
    group_by(case) %>%
    mutate(ti = t - sta) %>%   # re-centre: ti=0 is actual treatment
    ungroup()

  # Placebo treatment at ti = -5
  finaldata$sclgauss[finaldata$ti == -5] <- 0
  finaldata$scrgauss[finaldata$ti == -5] <- 0

  finaldata$all <- 1
  finaldata$sp  <- as.integer(finaldata$atype == "Single-party")
  finaldata$mi  <- as.integer(finaldata$atype == "Military")
  finaldata$pe  <- as.integer(finaldata$atype == "Personalist")

  agg <- function(df, group_col = "all") {
    g <- df %>% filter(get(group_col) == 1)
    Reduce(function(x, y) merge(x, y, all = TRUE), list(
      ddply(g, .(ti), summarise, yfit     = mean(yfit,     na.rm = TRUE)),
      ddply(g, .(ti), summarise, yact     = mean(yact,     na.rm = TRUE)),
      ddply(g, .(ti), summarise, scrgauss = mean(scrgauss, na.rm = TRUE)),
      ddply(g, .(ti), summarise, sclgauss = mean(sclgauss, na.rm = TRUE))
    ))
  }

  dp_all <- agg(finaldata, "all")
  dp_sp  <- agg(finaldata, "sp")
  dp_mi  <- agg(finaldata, "mi")
  dp_pe  <- agg(finaldata, "pe")

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

  make_placebo_plot <- function(dp, title_str) {
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
                         labels = c("-40%", "-20%", "0%", "+20%", "+40%", "+60%"),
                         expand = c(0.02, 0.02)) +
      labs(title = title_str, x = "", y = "") +
      geom_vline(xintercept =  0, linetype = "dashed", linewidth = 0.2) +
      geom_vline(xintercept = -5, linetype = "solid",  linewidth = 0.2) +
      guides(color = guide_legend(ncol = 1, nrow = 3, keyheight = 0.7,
                                  override.aes = list(fill = NA))) +
      base_theme
  }

  p_all <- make_placebo_plot(dp_all, "All authoritarians")
  p_sp  <- make_placebo_plot(dp_sp,  "Single-party")
  p_mi  <- make_placebo_plot(dp_mi,  "Military")
  p_pe  <- make_placebo_plot(dp_pe,  "Personalist")

  # Figure A_8: All (single panel)
  ggsave(file.path("figures", "FigureA_8.pdf"),
         p_all, width = 8, height = 6, units = "cm")
  cat("FigureA_8.pdf saved.\n")

  # Also save by-type version as A_8b (not a required figure, but useful)
  pdf(file.path("figures", "FigureA_8_bytype.pdf"), width = 18 / 2.54, height = 6 / 2.54)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
  print(p_sp, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_mi, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_pe, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
  dev.off()
  cat("FigureA_8_bytype.pdf saved.\n")

  rm(list = ls(pattern = "^_"))

}, silent = FALSE)

cat("ranscm_auth_placebo.R complete.\n")
