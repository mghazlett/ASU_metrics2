# ranscm_auth_outcomes_a11a12.R
# Produce FigureA_11 (Gini + labor share) and FigureA_12 (trade/openness)
# using grid.layout instead of patchwork (avoids patchwork linewidth bug)
#
# Run from: C:\PLE\auth_extension\programs\

renv::restore(prompt = FALSE)

library(zoo)
library(haven)
library(data.table)
library(scpi)
library(plyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(tibble)
library(readr)
library(grid)

rm(list = ls(all = TRUE))
setwd('..')

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

    tysub <- data %>% filter(year == Year) %>% select(cid, var) %>% rename(ivar = var)
    if (nrow(tysub) == 0 || is.na(tysub$ivar[tysub$cid == Oldc])) next
    data <- merge(data, tysub)
    data <- data %>%
      group_by(cid) %>%
      mutate(d = var - ivar, t = year - Year + sta) %>%
      ungroup()
    data <- transform(data, index = as.numeric(factor(cid)))
    data <- data %>% mutate(d = replace(d, war == 1, NA))

    avail_pre <- sum(data$year < Year & data$cid == Oldc & !is.na(data$d))
    if (avail_pre < 5) next

    fr1 <- max(0, sta - avail_pre)
    period.pre  <- seq(fr1, sta - 1)
    period.post <- seq(sta, sta + 15)
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
      if (n_pre_r < 1L || n_post_r < 1L) stop("Empty Y")

      t_pre_r  <- suppressWarnings(as.numeric(rownames(y.pre.act)))
      t_post_r <- suppressWarnings(as.numeric(rownames(y.post.act)))
      if (length(t_pre_r) == 0 || any(is.na(t_pre_r)))   t_pre_r  <- period.pre[seq_len(n_pre_r)]
      if (length(t_post_r) == 0 || any(is.na(t_post_r))) t_post_r <- period.post[seq_len(n_post_r)]

      y.pre.fit  <- as.matrix(res$est.results$Y.pre.fit)
      y.post.fit <- as.matrix(res$est.results$Y.post.fit)

      yact <- data.frame(t = c(t_pre_r, t_post_r),
                         yact = c(y.pre.act, y.post.act),
                         case = paste0(ep$iso, Year))
      yfit <- data.frame(t = c(t_pre_r, t_post_r),
                         yfit = c(y.pre.fit, y.post.fit))

      ci_g <- extract_ci(res$inference.results$CI.all.gaussian)
      if (!is.null(ci_g)) {
        n_ci <- min(NROW(ci_g), n_post_r)
        cis  <- data.frame(t = t_post_r[seq_len(n_ci)],
                           sclgauss = as.numeric(ci_g[seq_len(n_ci), 1]),
                           scrgauss = as.numeric(ci_g[seq_len(n_ci), 2]))
      } else {
        cis <- data.frame(t = t_post_r, sclgauss = NA_real_, scrgauss = NA_real_)
      }
      ys <- merge(yact, yfit, by = "t", all = TRUE)
      merge(ys, cis, by = "t", all = TRUE)
    }, error = function(e) {
      cat(sprintf("  Error [%s] %s %d: %s\n", varname, ep$iso, Year, conditionMessage(e)))
      NULL
    })
    if (is.null(series)) next
    all_series[[length(all_series) + 1]] <- series
    cat(sprintf("  Done [%s]: %s %d\n", varname, ep$iso, Year))
  }
  all_series
}

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

make_gap_plot <- function(all_series, title_str, ylim, ybreaks, ylabels) {
  if (length(all_series) == 0) return(NULL)
  fd <- rbindlist(all_series, fill = TRUE) %>%
    group_by(case) %>% mutate(ti = t - sta) %>% ungroup()
  fd$sclgauss[fd$ti == 0] <- 0
  fd$scrgauss[fd$ti == 0] <- 0
  dp <- Reduce(function(x, y) merge(x, y, all = TRUE), list(
    ddply(fd, .(ti), plyr::summarise, yfit     = mean(yfit,     na.rm = TRUE)),
    ddply(fd, .(ti), plyr::summarise, yact     = mean(yact,     na.rm = TRUE)),
    ddply(fd, .(ti), plyr::summarise, scrgauss = mean(scrgauss, na.rm = TRUE)),
    ddply(fd, .(ti), plyr::summarise, sclgauss = mean(sclgauss, na.rm = TRUE))
  ))
  ggplot(dp) +
    geom_ribbon(aes(ymin = ((yfit + sclgauss) - yact) * (-1),
                    ymax = ((yfit + scrgauss) - yact) * (-1),
                    x = ti, fill = "90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x = ti, y = ((yfit + sclgauss) - yact) * (-1),
                  color = "90% CI (out-of-sample uncertainty)",
                  linewidth = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x = ti, y = ((yfit + scrgauss) - yact) * (-1),
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

save_panels <- function(plots, fname, width_cm, height_cm = 7) {
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) { cat(fname, "skipped (no data)\n"); return(invisible(NULL)) }
  n <- length(plots)
  pdf(file.path("figures", fname), width = width_cm / 2.54, height = height_cm / 2.54)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, n)))
  for (j in seq_len(n))
    print(plots[[j]], vp = viewport(layout.pos.row = 1, layout.pos.col = j))
  dev.off()
  cat(fname, "saved.\n")
}

##############################################################
# Figure A_11: Gini + Labor share
##############################################################
try({
  cat("\n=== FigureA_11: gini + laborshare ===\n")
  s_gini <- run_outcome_scm("gini")
  s_ls   <- run_outcome_scm("laborshare")
  cat(sprintf("gini: %d, laborshare: %d\n", length(s_gini), length(s_ls)))

  p_gini <- make_gap_plot(s_gini, "Gini index",
    c(-3, 3), c(-3,-2,-1,0,1,2,3),
    c("-3 pt","-2 pt","-1 pt","0 pt","+1 pt","+2 pt","+3 pt"))
  p_ls <- make_gap_plot(s_ls, "Labor share",
    c(-0.03,0.03), c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03),
    c("-3 pp","-2 pp","-1 pp","0 pp","+1 pp","+2 pp","+3 pp"))

  save_panels(list(p_gini, p_ls), "FigureA_11.pdf", 15, 7)
}, silent = FALSE)

##############################################################
# Figure A_12: Trade + financial openness
##############################################################
try({
  cat("\n=== FigureA_12: koftrade + tradegdp + global ===\n")
  s_kof  <- run_outcome_scm("koftrade")
  s_trd  <- run_outcome_scm("tradegdp")
  s_glob <- run_outcome_scm("global")
  cat(sprintf("koftrade: %d, tradegdp: %d, global: %d\n",
              length(s_kof), length(s_trd), length(s_glob)))

  p_kof <- make_gap_plot(s_kof, "Trade openness (KOF)",
    c(-20,15), seq(-20,15,5),
    paste0(c("-20","-15","-10","-5","0","+5","+10","+15"), " pt"))
  p_trd <- make_gap_plot(s_trd, "Trade/GDP",
    c(-20,15), seq(-20,15,5),
    paste0(c("-20","-15","-10","-5","0","+5","+10","+15"), " pt"))
  p_glob <- make_gap_plot(s_glob, "Financial openness",
    c(-20,15), seq(-20,15,5),
    paste0(c("-20","-15","-10","-5","0","+5","+10","+15"), " pt"))

  save_panels(list(p_kof, p_trd, p_glob), "FigureA_12.pdf", 18, 6)
}, silent = FALSE)

cat("ranscm_auth_outcomes_a11a12.R complete.\n")
