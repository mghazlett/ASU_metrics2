#Restore libraries from lock file

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

rm(list=ls(all=TRUE))
setwd('..')


##############################################################################################################

#FIGURE 6 and FIGURE B2

##############################################################################################################

try({
  
  cov.adj  <- NULL 
  features <- NULL  
  constant <- FALSE 
  rho      <- 'type-1'                                      
  rho.max  <- 1                                            
  u.order  <- 0                                            
  e.order  <- 0                                           
  u.lags   <- 0                                             
  e.lags   <- 0                                          
  u.sigma  <- "HC1"                                          
  e.sigma  <- "HC1"                                         
  u.missp  <- T                                           
  u.alpha  <- 0.1                                           
  e.alpha  <- 0.1                                         
  cointegrated.data <- TRUE                                
  cores    <- 1                                              
  sims     <- 200                                      
  e.method = "gaussian"                                     
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)   
  
  year <- c(1973,1989,2003,1990,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,2003,2000,2001,2003,1999,1952,1952,1952,1946,1951,1960, 1990)
  left <- c(1   ,0   ,1   ,0   ,0   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,0   ,1   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,1   ,1   ,0   ,1   ,1   ,0    ,0)
  oid <-  c(1   ,1   ,1   ,6   ,16  ,16  ,25  ,28  ,29  ,29  ,30  ,36  ,38  ,41  ,41  ,42  ,50  ,54  ,55  ,56  ,60  ,5   ,9   ,16  ,1   ,6   ,16   ,47)
  nid <-  c(1   ,1   ,1   ,6   ,15  ,16  ,22  ,27  ,29  ,29  ,29  ,32  ,34  ,38  ,40  ,42  ,49  ,54  ,53  ,53  ,59  ,5   ,8   ,11  ,1   ,6   ,11   ,45)
  
  s <- data.frame(nid,oid,year,left)
  s$case <- paste(as.character(s$nid), as.character(s$year), sep=".")
  s$final <- paste("_", as.character(s$case))
  s$fr1 = 0
  s$fr2 = 14
  s$fr3 = 15
  s$sta = 15
  
  s[which(s$year==1952),"fr2"] = 1
  s[which(s$year==1952),"fr3"] = 9
  s[which(s$year==1946),"fr2"] = 7
  s[which(s$year==1951),"fr2"] = 2
  s[which(s$year==1951),"fr3"] = 10
  s[which(s$year==1960),"fr1"] = 1
  s[which(s$year==1990 & s$oid==47),"fr1"] = 10
  
  for (k in 1:length(s$nid)) {
    options(warn=0)
    Oldc = s$oid[k]  
    Trea = s$nid[k]
    Year = s$year[k]
    Case = s$case[k]
    Left= s$left[k]
    Set = s$final[k]
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- s$sta & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul))) 
    donors <- donors %>%  dplyr::filter(msimul != 1)
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(fstgdp) | cid==Oldc ))
    data$lgfstgdp <- log(data$fstgdp)
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, lgfstgdp)
    names(tysub)[names(tysub) == 'lgfstgdp'] <- 'ilgfstgdp'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = lgfstgdp-ilgfstgdp, t = year-Year+15)
    data <- transform(data, index = as.numeric(factor(country)))
    data <-  data %>% dplyr::mutate(d = replace(d, war==1, NA))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <- scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", 
                  time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,  e.order = e.order, e.lags = e.lags,  
                    u.alpha = u.alpha, e.alpha = e.alpha, rho = rho,  rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(s$fr1[k]:s$fr2[k], s$fr3[k]:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Case, left = Left)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    scl.insample  <- result$inference.results$CI.in.sample[, 1, drop = FALSE]
    scr.insample  <- result$inference.results$CI.in.sample[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclinsample = c(scl.insample), scrinsample = c(scr.insample),  sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    series  <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste(Set), series) 
  } 
  
  dfs <- lapply(ls(pattern="_"), function(x) get(x))
  finaldata <- rbindlist(dfs)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclinsample[finaldata$ti == 0] <- 0
  finaldata$scrinsample[finaldata$ti == 0] <- 0
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata$all <- 1
  finaldata$right <- NA
  finaldata$right <- ifelse(finaldata$left == 1, 0, 1)
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrinsample_all=mean(scrinsample[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclinsample_all=mean(sclinsample[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  dataforplot_left <- Reduce(function(x, y) merge(x, y, left=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_left=mean(yfit[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, yact_left=mean(yact[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrinsample_left=mean(scrinsample[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclinsample_left=mean(sclinsample[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrgauss_left=mean(scrgauss[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclgauss_left=mean(sclgauss[left==1], na.rm=TRUE))))
  
  dataforplot_right <- Reduce(function(x, y) merge(x, y, right=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_right=mean(yfit[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, yact_right=mean(yact[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrinsample_right=mean(scrinsample[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclinsample_right=mean(sclinsample[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrgauss_right=mean(scrgauss[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclgauss_right=mean(sclgauss[right==1], na.rm=TRUE))))
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all_gauss <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclgauss_all, ymax=scrgauss_all, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_left_gauss <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclgauss_left, ymax=scrgauss_left, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right_gauss <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclgauss_right, ymax=scrgauss_right, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_gap_all_gauss <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.10), breaks = c(-0.30,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1), labels = format(c("-30%","-25%","-20%","-15%","-10%","-5%","0%","+5%","+10%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_gap_left_gauss <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=(sclgauss_left-yact_left)*(-1), ymax=(scrgauss_left-yact_left)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_left-yact_left)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_left-yact_left)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_left-yact_left)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.10), breaks = c(-0.30,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1), labels = format(c("-30%","-25%","-20%","-15%","-10%","-5%","0%","+5%","+10%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gdp_gap_right_gauss <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=(sclgauss_right-yact_right)*(-1), ymax=(scrgauss_right-yact_right)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_right-yact_right)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_right-yact_right)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_right-yact_right)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.10), breaks = c(-0.30,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1), labels = format(c("-30%","-25%","-20%","-15%","-10%","-5%","0%","+5%","+10%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))    
  
  gdp_trends_gauss = (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss) & 
    plot_annotation(title = 'Panel A: Trends', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  gdp_gap_gauss = (gdp_gap_all_gauss + gdp_gap_left_gauss + gdp_gap_right_gauss)  & 
    plot_annotation(title = 'Panel B: Doppelganger gap', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times")))
  
  wrap_elements(gdp_trends_gauss) / wrap_elements(gdp_gap_gauss) 
  
  outpath <- file.path("figures", "Figure6.pdf")
  ggsave(outpath, wrap_elements(gdp_trends_gauss) / wrap_elements(gdp_gap_gauss), width = 23, height = 16, units = "cm")
  
  gdp_trends_all_insample <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclinsample_all, ymax=scrinsample_all, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_all, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_all, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))     
  
  gdp_trends_left_insample <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclinsample_left, ymax=scrinsample_left, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_left, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_left, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_right_insample <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclinsample_right, ymax=scrinsample_right, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_right, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_right, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_trends_insample = (gdp_trends_all_insample + gdp_trends_left_insample + gdp_trends_right_insample) &
    plot_annotation(title = 'Panel A: Accounting for in-sample uncertainty', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  gdp_trends_gauss = (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss) & 
    plot_annotation(title = 'Panel B: Accounting for out-of-sample uncertainty', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  wrap_elements(gdp_trends_insample) / wrap_elements(gdp_trends_gauss) 
  
  outpath <- file.path("figures", "FigureB2.pdf")
  ggsave(outpath, wrap_elements(gdp_trends_insample) / wrap_elements(gdp_trends_gauss), width = 23, height = 16, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 7

##############################################################################################################

try({
  
  cov.adj  <- NULL 
  features <- NULL  
  constant <- FALSE 
  rho      <- 'type-1'                                      
  rho.max  <- 1                                            
  u.order  <- 0                                            
  e.order  <- 0                                           
  u.lags   <- 0                                             
  e.lags   <- 0                                          
  u.sigma  <- "HC1"                                          
  e.sigma  <- "HC1"                                         
  u.missp  <- T                                           
  u.alpha  <- 0.1                                           
  e.alpha  <- 0.1                                         
  cointegrated.data <- TRUE                                
  cores    <- 1                                              
  sims     <- 10 
  e.method = "gaussian"                                     
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)   
  
  year <- c(1973,1989,2003,1990,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,2003,2000,2001,2003,1999,1952,1952,1952,1946,1951,1960, 1990)
  left <- c(1   ,0   ,1   ,0   ,0   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,0   ,1   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,1   ,1   ,0   ,1   ,1   ,0    ,0)
  oid <-  c(1   ,1   ,1   ,6   ,16  ,16  ,25  ,28  ,29  ,29  ,30  ,36  ,38  ,41  ,41  ,42  ,50  ,54  ,55  ,56  ,60  ,5   ,9   ,16  ,1   ,6   ,16   ,47)
  nid <-  c(1   ,1   ,1   ,6   ,15  ,16  ,22  ,27  ,29  ,29  ,29  ,32  ,34  ,38  ,40  ,42  ,49  ,54  ,53  ,53  ,59  ,5   ,8   ,11  ,1   ,6   ,11   ,45)
  
  s <- data.frame(nid,oid,year,left)
  s$case <- paste(as.character(s$nid), as.character(s$year), sep=".")
  s$final <- paste("_", as.character(s$case))
  s$fr1 = 0
  s$fr2 = 14
  s$fr3 = 15
  s$sta = 15
  
  s[which(s$year==1952),"fr2"] = 1
  s[which(s$year==1952),"fr3"] = 9
  s[which(s$year==1946),"fr2"] = 7
  s[which(s$year==1951),"fr2"] = 2
  s[which(s$year==1951),"fr3"] = 10
  s[which(s$year==1960),"fr1"] = 1
  s[which(s$year==1990 & s$oid==47),"fr1"] = 10
  
  
  for (k in 1:length(s$nid)) {
    options(warn=0)
    Oldc = s$oid[k]  
    Trea = s$nid[k]
    Year = s$year[k]
    Case = s$case[k]
    Left= s$left[k]
    Set = s$final[k]
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- s$sta & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul != 1)
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(fstgdp) | cid==Oldc ))
    data$lgfstgdp <- log(data$fstgdp)
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, lgfstgdp)
    names(tysub)[names(tysub) == 'lgfstgdp'] <- 'ilgfstgdp'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = lgfstgdp-ilgfstgdp, t = year-Year+15)
    data <- transform(data, index = as.numeric(factor(country)))
    data <-  data %>% dplyr::mutate(d = replace(d, war==1, NA))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    for (p in unique(data$index)) {
      df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,   cointegrated.data = cointegrated.data, id.var = "index", 
                      time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = p, unit.co = unique(data$index)[-p])    
      result <- try({  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,  e.order = e.order, e.lags = e.lags,  
                            u.alpha = u.alpha, e.alpha = e.alpha, rho = rho,  rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
      }, silent = TRUE)
      if (!is.null(result)) {
        y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
        yfit    <- data.frame(t = c(s$fr1[k]:s$fr2[k], s$fr3[k]:30), yfit = c(y.fit))
        y.act <- rbind(result$data$Y.pre, result$data$Y.post)
        yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Case, nid=Trea, left = Left)
        ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
        scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
        scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
        cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
        series  <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
        series <- series %>% dplyr::mutate(id = p)
        assign(paste(Set, p),  series[series$id == p, ] ) }
    }
  } 
  
  dfs <- lapply(ls(pattern="_"), function(x) get(x))
  finaldata <- rbindlist(dfs)
  
  finaldata <- finaldata %>% dplyr::filter(finaldata$nid != finaldata$id)
  
  finaldata <- finaldata %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata$all <- 1
  finaldata$right <- NA
  finaldata$right <- ifelse(finaldata$left == 1, 0, 1)
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  dataforplot_left <- Reduce(function(x, y) merge(x, y, left=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_left=mean(yfit[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, yact_left=mean(yact[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrgauss_left=mean(scrgauss[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclgauss_left=mean(sclgauss[left==1], na.rm=TRUE))))
  
  dataforplot_right <- Reduce(function(x, y) merge(x, y, right=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_right=mean(yfit[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, yact_right=mean(yact[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrgauss_right=mean(scrgauss[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclgauss_right=mean(sclgauss[right==1], na.rm=TRUE))))
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all_gauss <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclgauss_all, ymax=scrgauss_all, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_left_gauss <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclgauss_left, ymax=scrgauss_left, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right_gauss <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclgauss_right, ymax=scrgauss_right, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss) 
  
  outpath <- file.path("figures", "Figure7.pdf")
  ggsave(outpath, (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 8

##############################################################################################################

try({
  
  cov.adj  <- NULL                                         
  features <- NULL                                         
  constant <- FALSE                                        
  rho      <- 'type-1'                                    
  rho.max  <- 1                                             
  u.order  <- 0                                             
  e.order  <- 0                                            
  u.lags   <- 0                                           
  e.lags   <- 0                                           
  u.sigma  <- "HC1"                                        
  e.sigma  <- "HC1"                                          
  u.missp  <- T                                              
  u.alpha  <- 0.1                                             
  e.alpha  <- 0.1                                  
  cointegrated.data <- TRUE                                 
  cores    <- 1                                              
  sims     <- 200                                         
  e.method = "gaussian"                                   
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)  
  
  listyear <- c(2003,2001,2003,2003,2001,2001,1996,1996,1990,1990,1952,1952,1952,
                1960,1994,1999,2000,1970,1998,1966,1985,1989,1975,1973,1968,1951)
  listtrea <- c(53,53,1,49,29,29,16,27,6,40,5,8,11,13,29,59,54,32,42,22,38,1,34,1,15,6)
  listoldi <- c(56,55,1,50,30,29,16,28,6,41,5,9,16,16,29,60,54,36,42,25,41,1,38,1,16,6)
  listcase <- c("Erdogan2003","Shinawatra2001","Kirchner2003","Roh2003","Koizumi2001",
                "Berlusconi2001","Bucaram1996","Netanyahu1996","Collor1990","Fujimori1990",
                "Estenssoro1952","Ibanez1952","Velasco1952","Velasco1960","Berlusconi1994",
                "Chavez1999","Chen2000","Echeverria1970","Estrada1998","Gandhi1966","Garcia1985",
                "Menem1989","Muldoon1975","Peron1973","Velasco1968","Vargas1951")
  warsl <- c(31,31,31,31,31,31,31,31,31,31,2,2,2,0,31,31,31,31,31,31,31,31,31,31,31,3)
  warel <- c(31,31,31,31,31,31,31,31,31,31,8,8,8,0,31,31,31,31,31,31,31,31,31,31,31,9)
  midf <-  c(14,14,14,14,14,14,14,14,14,14,1,1,1,14,14,14,14,14,14,14,14,14,14,14,14,2) 
  mids <-  c(15,15,15,15,15,15,15,15,15,15,9,9,9,15,15,15,15,15,15,15,15,15,15,15,15,10)
  star <-  c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
  s <- data.frame(listyear,listtrea,listoldi,listcase,warsl,warel,midf,mids,star)
  
  for (k in 1:length(s$listyear)) {
    options(warn=0)
    Trea = s$listtrea[k]
    Year = s$listyear[k]
    Oldi = s$listoldi[k]
    Cass = s$listcase[k]
    Wars = s$warsl[k]
    Ware = s$warel[k]
    Midf = s$midf[k]
    Mids = s$mids[k]
    Star = s$star[k]
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- 15 & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldi)
    donors <- data %>% dplyr::filter(data$cid != Oldi)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul != 1)
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(fstgdp) | cid==Oldi ))
    data$lgfstgdp <- log(data$fstgdp)
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, lgfstgdp)
    names(tysub)[names(tysub) == 'lgfstgdp'] <- 'ilgfstgdp'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = lgfstgdp-ilgfstgdp, t = year-Year+15)
    data$d[data$t >= Wars & data$t <= Ware] <- NA  
    data <- transform(data, index = as.numeric(factor(country)))
    period.pre  <- seq(from = 0, to = 10, by = 1) 
    period.post <- (11:30)
    df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj, cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", 
                    outcome.var = "d", period.pre = period.pre, period.post = period.post,  unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,   e.order = e.order, e.lags = e.lags,  u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, 
                    rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(Star:Midf, Mids:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Cass)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    final   <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste(Cass), final)
  }
  
  finaldata <- rbind(Berlusconi1994, Berlusconi2001, Bucaram1996,   Chavez1999, Chen2000, Collor1990,  Echeverria1970, Erdogan2003, Estenssoro1952, 
                     Estrada1998, Fujimori1990, Gandhi1966,   Garcia1985,  Ibanez1952, Kirchner2003, Koizumi2001,    Menem1989, Muldoon1975,   
                     Netanyahu1996, Peron1973,       Roh2003, Shinawatra2001, Vargas1951,   Velasco1952, Velasco1960, Velasco1968)
  
  rm(list=grep("^finaldata", ls(), value=T, invert=T))
  
  finaldata <- finaldata %>% dplyr::mutate(ti = t - 15)
  
  finaldata <- finaldata %>% dplyr::mutate(sclgauss = ifelse(ti == -5,yfit, sclgauss))
  finaldata <- finaldata %>% dplyr::mutate(scrgauss = ifelse(ti == -5,yfit, scrgauss))
  
  finaldata <- finaldata %>% dplyr::mutate(all = 1)
  finaldata <- finaldata %>% dplyr::mutate(left = 0)
  finaldata <- finaldata %>% dplyr::mutate(right = 0)
  
  finaldata$right[finaldata$case == 'Berlusconi1994'] <- 1 
  finaldata$right[finaldata$case == 'Berlusconi2001'] <- 1 
  finaldata$right[finaldata$case == 'Bucaram1996'] <- 1 
  finaldata$left[finaldata$case == 'Chavez1999'] <- 1 
  finaldata$right[finaldata$case == 'Chen2000'] <- 1 
  finaldata$right[finaldata$case == 'Collor1990'] <- 1 
  finaldata$left[finaldata$case == 'Echeverria1970'] <- 1
  finaldata$right[finaldata$case == 'Erdogan2003'] <- 1 
  finaldata$left[finaldata$case == 'Estenssoro1952'] <- 1 
  finaldata$left[finaldata$case == 'Estrada1998'] <- 1 
  finaldata$right[finaldata$case == 'Fujimori1990'] <- 1 
  finaldata$left[finaldata$case == 'Gandhi1966'] <- 1
  finaldata$left[finaldata$case == 'Garcia1985'] <- 1 
  finaldata$left[finaldata$case == 'Ibanez1952'] <- 1 
  finaldata$left[finaldata$case == 'Kirchner2003'] <- 1 
  finaldata$right[finaldata$case == 'Koizumi2001'] <- 1 
  finaldata$right[finaldata$case == 'Meciar1990'] <- 1 
  finaldata$right[finaldata$case == 'Menem1989'] <- 1 
  finaldata$right[finaldata$case == 'Muldoon1975'] <- 1 
  finaldata$right[finaldata$case == 'Netanyahu1996'] <- 1 
  finaldata$left[finaldata$case == 'Peron1946'] <- 1 
  finaldata$left[finaldata$case == 'Peron1973'] <- 1 
  finaldata$right[finaldata$case == 'Roh2003'] <- 1 
  finaldata$right[finaldata$case == 'Shinawatra2001'] <- 1 
  finaldata$left[finaldata$case == 'Vargas1951'] <- 1 
  finaldata$right[finaldata$case == 'Velasco1952'] <- 1 
  finaldata$right[finaldata$case == 'Velasco1960'] <- 1 
  finaldata$right[finaldata$case == 'Velasco1968'] <- 1
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  dataforplot_left <- Reduce(function(x, y) merge(x, y, left=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_left=mean(yfit[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, yact_left=mean(yact[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrgauss_left=mean(scrgauss[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclgauss_left=mean(sclgauss[left==1], na.rm=TRUE))))
  
  dataforplot_right <- Reduce(function(x, y) merge(x, y, right=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_right=mean(yfit[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, yact_right=mean(yact[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrgauss_right=mean(scrgauss[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclgauss_right=mean(sclgauss[right==1], na.rm=TRUE))))
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclgauss_all, ymax=scrgauss_all, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+geom_vline(xintercept = -5, linetype="solid",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_trends_left <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclgauss_left, ymax=scrgauss_left, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+geom_vline(xintercept = -5, linetype="solid",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_trends_right <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclgauss_right, ymax=scrgauss_right, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+geom_vline(xintercept = -5, linetype="solid",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  (gdp_trends_all + gdp_trends_left + gdp_trends_right) 
  
  outpath <- file.path("figures", "Figure8.pdf")
  ggsave(outpath, (gdp_trends_all + gdp_trends_left + gdp_trends_right), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 9

##############################################################################################################

try({
  
  wsoll1 <- function(X0,X1,V,pen=0.0){
    n = ncol(X0)
    Delta = diag(t(X0 - matrix(rep(1,n),ncol=n)%x%X1)%*%V%*%(X0 - matrix(rep(1,n),ncol=n)%x%X1))
    P = 2*t(X0)%*%V%*%X0
    q = t(-2*t(X0)%*%V%*%X1 + pen*Delta)
    sol = LowRankQP(Vmat=P,dvec=q,Amat=matrix(1, ncol=n),bvec=1,uvec=rep(1,n), method="LU")
    return(sol$alpha)
  }
  
  TZero <- function(x,tol=1e-6,scale=T){
    if(!all(x > 0)) stop("Some elements are negative!")
    y = ifelse(x < tol,0,x)
    if(scale) y = y/sum(y)
    return(y)
  }
  
  regsynth <- function(X0,X1,Y0,Y1,V,pen,tol=1e-6){
    func_list = c("wsoll1","TZero")
    sol = wsoll1(X0,X1,V,pen)
    sol = TZero(sol,tol)
    sol = round(sol,3)
    return(sol)
  }
  
  country <- c(1,1,1,1,5,6,6,9,16,16,16,16,25,28,29,29,30,36,38,41,41,42,47,50,54,55,56,60)
  year <- c(1946,1973,1989,2003,1952,1951,1990,1952,1952,1960,1968,1996,1966,1996, 1994,2001,2001,1970,1975,1985,1990,1998,1990,2003,2000,2001,2003,1999)
  left <- c(1,1,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1)
  s <- data.frame(country,year,left)
  s$case <- paste(as.character(s$country), as.character(s$year), sep=".")
  s$minus = -15
  s$plus = 15
  s[which(s$country==47 & s$year==1990),"minus"] = -5
  data <- read_dta("ple_dataset.dta") 
  data$lgfstgdp <- log(data$fstgdp) 
  data <- data[which(data$year <= 1913 | (data$year >=1919 & data$year <=1938) |data$year >=1946),]
  Y_synth = data.frame(Y0hat=numeric(),  time=numeric())
  lambda= c(0,.00001,.01,.1,.15,seq(.25,5,.1)) 
  curve_RMSE =  matrix(NA,nrow=length(s$country),ncol=length(lambda))
  curve_bias = matrix(NA,nrow=length(s$country),ncol=length(lambda))
  Y_synth = data.frame(Y0hat=numeric(), time=numeric())
  
  for(l in 1:length(lambda)){
    for (k in 1:length(s$country)) {
      i = s$country[k]
      date = s$year[k]
      other_pop = s[which(s$year==date & s$country != i),"country"]
      scm_data <- data[which(data$year>= date+s$minus[k] & data$year<=date+s$plus[k]),]
      scm_data <- scm_data[which(!(scm_data$cid %in% other_pop)),]
      na_index = which(is.na(scm_data[,"lgfstgdp"]))
      na_cid = scm_data$cid[na_index]
      na_cid = na_cid[!duplicated(na_cid)]
      scm_data <- scm_data[which(!(scm_data$cid %in% na_cid) | scm_data$cid==i),]
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp-lgfstgdp[which(year==date)])
      scm_data_y = scm_data[c("cid","year","Y")]
      scm_data_x = scm_data_y[which(scm_data_y$year < date),]
      scm_data_y = spread(scm_data_y, year, Y)
      scm_data_x = dcast(setDT(scm_data_x), cid  ~year, value.var = c("Y"), sep = "")
      scm_mat_x = as.matrix(scm_data_x)
      scm_mat_y = as.matrix(scm_data_y)
      X_0 = t(subset(scm_mat_x, scm_mat_x[,"cid"]!=i))[-1,]
      X_1 = t(subset(scm_mat_x, scm_mat_x[,"cid"]==i))[-1]
      Y_0 = subset(scm_mat_y, scm_mat_y[,"cid"]!=i)[,-1]
      Y_1 = subset(scm_mat_y, scm_mat_y[,"cid"]==i)[-1]
      V = diag(nrow(X_0))
      W = regsynth(X_0,X_1,Y_0,Y_1,V,pen=lambda[l])
      Y_0hat = t(t(W)%*%Y_0)
      destr <- as.data.frame(Y_0hat)
      destr <- rownames_to_column(destr, "tyears")
      destr = subset(destr, select = c(1))
      destr$tyears <- as.numeric(destr$tyears) 
      destrvec = as.vector(unlist(destr$tyears))
      time = destrvec - date
      Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
      Y$left = ifelse(s$left[k]==1,1,0)
      Y$case = s$case[k]
      Y_synth = rbind(Y_synth, Y)
      tau = Y_1 - Y_0hat
      tau = do.call(rbind, Map(data.frame, tau=tau, time= time))
      tau = tau$tau[which(tau$time<=0 & tau$time>= -15)]
      RMSE = sqrt(mean(tau^2))
      Bias = abs(mean(tau))
      curve_RMSE[k,l] = RMSE
      curve_bias[k,l] = Bias
    }
  }
  mean_RMSE = apply(curve_RMSE,2,mean)
  mean_bias = apply(curve_bias,2,mean)
  lambda_opt_RMSE = min(lambda[which(mean_RMSE==min(mean_RMSE))])
  lambda_opt_bias= min(lambda[which(mean_bias==min(mean_bias))])
  
  lambda_final = c(0, 0.1, lambda_opt_RMSE)
  for(l in 1:length(lambda_final)){
    Y_synth = data.frame(Y0hat=numeric(), time=numeric())
    for (k in 1:length(s$country)) { 
      i = s$country[k]
      date = s$year[k]
      other_pop = s[which(s$year==date & s$country != i),"country"]
      scm_data <- data[which(data$year>= date+s$minus[k] & data$year<=date+s$plus[k]),]
      scm_data <- scm_data[which(!(scm_data$cid %in% other_pop)),]
      na_index = which(is.na(scm_data[,"lgfstgdp"]))
      na_cid = scm_data$cid[na_index]
      na_cid = na_cid[!duplicated(na_cid)]
      scm_data <- scm_data[which(!(scm_data$cid %in% na_cid) | scm_data$cid==i),]
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp-lgfstgdp[which(year==date)])
      scm_data_y = scm_data[c("cid","year","Y")]
      scm_data_x = scm_data_y[which(scm_data_y$year < date),]
      scm_data_y = spread(scm_data_y, year, Y)
      scm_data_x = dcast(setDT(scm_data_x), cid  ~year, value.var = c("Y"), sep = "")
      scm_mat_x = as.matrix(scm_data_x)
      scm_mat_y = as.matrix(scm_data_y)
      X_0 = t(subset(scm_mat_x, scm_mat_x[,"cid"]!=i))[-1,]
      X_1 = t(subset(scm_mat_x, scm_mat_x[,"cid"]==i))[-1]
      Y_0 = subset(scm_mat_y, scm_mat_y[,"cid"]!=i)[,-1]
      Y_1 = subset(scm_mat_y, scm_mat_y[,"cid"]==i)[-1]
      V = diag(nrow(X_0))
      W = regsynth(X_0,X_1,Y_0,Y_1,V,pen=lambda_final[l])
      Y_0hat = t(t(W)%*%Y_0)
      destr <- as.data.frame(Y_0hat)
      destr <- rownames_to_column(destr, "tyears")
      destr = subset(destr, select = c(1))
      destr$tyears <- as.numeric(destr$tyears) 
      destrvec = as.vector(unlist(destr$tyears))
      time = destrvec - date
      Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
      Y$left = ifelse(s$left[k]==1,1,0)
      Y$case = s$case[k]
      Y_synth = rbind(Y_synth, Y)
    }
    assign(paste0("Y_Synth",l), Y_synth, envir = .GlobalEnv)
  } 
  
  no_penalty <- Y_Synth1
  fixed_penalty <- Y_Synth2
  optimal_penalty <- Y_Synth3
  
  optimal_penalty <- optimal_penalty %>% group_by(case) %>% dplyr::mutate(all = 1)
  optimal_penalty <- optimal_penalty %>% group_by(case) %>% dplyr::mutate(right = 0)
  optimal_penalty$right[optimal_penalty$left == 0] <- 1
  
  Y0hat_all <- ddply(optimal_penalty, .(time), summarise, Y0hat_all=mean(Y0hat[all==1], na.rm=TRUE))
  Y0hat_left <- ddply(optimal_penalty, .(time), summarise, Y0hat_left=mean(Y0hat[left==1], na.rm=TRUE)) 
  Y0hat_right <- ddply(optimal_penalty, .(time), summarise, Y0hat_right=mean(Y0hat[right==1], na.rm=TRUE))
  
  Y1_all <- ddply(optimal_penalty, .(time), summarise, Y1_all=mean(Y1[all==1], na.rm=TRUE))
  Y1_left <- ddply(optimal_penalty, .(time), summarise, Y1_left=mean(Y1[left==1], na.rm=TRUE)) 
  Y1_right <- ddply(optimal_penalty, .(time), summarise, Y1_right=mean(Y1[right==1], na.rm=TRUE))
  
  mergelist_all <- list(Y0hat_all, Y1_all)
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_all)
  
  mergelist_left <- list(Y0hat_left, Y1_left)
  dataforplot_left <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_left)
  
  mergelist_right <- list(Y0hat_right, Y1_right)
  dataforplot_right <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_right)
  
  # plot trends
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all <- ggplot(dataforplot_all) + 
    geom_line(aes(y=Y0hat_all, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_all, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gdp_trends_left <- ggplot(dataforplot_left) + 
    geom_line(aes(y=Y0hat_left, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_left, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right <- ggplot(dataforplot_right) + 
    geom_line(aes(y=Y0hat_right, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_right, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  (gdp_trends_all + gdp_trends_left + gdp_trends_right) 
  
  outpath <- file.path("figures", "Figure9.pdf")
  ggsave(outpath, (gdp_trends_all + gdp_trends_left + gdp_trends_right), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 10

##############################################################################################################

try({
  
  df <- read_dta("ple_dataset.dta")
  df$lgfstgdp <- log(df$fstgdp)
  data <- df[, c("country", "year", "lgfstgdp")]
  Italy_Berlusconi2001 <- data[(data$country == "Italy"),]
  Italy_Berlusconi2001 <- Italy_Berlusconi2001 %>% dplyr::mutate(country = "Italy_Berlusconi2001")
  Ecuador_Velasco1960 <- data[(data$country == "Ecuador"),]
  Ecuador_Velasco1960 <- Ecuador_Velasco1960 %>% dplyr::mutate(country = "Ecuador_Velasco1960")
  Ecuador_Velasco1968 <- data[(data$country == "Ecuador"),]
  Ecuador_Velasco1968 <- Ecuador_Velasco1968 %>% dplyr::mutate(country = "Ecuador_Velasco1968")
  Ecuador_Bucaram1996 <- data[(data$country == "Ecuador"),]
  Ecuador_Bucaram1996 <- Ecuador_Bucaram1996 %>% dplyr::mutate(country = "Ecuador_Bucaram1996")
  Brazil_Collor1990 <- data[(data$country == "Brazil"),]
  Brazil_Collor1990 <- Brazil_Collor1990 %>% dplyr::mutate(country = "Brazil_Collor1990")
  Peru_Fujimori1990 <- data[(data$country == "Peru"),]
  Peru_Fujimori1990 <- Peru_Fujimori1990 %>% dplyr::mutate(country = "Peru_Fujimori1990")
  Argentina_Peron1973 <- data[(data$country == "Argentina"),]
  Argentina_Peron1973 <- Argentina_Peron1973 %>% dplyr::mutate(country = "Argentina_Peron1973")
  Argentina_Menem1989 <- data[(data$country == "Argentina"),]
  Argentina_Menem1989 <- Argentina_Menem1989 %>% dplyr::mutate(country = "Argentina_Menem1989")
  Argentina_Kirchner2003 <- data[(data$country == "Argentina"),]
  Argentina_Kirchner2003 <- Argentina_Kirchner2003 %>% dplyr::mutate(country = "Argentina_Kirchner2003")
  
  pseudopanel <- rbind(data, Italy_Berlusconi2001, Ecuador_Velasco1960, Ecuador_Velasco1968, Ecuador_Bucaram1996, 
                       Brazil_Collor1990, Peru_Fujimori1990, Argentina_Peron1973, Argentina_Menem1989, Argentina_Kirchner2003)
  
  data <- pseudopanel %>%  dplyr::mutate(treatedyear= 0)
  data$treatedyear[data$country == "Italy" & data$year >=1994] <- 1
  data$treatedyear[data$country == "Italy_Berlusconi2001" & data$year >=2001] <- 1
  data$treatedyear[data$country == "Ecuador" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Ecuador_Velasco1960" & data$year >= 1960] <- 1
  data$treatedyear[data$country == "Ecuador_Velasco1968" & data$year >= 1968] <- 1
  data$treatedyear[data$country == "Ecuador_Bucaram1996" & data$year >= 1996] <- 1
  data$treatedyear[data$country == "Venezuela" & data$year >= 1999] <- 1
  data$treatedyear[data$country == "Taiwan" & data$year >= 2000] <- 1
  data$treatedyear[data$country == "Brazil" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Brazil_Collor1990" & data$year >= 1990] <- 1
  data$treatedyear[data$country == "Mexico" & data$year >= 1970] <- 1
  data$treatedyear[data$country == "Turkey" & data$year >= 2003] <- 1
  data$treatedyear[data$country == "Bolivia" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Philippines" & data$year >= 1998] <- 1
  data$treatedyear[data$country == "Peru" & data$year >= 1985] <- 1
  data$treatedyear[data$country == "Peru_Fujimori1990" & data$year >= 1990] <- 1
  data$treatedyear[data$country == "India" & data$year >= 1966] <- 1
  data$treatedyear[data$country == "Chile" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Argentina" & data$year >= 1946] <- 1
  data$treatedyear[data$country == "Argentina_Peron1973" & data$year >= 1973] <- 1
  data$treatedyear[data$country == "Argentina_Menem1989" & data$year >= 1989] <- 1
  data$treatedyear[data$country == "Argentina_Kirchner2003" & data$year >= 2003] <- 1
  data$treatedyear[data$country == "Japan" & data$year >= 2001] <- 1
  data$treatedyear[data$country == "Slovakia" & data$year >= 1990] <- 1
  data$treatedyear[data$country == "New Zealand" & data$year >= 1975] <- 1
  data$treatedyear[data$country == "Israel" & data$year >= 1996] <- 1
  data$treatedyear[data$country == "South Korea" & data$year >= 2003] <- 1
  data$treatedyear[data$country == "Thailand" & data$year >= 2001] <- 1
  
  ppool_syn3 <- multisynth(lgfstgdp ~ treatedyear, country, year, data, fixedeff = T, alpha = 0.1, n_lags=15, n_leads=16, nu = 0.5) 
  ppool <- summary(ppool_syn3)
  ppool
  ppool <- data.frame(ppool$att)
  
  ppool <- ppool[!(is.na(ppool$Time)),]
  ppool <- ppool[!(is.na(ppool$Estimate)),]
  ppool <- ppool %>% dplyr::filter(Time >= -15, Time <= 15) 
  ppool<-ppool[!(ppool$Level=="Average"),]
  
  ppool <- ppool %>%  dplyr::mutate(year= 0)
  ppool$year[ppool$Level == "Italy"] <- 1994
  ppool$year[ppool$Level == "Italy_Berlusconi2001"] <- 2001  
  ppool$year[ppool$Level == "Ecuador"] <- 1952 
  ppool$year[ppool$Level == "Ecuador_Velasco1960"] <- 1960 
  ppool$year[ppool$Level == "Ecuador_Velasco1968"] <- 1968  
  ppool$year[ppool$Level == "Ecuador_Bucaram1996"] <- 1996 
  ppool$year[ppool$Level == "Venezuela"] <-  1999 
  ppool$year[ppool$Level == "Taiwan"] <- 2000 
  ppool$year[ppool$Level == "Brazil"] <- 1952 
  ppool$year[ppool$Level == "Brazil_Collor1990"] <- 1990 
  ppool$year[ppool$Level == "Mexico"] <- 1970 
  ppool$year[ppool$Level == "Turkey"] <- 2003 
  ppool$year[ppool$Level == "Bolivia"] <- 1952 
  ppool$year[ppool$Level == "Philippines"] <- 1998 
  ppool$year[ppool$Level == "Peru"] <- 1985 
  ppool$year[ppool$Level == "Peru_Fujimori1990"] <- 1990 
  ppool$year[ppool$Level == "India"] <- 1966 
  ppool$year[ppool$Level == "Chile"] <- 1952 
  ppool$year[ppool$Level == "Argentina"] <- 1946 
  ppool$year[ppool$Level == "Argentina_Peron1973"] <- 1973 
  ppool$year[ppool$Level == "Argentina_Menem1989"] <- 1989 
  ppool$year[ppool$Level == "Argentina_Kirchner2003"] <- 2003 
  ppool$year[ppool$Level == "Japan"] <- 2001 
  ppool$year[ppool$Level == "Slovakia"] <- 1990 
  ppool$year[ppool$Level == "New Zealand"] <- 1975 
  ppool$year[ppool$Level == "Israel"] <- 1996 
  ppool$year[ppool$Level == "South Korea"] <- 2003 
  ppool$year[ppool$Level == "Thailand"] <- 2001 
  ppool <- ppool %>%  dplyr::mutate(year= year + Time)
  ppool <- ppool %>%  dplyr::mutate(country= Level)
  ppool <- merge(ppool, data,  by = c("country", "year")) 
  
  ppool_zero <- ppool %>% dplyr::filter(Time  == 0) 
  ppool_zero <- ppool_zero %>%  dplyr::mutate(ilgfstgdp= lgfstgdp)
  ppool_zero <- ppool_zero[, c("country", "ilgfstgdp")]
  ppool <- merge(ppool, ppool_zero,  by = c("country"))
  
  ppool['all']   <- 1
  
  ppool_ilgfstgdp_all <- ddply(ppool, .(Time), summarise, ilgfstgdp=mean(ilgfstgdp[all==1], na.rm=TRUE))
  ppool_lgfstgdp_all <- ddply(ppool, .(Time), summarise, lgfstgdp=mean(lgfstgdp[all==1], na.rm=TRUE))
  ppool_estatt_all <- ddply(ppool, .(Time), summarise, Estimate=mean(Estimate[all==1], na.rm=TRUE))
  ppool_lower_bound_all <- ddply(ppool, .(Time), summarise, lower_bound=mean(lower_bound[all==1], na.rm=TRUE))
  ppool_upper_bound_all <- ddply(ppool, .(Time), summarise, upper_bound=mean(upper_bound[all==1], na.rm=TRUE))
  mergelist_ppool_all <- list(ppool_estatt_all, ppool_lgfstgdp_all, ppool_ilgfstgdp_all, ppool_lower_bound_all, ppool_upper_bound_all)
  dataforplot_ppool_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_ppool_all)
  dataforplot_ppool_all <- dataforplot_ppool_all %>%  dplyr::mutate(yact_norm= lgfstgdp-ilgfstgdp)
  dataforplot_ppool_all <- dataforplot_ppool_all %>%  dplyr::mutate(yfit= yact_norm-Estimate)
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  bm_all <- ggplot(dataforplot_ppool_all) + 
    geom_line(aes(y=yfit, x=Time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_norm, x=Time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  data <- pseudopanel %>%  dplyr::mutate(treatedyear= 0)
  data$treatedyear[data$country == "Venezuela" & data$year >= 1999] <- 1
  data$treatedyear[data$country == "Brazil" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Mexico" & data$year >= 1970] <- 1
  data$treatedyear[data$country == "Bolivia" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Philippines" & data$year >= 1998] <- 1
  data$treatedyear[data$country == "Peru" & data$year >= 1985] <- 1
  data$treatedyear[data$country == "India" & data$year >= 1966] <- 1
  data$treatedyear[data$country == "Chile" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Argentina" & data$year >= 1946] <- 1
  data$treatedyear[data$country == "Argentina_Peron1973" & data$year >= 1973] <- 1
  data$treatedyear[data$country == "Argentina_Kirchner2003" & data$year >= 2003] <- 1
  
  ppool_syn <- multisynth(lgfstgdp ~ treatedyear, country, year,data, fixedeff = T, alpha = 0.1, n_lags=15, n_leads=16, nu = 0.5)
  ppool <- summary(ppool_syn)
  ppool
  ppool <- data.frame(ppool$att)
  
  ppool <- ppool[!(is.na(ppool$Time)),]
  ppool <- ppool[!(is.na(ppool$Estimate)),]
  ppool <- ppool %>% dplyr::filter(Time >= -15, Time <= 15) 
  ppool<-ppool[!(ppool$Level=="Average"),]
  
  ppool <- ppool %>%  dplyr::mutate(year= 0)
  ppool$year[ppool$Level == "Ecuador"] <- 1952 
  ppool$year[ppool$Level == "Ecuador_Velasco1960"] <- 1960 
  ppool$year[ppool$Level == "Ecuador_Velasco1968"] <- 1968  
  ppool$year[ppool$Level == "Venezuela"] <-  1999 
  ppool$year[ppool$Level == "Brazil"] <- 1952 
  ppool$year[ppool$Level == "Mexico"] <- 1970 
  ppool$year[ppool$Level == "Bolivia"] <- 1952 
  ppool$year[ppool$Level == "Philippines"] <- 1998 
  ppool$year[ppool$Level == "Peru"] <- 1985 
  ppool$year[ppool$Level == "India"] <- 1966 
  ppool$year[ppool$Level == "Chile"] <- 1952 
  ppool$year[ppool$Level == "Argentina"] <- 1946 
  ppool$year[ppool$Level == "Argentina_Peron1973"] <- 1973 
  ppool$year[ppool$Level == "Argentina_Kirchner2003"] <- 2003 
  
  ppool <- ppool %>%  dplyr::mutate(year= year + Time)
  ppool <- ppool %>%  dplyr::mutate(country= Level)
  ppool <- merge(ppool, data,  by = c("country", "year")) 
  
  ppool_zero <- ppool %>% dplyr::filter(Time  == 0) 
  ppool_zero <- ppool_zero %>%  dplyr::mutate(ilgfstgdp= lgfstgdp)
  ppool_zero <- ppool_zero[, c("country", "ilgfstgdp")]
  ppool <- merge(ppool, ppool_zero,  by = c("country"))
  
  ppool['all']   <- 1
  
  ppool_ilgfstgdp_all <- ddply(ppool, .(Time), summarise, ilgfstgdp=mean(ilgfstgdp[all==1], na.rm=TRUE))
  ppool_lgfstgdp_all <- ddply(ppool, .(Time), summarise, lgfstgdp=mean(lgfstgdp[all==1], na.rm=TRUE))
  ppool_estatt_all <- ddply(ppool, .(Time), summarise, Estimate=mean(Estimate[all==1], na.rm=TRUE))
  ppool_lower_bound_all <- ddply(ppool, .(Time), summarise, lower_bound=mean(lower_bound[all==1], na.rm=TRUE))
  ppool_upper_bound_all <- ddply(ppool, .(Time), summarise, upper_bound=mean(upper_bound[all==1], na.rm=TRUE))
  mergelist_ppool_all <- list(ppool_estatt_all, ppool_lgfstgdp_all, ppool_ilgfstgdp_all, ppool_lower_bound_all, ppool_upper_bound_all)
  dataforplot_ppool_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_ppool_all)
  dataforplot_ppool_all <- dataforplot_ppool_all %>%  dplyr::mutate(yact_norm= lgfstgdp-ilgfstgdp)
  dataforplot_ppool_all <- dataforplot_ppool_all %>%  dplyr::mutate(yfit= yact_norm-Estimate)
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  bm_left <- ggplot(dataforplot_ppool_all) + 
    geom_line(aes(y=yfit, x=Time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_norm, x=Time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  data <- pseudopanel %>%  dplyr::mutate(treatedyear= 0)
  data$treatedyear[data$country == "Italy" & data$year >=1994] <- 1
  data$treatedyear[data$country == "Italy_Berlusconi2001" & data$year >=2001] <- 1
  data$treatedyear[data$country == "Ecuador" & data$year >= 1952] <- 1
  data$treatedyear[data$country == "Ecuador_Velasco1960" & data$year >= 1960] <- 1
  data$treatedyear[data$country == "Ecuador_Velasco1968" & data$year >= 1968] <- 1
  data$treatedyear[data$country == "Ecuador_Bucaram1996" & data$year >= 1996] <- 1
  data$treatedyear[data$country == "Taiwan" & data$year >= 2000] <- 1
  data$treatedyear[data$country == "Brazil_Collor1990" & data$year >= 1990] <- 1
  data$treatedyear[data$country == "Turkey" & data$year >= 2003] <- 1
  data$treatedyear[data$country == "Peru_Fujimori1990" & data$year >= 1990] <- 1
  data$treatedyear[data$country == "Argentina_Menem1989" & data$year >= 1989] <- 1
  data$treatedyear[data$country == "Japan" & data$year >= 2001] <- 1
  data$treatedyear[data$country == "Slovakia" & data$year >= 1990] <- 1
  data$treatedyear[data$country == "New Zealand" & data$year >= 1975] <- 1
  data$treatedyear[data$country == "Israel" & data$year >= 1996] <- 1
  data$treatedyear[data$country == "South Korea" & data$year >= 2003] <- 1
  data$treatedyear[data$country == "Thailand" & data$year >= 2001] <- 1
  
  ppool_syn3 <- multisynth(lgfstgdp ~ treatedyear, country, year,  data, fixedeff = T, alpha = 0.1, n_lags=15, n_leads=16, nu = 0.5)
  ppool <- summary(ppool_syn3)
  ppool
  ppool <- data.frame(ppool$att)
  
  ppool <- ppool[!(is.na(ppool$Time)),]
  ppool <- ppool[!(is.na(ppool$Estimate)),]
  ppool <- ppool %>% dplyr::filter(Time >= -15, Time <= 15) 
  ppool<-ppool[!(ppool$Level=="Average"),]
  
  ppool <- ppool %>%  dplyr::mutate(year= 0)
  ppool$year[ppool$Level == "Italy"] <- 1994
  ppool$year[ppool$Level == "Italy_Berlusconi2001"] <- 2001  
  ppool$year[ppool$Level == "Ecuador"] <- 1952 
  ppool$year[ppool$Level == "Ecuador_Velasco1960"] <- 1960 
  ppool$year[ppool$Level == "Ecuador_Velasco1968"] <- 1968  
  ppool$year[ppool$Level == "Ecuador_Bucaram1996"] <- 1996 
  ppool$year[ppool$Level == "Taiwan"] <- 2000 
  ppool$year[ppool$Level == "Brazil_Collor1990"] <- 1990 
  ppool$year[ppool$Level == "Turkey"] <- 2003 
  ppool$year[ppool$Level == "Peru_Fujimori1990"] <- 1990 
  ppool$year[ppool$Level == "Argentina_Menem1989"] <- 1989 
  ppool$year[ppool$Level == "Japan"] <- 2001 
  ppool$year[ppool$Level == "Slovakia"] <- 1990 
  ppool$year[ppool$Level == "New Zealand"] <- 1975 
  ppool$year[ppool$Level == "Israel"] <- 1996 
  ppool$year[ppool$Level == "South Korea"] <- 2003 
  ppool$year[ppool$Level == "Thailand"] <- 2001 
  ppool <- ppool %>%  dplyr::mutate(year= year + Time)
  ppool <- ppool %>%  dplyr::mutate(country= Level)
  ppool <- merge(ppool, data,  by = c("country", "year")) 
  
  ppool_zero <- ppool %>% dplyr::filter(Time  == 0) 
  ppool_zero <- ppool_zero %>%  dplyr::mutate(ilgfstgdp= lgfstgdp)
  ppool_zero <- ppool_zero[, c("country", "ilgfstgdp")]
  ppool <- merge(ppool, ppool_zero,  by = c("country"))
  
  ppool['all']   <- 1
  
  ppool_ilgfstgdp_all <- ddply(ppool, .(Time), summarise, ilgfstgdp=mean(ilgfstgdp[all==1], na.rm=TRUE))
  ppool_lgfstgdp_all <- ddply(ppool, .(Time), summarise, lgfstgdp=mean(lgfstgdp[all==1], na.rm=TRUE))
  ppool_estatt_all <- ddply(ppool, .(Time), summarise, Estimate=mean(Estimate[all==1], na.rm=TRUE))
  ppool_lower_bound_all <- ddply(ppool, .(Time), summarise, lower_bound=mean(lower_bound[all==1], na.rm=TRUE))
  ppool_upper_bound_all <- ddply(ppool, .(Time), summarise, upper_bound=mean(upper_bound[all==1], na.rm=TRUE))
  mergelist_ppool_all <- list(ppool_estatt_all, ppool_lgfstgdp_all, ppool_ilgfstgdp_all, ppool_lower_bound_all, ppool_upper_bound_all)
  dataforplot_ppool_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_ppool_all)
  dataforplot_ppool_all <- dataforplot_ppool_all %>%  dplyr::mutate(yact_norm= lgfstgdp-ilgfstgdp)
  dataforplot_ppool_all <- dataforplot_ppool_all %>%  dplyr::mutate(yfit= yact_norm-Estimate)
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  bm_right <- ggplot(dataforplot_ppool_all) + 
    geom_line(aes(y=yfit, x=Time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_norm, x=Time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  (bm_all + bm_left + bm_right)  
  
  outpath <- file.path("figures", "Figure10.pdf")
  ggsave(outpath, (bm_all + bm_left + bm_right), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 11 and FIGURE C3

##############################################################################################################

try({
  
  cov.adj     <- NULL                                       # Covariates for adjustment
  features    <- NULL                                       # No features other than outcome
  constant    <- FALSE                                      # No constant term
  rho     <- 'type-1'                                       # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                                          # Maximum value attainable by rho
  u.order  <- 0                                             # Degree of polynomial in B and C when modelling u
  e.order  <- 0                                             # Degree of polynomial in B and C when modelling e
  u.lags   <- 0                                             # Lags of B to be used when modelling u
  e.lags   <- 0                                             # Lags of B to be used when modelling e
  u.sigma  <- "HC1"                                         # Estimator for the variance-covariance of u
  e.sigma  <- "HC1"                                         # Estimator for the variance-covariance of e
  u.missp  <- T                                             # If TRUE then the model is treated as misspecified
  u.alpha  <- 0.1                                          # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                                          # Confidence level (out-of-sample uncertainty)
  cointegrated.data <- TRUE                                 # Belief that the data are cointegrated
  cores    <- 1                                             # Number of cores to be used by scpi
  sims     <- 200                                           # Number of simulations
  e.method = "gaussian"                                     # Estimation method for out-of-sample uncertainty
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)     # Simplex-type constraint set
  
  # Gini index
  ple <- read_dta("ple_dataset.dta")
  set.seed(8894)
  ple$var <- ple$gini
  
  casel <- c("Peron1973","Menem1989","Berlusconi1994","Echeverria1970","Garcia1985","Estrada1998","Chen2000","Chavez1999",
             "Kirchner2003","Collor1990","Netanyahu1996","Berlusconi2001","Koizumi2001","Fujimori1990","Roh2003",
             "Shinawatra2001","Erdogan2003","Bucaram1996","Meciar1990")
  yearl <- c(1973,1989,1994,1970,1985,1998,2000,1999,2003,1990,1996,2001,2001,1990,2003,2001,2003,1996,1990)
  froml <- c(3,0,0,8,2,0,0,0,0,0,0,0,0,0,0,0,0,6,13)
  minnl <- c(12,15,15,7,13,15,15,15,15,15,15,15,15,15,15,15,15,9,2)
  start <- c(3,0,0 ,8 ,2 ,0 ,0 ,0,0,0,0,0,0,0,0,0,0,6,13)
  treal <- c(1,1,20,7 ,17,28,38,41,1,3,19,21,21,20,18,35,20,12,39)
  oldci <- c(1,1,29,36,41,42,54,60,1,6,28,29,30,41,50,55,26,16,47)
  s <- data.frame(casel,yearl,minnl,froml,start,oldci,treal)
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Minn = s$minnl[k]
    Froml = s$froml[k]
    Start = s$start[k]
    Oldci = s$oldci[k]
    
    period.post <- (16:30)
    period.pre <- seq(from = Froml, to = 15, by = 1) 
    intcps <- with(ple[ple$year == Year, ], data.frame(cid=cid, country=country, ivar=var))
    data <-ple[which(ple$year >=Year-Minn & ple$year <=Year+15),] 
    taker <- data %>% dplyr::filter(data$cid == Oldci)
    donors <- data %>% dplyr::filter(data$cid != Oldci)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0)  
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na((var)))) 
    data <- transform(merge(data, intcps) %>% group_by(cid) %>% dplyr::mutate(d = var-ivar, t = year-Year+15),index = as.numeric(factor(country)))
    df <- scdata(df = data, features = features, constant = constant, cov.adj = cov.adj, cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea]) 
    result <- scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp, e.order = e.order, e.lags = e.lags, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    ys <- merge(data.frame(t = c(period.pre, period.post), yact = c(rbind(result$data$Y.pre, result$data$Y.post)), case = Cass), data.frame(t = c(Start:30), yfit = c(y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit))), by.x="t", by.y="t", all = TRUE)
    final <- merge(ys, data.frame(t = c(period.post), cbind(result$inference.results$bounds$insample, result$inference.results$bounds$subgaussian)), by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  rm("final")
  finaldata <- do.call("rbind", mget(ls(pattern="final_")))
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  finaldata$X3[finaldata$ti == 0] <- 0
  finaldata$X4[finaldata$ti == 0] <- 0
  finaldata['all']  <- 1
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(X4[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(X3[all==1], na.rm=TRUE)))) 
  
  gini_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=((yfit_all+sclgauss_all)-yact_all)*(-1), ymax=((yfit_all+scrgauss_all)-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = ((yfit_all+sclgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = ((yfit_all+scrgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +   
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-3, 3), breaks = c(-3,-2,-1,0,1,2,3), labels = format(c("-3 pt","-2 pt","-1 pt","0 pt","+1 pt","+2 pt","+3 pt"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Gini index", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gini_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-4, 4), breaks = c(-4,-3,-2,-1,0,1,2,3,4), labels = format(c("-4 pt", "-3 pt","-2 pt","-1 pt","0 pp","+1 pt","+2 pt","+3 pt","+4 pt"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Gini index", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  # Labor share
  ple <- read_dta("ple_dataset.dta")
  set.seed(8894)
  ple$var <- ple$laborshare
  
  casel <- c("Peron1973","Menem1989","Velasco1968","Velasco1960","Gandhi1966","Berlusconi1994","Garcia1985","Muldoon1975",
             "Echeverria1970","Estrada1998","Chen2000","Chavez1999","Collor1990","Bucaram1996","Netanyahu1996",
             "Berlusconi2001","Koizumi2001","Meciar1990","Kirchner2003","Estenssoro1952","Velasco1952","Fujimori1990","Roh2003","Shinawatra2001","Erdogan2003")
  yearl <- c(1973,1989,1968,1960,1966,1994,1985,1975,1970,1998,2000,1999,1990,1996,1996,2001,2001,1990,2003,1952,1952,1990,2003,2001,2003)
  minnl <- c(15,15,15,10,15,15,7,15,15,15,15,15,15,15,15,15,15,15,15,2,2,12,15,15,15)
  start <- c(0,0,0,5,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,13,13,3,0,0,0)
  plsl <-  c(15,15,15,10,15,15,7 ,15,15,15,15,15,15,15,15,15,15,15,14,15,15,15,14,15,14)
  endl <-  c(30,30,30,25,30,30,22,30,30,30,30,30,30,30,30,30,30,30,29,30,30,30,29,30,29)
  treal <- c(1,1,13,11,19,29,41,31,28,42,54,60,6,16,27,29,29,45,1,5,10,40,49,53,54)
  oldci <- c(1,1,16,16,25,29,41,38,36,42,54,60,6,16,28,29,30,47,1,5,16,41,50,55,56)
  s <- data.frame(casel,yearl,minnl,start,oldci,treal,plsl,endl)
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea =  s$treal[k]
    Year =  s$yearl[k]
    Cass =  s$casel[k]
    Minn =  s$minnl[k]
    Start = s$start[k]
    Plus =  s$plsl[k]
    Endl = s$endl[k]
    Oldci = s$oldci[k]
    
    period.post <- (16:Endl)
    period.pre <- seq(from = Start, to = 15, by = 1) 
    intcps <- with(ple[ple$year == Year, ], data.frame(cid=cid, country=country, ivar=var))
    data <-ple[which(ple$year >=Year-Minn & ple$year <=Year+Plus),] 
    taker <- data %>% dplyr::filter(data$cid == Oldci)
    donors <- data %>% dplyr::filter(data$cid != Oldci)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0)  
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na((var)))) 
    data <- transform(merge(data, intcps) %>% group_by(cid) %>% dplyr::mutate(d = var-ivar, t = year-Year+15),index = as.numeric(factor(country)))
    df <- scdata(df = data, features = features, constant = constant, cov.adj = cov.adj, cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea]) 
    result <- scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp, e.order = e.order, e.lags = e.lags, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    ys <- merge(data.frame(t = c(period.pre, period.post), yact = c(rbind(result$data$Y.pre, result$data$Y.post)), case = Cass), data.frame(t = c(Start:Endl), yfit = c(y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit))), by.x="t", by.y="t", all = TRUE)
    final <- merge(ys, data.frame(t = c(period.post), cbind(result$inference.results$bounds$insample, result$inference.results$bounds$subgaussian)), by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  rm("final")
  finaldata <- do.call("rbind", mget(ls(pattern="final_")))
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  finaldata$X3[finaldata$ti == 0] <- 0
  finaldata$X4[finaldata$ti == 0] <- 0
  finaldata['all']  <- 1
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(X4[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(X3[all==1], na.rm=TRUE)))) 
  
  laborshare_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=((yfit_all+sclgauss_all)-yact_all)*(-1), ymax=((yfit_all+scrgauss_all)-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = ((yfit_all+sclgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = ((yfit_all+scrgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +   
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.03, 0.03), breaks = c(-0.03,-0.02,-0.01,0.00,0.01,0.02,0.03), labels = format(c("-3 pp","-2 pp","-1 pp","0 pp","+1 pp","+2 pp","+3 pp"),nsmall=1), expand=c(0.002,0.002))+  
    labs(title = "Labor share", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  laborshare_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.04, 0.04), breaks = c(-0.04,-0.03,-0.02,-0.01,0.00,0.01,0.02,0.03,0.04), labels = format(c("-4 pp", "-3 pp","-2 pp","-1 pp","0 pp","+1 pp","+2 pp","+3 pp","+4 pp"),nsmall=1), expand=c(0.002,0.002))+  
    labs(title = "Labor share", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  (gini_combinedbands + laborshare_combinedbands)   
  
  outpath <- file.path("figures", "Figure11.pdf")
  ggsave(outpath, (gini_combinedbands + laborshare_combinedbands), width = 15, height = 7, units = "cm")
  
  (gini_trends + laborshare_trends)
  
  outpath <- file.path("figures", "FigureC3.pdf")
  ggsave(outpath, (gini_trends + laborshare_trends), width = 15, height = 7, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 12 and FIGURE C4

##############################################################################################################

try({
  
  # Tariff rate
  
  cov.adj     <- NULL                                       # Covariates for adjustment
  features    <- NULL                                       # No features other than outcome
  constant    <- FALSE                                      # No constant term
  rho     <- 'type-1'                                       # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                                          # Maximum value attainable by rho
  u.order  <- 0                                             # Degree of polynomial in B and C when modelling u
  e.order  <- 0                                             # Degree of polynomial in B and C when modelling e
  u.lags   <- 0                                             # Lags of B to be used when modelling u
  e.lags   <- 0                                             # Lags of B to be used when modelling e
  u.sigma  <- "HC1"                                         # Estimator for the variance-covariance of u
  e.sigma  <- "HC1"                                         # Estimator for the variance-covariance of e
  u.missp  <- T                                             # If TRUE then the model is treated as misspecified
  u.alpha  <- 0.1                                          # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                                          # Confidence level (out-of-sample uncertainty)
  cointegrated.data <- TRUE                                 # Belief that the data are cointegrated
  cores    <- 1                                             # Number of cores to be used by scpi
  sims     <- 200                                         # Number of simulations
  e.method = "gaussian"                                     # Estimation method for out-of-sample uncertainty
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)     # Simplex-type constraint set
  
  ple <- read_dta("ple_dataset.dta")
  ple$var <- ple$tariffs
  
  casel <- c("Gandhi1966","Muldoon1975","Garcia1985","Estrada1998","Chen2000","Chavez1999","Collor1990","Netanyahu1996","Fujimori1990","Roh2003",
             "Kirchner2003","Bucaram1996","Koizumi2001","Erdogan2003","Shinawatra2001")
  yearl <- c(1966,1975,1985,1998,2000,1999,1990,1996,1990,2003,2003,1996,2001,2003,2001)
  froml <- c(9,0,0,0,0,8,0,0,0,0,5,12,2,5,0)
  minnl <- c(6,15,15,15,15,7,15,15,15,15,10,3,13,10,15)
  plusl <- c(15,15,15,15,14,15,15,15,15,15,15,15,15,15,14)
  start <- c(9,0,0,0,0,8,0,0,0,0,5,12,2,5,0)
  enddl <- c(30,30,30,30,29,30,30,30,30,30,30,30,30,30,29)
  treal <- c(10,13,17,29,34,58,6,18,21,46,1,16,27,48,33)
  oldci <- c(25,38,41,42,54,60,6,28,41,50,1,16,30,56,55)
  s <- data.frame(casel,yearl,minnl,plusl,froml,start,enddl,treal,oldci)
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Minn = s$minnl[k]
    Plus = s$plusl[k]
    Froml = s$froml[k]
    Start = s$start[k]
    Enddl = s$enddl[k]
    Oldci = s$oldci[k]
    
    period.post <- (16:Enddl)
    period.pre <- seq(from = Froml, to = 15, by = 1) 
    intcps <- with(ple[ple$year == Year, ], data.frame(cid=cid, country=country, ivar=var))
    data <-ple[which(ple$year >=Year-Minn & ple$year <=Year+Plus),] 
    taker <- data %>% dplyr::filter(data$cid == Oldci)
    donors <- data %>% dplyr::filter(data$cid != Oldci)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0)  
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na((var)))) 
    data <- transform(merge(data, intcps) %>% group_by(cid) %>% dplyr::mutate(d = var-ivar, t = year-Year+15),index = as.numeric(factor(country)))
    df <- scdata(df = data, features = features, constant = constant, cov.adj = cov.adj, cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea]) 
    result <- scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp, e.order = e.order, e.lags = e.lags, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    ys <- merge(data.frame(t = c(period.pre, period.post), yact = c(rbind(result$data$Y.pre, result$data$Y.post)), case = Cass), data.frame(t = c(Start:Enddl), yfit = c(y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit))), by.x="t", by.y="t", all = TRUE)
    final <- merge(ys, data.frame(t = c(period.post), cbind(result$inference.results$bounds$insample, result$inference.results$bounds$subgaussian)), by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  # Peron1946 // No data in takeover # Peron1973 // No data in takeover # Menem1989 // No data in takeover # Estenssoro1952 // No data in takeover # Vargas1951 // No data in takeover # Ibanez1952 // No data in takeover
  # Velasco1952 // No data in takeover # Velasco1960 // No data in takeover # Velasco1968 // No data in takeover # Berlusconi1994 // EUCU # Berlusconi2001 // EUCU # Echeverria1970 // No data in takeover # Meciar1990 // EUCU
  
  rm("final")
  finaldata <- do.call("rbind", mget(ls(pattern="final_")))
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  finaldata$X3[finaldata$ti == 0] <- 0
  finaldata$X4[finaldata$ti == 0] <- 0
  finaldata['all']  <- 1
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(X4[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(X3[all==1], na.rm=TRUE)))) 
  
  tariffs_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=((yfit_all+sclgauss_all)-yact_all)*(-1), ymax=((yfit_all+scrgauss_all)-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = ((yfit_all+sclgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = ((yfit_all+scrgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +   
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-10, 15), breaks = c(-10,-5,0,5,10,15), labels = format(c("-10 pp","-5 pp","0 pp","+5 pp","+10 pp","+15 pp"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Tariff rate", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  tariffs_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-15, 10), breaks = c(-15,-10,-5,0,5,10), labels = format(c("-15 pp","-10 pp","-5 pp","0 pp","+5 pp","+10 pp"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Tariff rate", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  x<- which(ls()=="tariffs_combinedbands"|ls()=="tariffs_trends")
  ls1<- ls()[-x]
  rm(list = ls1)
  
  # Trade/GDP
  
  cov.adj     <- NULL                                       # Covariates for adjustment
  features    <- NULL                                       # No features other than outcome
  constant    <- FALSE                                      # No constant term
  rho     <- 'type-1'                                       # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                                          # Maximum value attainable by rho
  u.order  <- 0                                             # Degree of polynomial in B and C when modelling u
  e.order  <- 0                                             # Degree of polynomial in B and C when modelling e
  u.lags   <- 0                                             # Lags of B to be used when modelling u
  e.lags   <- 0                                             # Lags of B to be used when modelling e
  u.sigma  <- "HC1"                                         # Estimator for the variance-covariance of u
  e.sigma  <- "HC1"                                         # Estimator for the variance-covariance of e
  u.missp  <- T                                             # If TRUE then the model is treated as misspecified
  u.alpha  <- 0.1                                          # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                                          # Confidence level (out-of-sample uncertainty)
  cointegrated.data <- TRUE                                 # Belief that the data are cointegrated
  cores    <- 1                                             # Number of cores to be used by scpi
  sims     <- 200                                      # Number of simulations
  e.method = "gaussian"                                     # Estimation method for out-of-sample uncertainty
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)     # Simplex-type constraint set
  
  ple <- read_dta("ple_dataset.dta")
  ple$var <- ple$tradegdp
  
  casel <- c("Berlusconi1994","Chavez1999","Chen2000","Echeverria1970","Estrada1998","Gandhi1966","Garcia1985","Menem1989","Muldoon1975","Velasco1968",
             "Berlusconi2001","Bucaram1996","Collor1990","Erdogan2003","Fujimori1990","Kirchner2003","Koizumi2001","Netanyahu1996","Shinawatra2001",
             "Estenssoro1952","Ibanez1952","Velasco1952","Peron1946","Vargas1951","Velasco1960")
  yearl <- c(1994,1999,2000,1970,1998,1966,1985,1989,1975,1968,2001,1996,1990,2003,1990,2003,2001,1996,2001,1952,1952,1952,1946,1951,1960)
  froml <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  minnl <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15)
  plusl <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15)
  start <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  enddl <- c(30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30)
  treal <- c(29,59,54,32,42,22,38,1,34,15,29,16,6,53,40,1,29,27,53,5,8,11,1,6,13)
  oldci <- c(29,60,54,36,42,25,41,1,38,16,29,16,6,56,41,1,30,28,55,5,9,16,1,6,16)
  warsl <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,2,2,2,8,3,0)
  warel <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,8,8,8,14,9,0)
  midfl <- c(14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,1,1,1,7,2,14)
  midsl <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,9,9,9,15,10,15)
  
  
  s <- data.frame(casel,yearl,minnl,plusl,froml,start,enddl,treal,oldci,warsl,warel,midfl,midsl)
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Minn = s$minnl[k]
    Plus = s$plusl[k]
    Froml = s$froml[k]
    Start = s$start[k]
    Enddl = s$enddl[k]
    Oldci = s$oldci[k]
    Wars = s$warsl[k]
    Ware = s$warel[k]
    Midf = s$midfl[k]
    Mids = s$midsl[k]
    
    period.post <- (16:Enddl)
    period.pre <- seq(from = Froml, to = 15, by = 1) 
    intcps <- with(ple[ple$year == Year, ], data.frame(cid=cid, country=country, ivar=var))
    data <-ple[which(ple$year >=Year-Minn & ple$year <=Year+Plus),] 
    taker <- data %>% dplyr::filter(data$cid == Oldci)
    donors <- data %>% dplyr::filter(data$cid != Oldci)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0)  
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na((var)))) 
    data <- transform(merge(data, intcps) %>% group_by(cid) %>% dplyr::mutate(d = var-ivar, t = year-Year+15),index = as.numeric(factor(country)))
    data$d[data$t >= Wars & data$t <= Ware] <- NA
    df <- scdata(df = data, features = features, constant = constant, cov.adj = cov.adj, cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea]) 
    result <- scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp, e.order = e.order, e.lags = e.lags, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    ys <- merge(data.frame(t = c(period.pre, period.post), yact = c(rbind(result$data$Y.pre, result$data$Y.post)), case = Cass), data.frame(t = c(Start:Midf, Mids:Enddl), yfit = c(y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit))), by.x="t", by.y="t", all = TRUE)
    final <- merge(ys, data.frame(t = c(period.post), cbind(result$inference.results$bounds$insample, result$inference.results$bounds$subgaussian)), by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  # Meciar1990 // Data unreliable # Peron1973 // Data unreliable # Roh2003 // Data unreliable
  
  rm("final")
  finaldata <- do.call("rbind", mget(ls(pattern="final_")))
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  finaldata$X3[finaldata$ti == 0] <- 0
  finaldata$X4[finaldata$ti == 0] <- 0
  finaldata['all']  <- 1
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(X4[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(X3[all==1], na.rm=TRUE)))) 
  
  tradegdp_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=((yfit_all+sclgauss_all)-yact_all)*(-1), ymax=((yfit_all+scrgauss_all)-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = ((yfit_all+sclgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = ((yfit_all+scrgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +   
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue",  "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.15, 0.05), breaks = c(-0.15,-0.10,-0.05,0,0.05), labels = format(c("-15 pp","-10 pp","-5 pp","0 pp","+5 pp"),nsmall=1), expand=c(0.002,0.002))+  
    labs(title = "Trade/GDP", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  tradegdp_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.10, 0.15), breaks = c(-0.10,-0.05,0,0.05,0.10,0.15), labels = format(c("-10 pp","-5 pp","0 pp","+5 pp","+10 pp","+15 pp"),nsmall=1), expand=c(0.002,0.002))+  
    labs(title = "Trade/GDP", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  x<- which(ls()=="tariffs_combinedbands"|ls()=="tariffs_trends"|ls()=="tradegdp_combinedbands"|ls()=="tradegdp_trends")
  ls1<- ls()[-x]
  rm(list = ls1)
  
  # Financial openness
  
  cov.adj     <- NULL                                       # Covariates for adjustment
  features    <- NULL                                       # No features other than outcome
  constant    <- FALSE                                      # No constant term
  rho     <- 'type-1'                                       # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                                          # Maximum value attainable by rho
  u.order  <- 0                                             # Degree of polynomial in B and C when modelling u
  e.order  <- 0                                             # Degree of polynomial in B and C when modelling e
  u.lags   <- 0                                             # Lags of B to be used when modelling u
  e.lags   <- 0                                             # Lags of B to be used when modelling e
  u.sigma  <- "HC1"                                         # Estimator for the variance-covariance of u
  e.sigma  <- "HC1"                                         # Estimator for the variance-covariance of e
  u.missp  <- T                                             # If TRUE then the model is treated as misspecified
  u.alpha  <- 0.1                                          # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                                          # Confidence level (out-of-sample uncertainty)
  cointegrated.data <- TRUE                                 # Belief that the data are cointegrated
  cores    <- 1                                             # Number of cores to be used by scpi
  sims     <- 200                                         # Number of simulations
  e.method = "gaussian"                                     # Estimation method for out-of-sample uncertainty
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)     # Simplex-type constraint set
  
  ple <- read_dta("ple_dataset.dta")
  ple$var <- ple$global
  
  casel <- c("Berlusconi1994","Chavez1999","Estrada1998","Garcia1985","Menem1989","Muldoon1975","Peron1973","Berlusconi2001",
             "Bucaram1996","Collor1990","Koizumi2001","Netanyahu1996","Shinawatra2001","Erdogan2003","Fujimori1990","Kirchner2003","Roh2003")
  yearl <- c(1994,1999,1998,1985,1989,1975,1973,2001,1996,1990,2001,1996,2001,2003,1990,2003,2003)
  froml <- c(0,0,0,0,0,10,12,0,0,0,0,0,0,0,0,0,0)
  minnl <- c(15,15,15,15,15,5,3,15,15,15,15,15,15,15,15,15,15)
  plusl <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,14,14,14,14)
  start <- c(0,0,0,0,0,10,12,0,0,0,0,0,0,0,0,0,0)
  enddl <- c(30,30,30,30,30,30,30,30,30,30,30,30,30,29,29,29,29)
  treal <- c(26,51,37,36,1,33,1,26,14,6,26,24,44,45,35,1,41)
  oldci <- c(29,60,42,41,1,38,1,29,16,6,30,28,55,56,41,1,50)
  s <- data.frame(casel,yearl,minnl,plusl,froml,start,enddl,treal,oldci)
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Minn = s$minnl[k]
    Plus = s$plusl[k]
    Froml = s$froml[k]
    Start = s$start[k]
    Enddl = s$enddl[k]
    Oldci = s$oldci[k]
    
    period.post <- (16:Enddl)
    period.pre <- seq(from = Froml, to = 15, by = 1) 
    intcps <- with(ple[ple$year == Year, ], data.frame(cid=cid, country=country, ivar=var))
    data <-ple[which(ple$year >=Year-Minn & ple$year <=Year+Plus),] 
    taker <- data %>% dplyr::filter(data$cid == Oldci)
    donors <- data %>% dplyr::filter(data$cid != Oldci)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0)  
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na((var)))) 
    data <- transform(merge(data, intcps) %>% group_by(cid) %>% dplyr::mutate(d = var-ivar, t = year-Year+15),index = as.numeric(factor(country)))
    df <- scdata(df = data, features = features, constant = constant, cov.adj = cov.adj, cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea]) 
    result <- scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp, e.order = e.order, e.lags = e.lags, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    ys <- merge(data.frame(t = c(period.pre, period.post), yact = c(rbind(result$data$Y.pre, result$data$Y.post)), case = Cass), data.frame(t = c(Start:Enddl), yfit = c(y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit))), by.x="t", by.y="t", all = TRUE)
    final <- merge(ys, data.frame(t = c(period.post), cbind(result$inference.results$bounds$insample, result$inference.results$bounds$subgaussian)), by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  # Chen2000 // No data in takeover # Echeverria1970 // No data in takeover # Estenssoro1952 // No data in takeover # Gandhi1966 // No data in takeover
  # Ibanez1952 // No data in takeover # Meciar1990 // No data in takeover # Peron1946 // No data in takeover # Vargas1951 // No data in takeover
  # Velasco1952 // No data in takeover # Velasco1960 // No data in takeover # Velasco1968 // No data in takeover
  
  rm("final")
  finaldata <- do.call("rbind", mget(ls(pattern="final_")))
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  finaldata$X3[finaldata$ti == 0] <- 0
  finaldata$X4[finaldata$ti == 0] <- 0
  finaldata['all']  <- 1
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(X4[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(X3[all==1], na.rm=TRUE)))) 
  
  global_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=((yfit_all+sclgauss_all)-yact_all)*(-1), ymax=((yfit_all+scrgauss_all)-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = ((yfit_all+sclgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = ((yfit_all+scrgauss_all)-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +   
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-15, 5), breaks = c(-15, -10, -5, 0, 5), labels = format(c("-15 pt","-10 pt", "-5 pt", "0 pt", "+5 pt"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Financial openness", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  global_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-20,15), breaks = c(-20,-15,-10,-5,0,5,10,15), labels = format(c("-20 pt","-15 pt","-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt", "+15 pt"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Financial openness", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  (tariffs_combinedbands + tradegdp_combinedbands + global_combinedbands)  
  
  outpath <- file.path("figures", "Figure12.pdf")
  ggsave(outpath, (tariffs_combinedbands + tradegdp_combinedbands + global_combinedbands), width = 18, height = 6, units = "cm")
  
  (tariffs_trends + tradegdp_trends + global_trends)   
  
  outpath <- file.path("figures", "FigureC4.pdf")
  ggsave(outpath, (tariffs_trends + tradegdp_trends + global_trends), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 13 AND FIGURE C5

##############################################################################################################

try({
  
  cov.adj     <- NULL                                       # Covariates for adjustment
  features    <- NULL                                       # No features other than outcome
  constant    <- FALSE                                      # No constant term
  rho     <- 'type-1'                                       # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                                          # Maximum value attainable by rho
  u.order  <- 0                                             # Degree of polynomial in B and C when modelling u
  e.order  <- 0                                             # Degree of polynomial in B and C when modelling e
  u.lags   <- 0                                             # Lags of B to be used when modelling u
  e.lags   <- 0                                             # Lags of B to be used when modelling e
  u.sigma  <- "HC1"                                         # Estimator for the variance-covariance of u
  e.sigma  <- "HC1"                                         # Estimator for the variance-covariance of e
  u.missp  <- T                                             # If TRUE then the model is treated as misspecified
  u.alpha  <- 0.1                                          # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                                          # Confidence level (out-of-sample uncertainty)
  cointegrated.data <- TRUE                                 # Belief that the data are cointegrated
  cores    <- 1                                             # Number of cores to be used by scpi
  sims     <- 200                                            # Number of simulations
  e.method = "gaussian"                                     # Estimation method for out-of-sample uncertainty
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)     # Simplex-type constraint set
  
  # Debt/GDP
  
  casel <- c("Berlusconi1994","Chavez1999","Chen2000","Estrada1998","Gandhi1966","Menem1989","Muldoon1975",
             "Peron1973","Velasco1968","Berlusconi2001","Bucaram1996","Collor1990","Erdogan2003",
             "Fujimori1990","Kirchner2003","Koizumi2001","Netanyahu1996","Roh2003","Shinawatra2001",
             "Ibanez1952","Velasco1952","Peron1946","Vargas1951","Velasco1960","Echeverria1970","Garcia1985")
  yearl <- c(1994,1999,2000,1998,1966,1989,1975,1973,1968,2001,1996,1990,2003,1990,2003,2001,1996,2003,2001,1952,1952,1946,1951,1960,1970,1985)
  froml <- c(0,0,2,0,0,0,0,0,0,0,0,0,0,6,0,0,2,0,0,0,0,0,0,0,0,0)
  minnl <- c(15,15,13,15,15,15,15,15,15,15,15,15,15,9, 15,15,13,15,15,15,15,15,15,15,15,15)
  plusl <- c(15,15,15,11,15,15,15,15,15,15,15,15,14,15,14,15,15,14,15,15,15,15,15,15,15,15)
  start <- c(0,0,2,0,0,0,0,0,0,0,0,0,0,6,0,0,2,0,0,0,0,0,0,1,0,2)
  enddl <- c(30,30,30,26,30,30,30,30,30,30,30,30,29,30,29,30,30,29,30,30,30,30,30,30,30,30)
  treal <- c(21,42,39,34,16,1,21,1,10,22,12,6,37,31,1,22,20,32,37,5,7,1,3,8,20,30)
  oldil <- c(29,60,54,42,25,1,38,1,16,29,16,6,56,41,1,30,28,50,55,9,16,1,6,16,36,41)
  warsl <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,2,2,8,3,0,13,3)
  warel <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,8,8,14,9,0,14,9)
  midfl <- c(14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,1,1,7,2,14,12,2)
  midsl <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,9,9,15,10,15,15,10)
  
  s <- data.frame(casel,yearl,minnl,plusl,froml,start,enddl,treal,oldil,warsl,warel,midfl,midsl)
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Minn = s$minnl[k]
    Plus = s$plusl[k]
    Froml = s$froml[k]
    Start = s$start[k]
    Enddl = s$enddl[k]
    Oldc = s$oldil[k]
    Wars = s$warsl[k]
    Ware = s$warel[k]
    Midf = s$midfl[k]
    Mids = s$midsl[k]
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year-Minn & data$year <=Year+Plus),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    taker <- taker  %>% dplyr::mutate(type = 0)  
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0) 
    donors <- donors  %>% dplyr::mutate(type = 1) 
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data[which(data$year >=Year-Minn & data$year <=Year+Plus),]
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(debtgdp)) | type==0)
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, debtgdp)
    names(tysub)[names(tysub) == 'debtgdp'] <- 'idebtgdp'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = debtgdp-idebtgdp, t = year-Year+15)
    data$d[data$t >= Wars & data$t <= Ware] <- NA
    data <- transform(data, index = as.numeric(factor(country)))
    period.pre  <- seq(from = Froml, to = 15, by = 1) 
    period.post <- (16:Enddl)
    df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", 
                    outcome.var = "d", period.pre = period.pre, period.post = period.post,   unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,   e.order = e.order, e.lags = e.lags,  u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, 
                    rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(Start:Midf, Mids:Enddl), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Cass)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    final  <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  # Estenssoro1952 // No data in takeover # Meciar1990 // No data in takeover
  
  finaldata <- rbind(final_Berlusconi1994, final_Berlusconi2001, final_Bucaram1996,   final_Chavez1999, final_Chen2000, final_Collor1990, 
                     final_Echeverria1970, final_Erdogan2003,   final_Estrada1998, final_Fujimori1990, final_Gandhi1966,  final_Garcia1985, 
                     final_Ibanez1952, final_Kirchner2003, final_Koizumi2001,   final_Menem1989, final_Muldoon1975, 
                     final_Netanyahu1996, final_Peron1946, final_Peron1973,  final_Roh2003, final_Shinawatra2001, final_Vargas1951,  
                     final_Velasco1952, final_Velasco1960, final_Velasco1968)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata['all']   <- 1
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  debtgdp_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.20, 0.30), breaks = c(-0.2,-0.1,0,0.1,0.2,0.3), labels = format(c("-20 pp","-10 pp", "0 pp", "+10 pp", "+20 pp", "+30 pp"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Debt/GDP", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  debtgdp_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.30), breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3), labels = format(c("-30 pp","-20 pp","-10 pp", "0 pp", "+10 pp", "+20 pp", "+30 pp"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Debt/GDP", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  # Inflation
  
  casel <- c("Berlusconi1994","Chavez1999","Chen2000","Echeverria1970","Estrada1998","Gandhi1966","Muldoon1975",
             "Velasco1968","Berlusconi2001","Bucaram1996","Koizumi2001","Meciar1990","Roh2003","Shinawatra2001",
             "Estenssoro1952","Ibanez1952","Peron1946","Vargas1951","Velasco1960")
  yearl <- c(1994,1999,2000,1970,1998,1966,1975,1968,2001,1996,2001,1990,2003,2001,1952,1952,1946,1951,1960)
  treal <- c(29,60,54,32,42,22,34,15,29,16,29,47,50,55,5,7,1,6,14)
  olddl <- c(29,60,54,36,42,25,38,16,29,16,30,47,50,55,5,9,1,6,16)
  warsl <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,2,2,8,3,0)
  warel <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,8,8,14,9,0)
  start <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0,0,0,0,1)
  midfl <- c(14,14,14,14,14,14,14,14,14,14,14,14,14,14,1,1,7,2,14)
  midsl <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,9,9,15,10,15)
  
  s <- data.frame(casel,yearl,treal,olddl,warsl,warel,start,midfl,midsl)
  
  for (k in 1:length(s$olddl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Oldc = s$olddl[k]
    Wars = s$warsl[k]
    Ware = s$warel[k]
    Start = s$start[k]
    Midf = s$midfl[k]
    Mids = s$midsl[k]
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year-15 & data$year <=Year+15),]
    data <- data  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year & cid !=Oldc, 1,0))
    data <- data %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    data <- data %>%  dplyr::filter(msimul == 0) 
    data <- data[which(data$year >=Year-15 & data$year <=Year+15),]
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(inflation)  | cid==Oldc  ))
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = inflation, t = year-Year+15)
    data$d[data$t >= Wars & data$t <= Ware] <- NA
    data <- transform(data, index = as.numeric(factor(country)))
    df = subset(data, select = c(country,iso,cid,index))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", 
                    outcome.var = "d", period.pre = period.pre, period.post = period.post,   unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,   e.order = e.order, e.lags = e.lags,  u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, 
                    rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(Start:Midf, Mids:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Cass)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    final   <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  # Collor1990 // Hyperinflation before takeover # Erdogan2003 // Hyperinflation before takeover # Velasco1952 // Hyperinflation before takeover
  # Peron1973 // Hyperinflation before takeover # Netanyahu1996 // Hyperinflation before takeover # Menem1989 // Hyperinflation before takeover
  # Kirchner2003 // Hyperinflation before takeover # Fujimori1990 // Hyperinflation before takeover # Garcia1985 // Hyperinflation before takeover
  
  finaldata <- rbind(final_Berlusconi1994, final_Berlusconi2001, final_Bucaram1996,   final_Chavez1999, final_Chen2000,  
                     final_Echeverria1970,  final_Estenssoro1952,   final_Estrada1998,  final_Gandhi1966,
                     final_Ibanez1952, final_Koizumi2001,       final_Meciar1990,   final_Muldoon1975,  final_Peron1946,  
                     final_Roh2003, final_Shinawatra2001, final_Vargas1951,   final_Velasco1960, final_Velasco1968)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata <- finaldata %>% dplyr::mutate(sclgauss = ifelse(ti == 0,yfit, sclgauss))
  finaldata <- finaldata %>% dplyr::mutate(scrgauss = ifelse(ti == 0,yfit, scrgauss))
  
  finaldata['all']   <- 1
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  inflation_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.2,0.30), breaks = c(-0.20,-0.10,0,0.10,0.20,0.30), labels = format(c("-20","-10","0","10","20","30"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Inflation", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  inflation_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.1,0.40), breaks = c(-0.10,0,0.10,0.20,0.30,0.40), labels = format(c("-10","0","10","20","30","40"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Inflation", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  (debtgdp_combinedbands + inflation_combinedbands)     
  
  outpath <- file.path("figures", "Figure13.pdf")
  ggsave(outpath, (debtgdp_combinedbands + inflation_combinedbands), width = 15, height = 7, units = "cm")
  
  (debtgdp_trends + inflation_trends)  
  
  outpath <- file.path("figures", "FigureC5.pdf")
  ggsave(outpath, (debtgdp_trends + inflation_trends) , width = 15, height = 7, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE 14 AND FIGURE C6

##############################################################################################################

try({
  
  cov.adj     <- NULL                                       # Covariates for adjustment
  features    <- NULL                                       # No features other than outcome
  constant    <- FALSE                                      # No constant term
  rho     <- 'type-1'                                       # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                                          # Maximum value attainable by rho
  u.order  <- 0                                             # Degree of polynomial in B and C when modelling u
  e.order  <- 0                                             # Degree of polynomial in B and C when modelling e
  u.lags   <- 0                                             # Lags of B to be used when modelling u
  e.lags   <- 0                                             # Lags of B to be used when modelling e
  u.sigma  <- "HC1"                                         # Estimator for the variance-covariance of u
  e.sigma  <- "HC1"                                         # Estimator for the variance-covariance of e
  u.missp  <- T                                             # If TRUE then the model is treated as misspecified
  u.alpha  <- 0.1                                          # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                                          # Confidence level (out-of-sample uncertainty)
  cointegrated.data <- TRUE                                 # Belief that the data are cointegrated
  cores    <- 1                                             # Number of cores to be used by scpi
  sims     <- 200                                           # Number of simulations
  e.method = "gaussian"                                     # Estimation method for out-of-sample uncertainty
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)     # Simplex-type constraint set
  
  casel <- c("Berlusconi1994","Chavez1999","Chen2000","Echeverria1970","Estrada1998","Gandhi1966","Garcia1985",
             "Menem1989","Muldoon1975","Peron1973","Velasco1968","Berlusconi2001","Bucaram1996","Collor1990",
             "Erdogan2003","Fujimori1990","Kirchner2003","Koizumi2001","Roh2003","Shinawatra2001","Netanyahu1996",
             "Peron1946","Vargas1951","Velasco1960","Estenssoro1952","Ibanez1952","Velasco1952")
  yearl <- c(1994,1999,2000,1970,1998,1966,1985,1989,1975,1973,1968,2001,1996,1990,2003,1990,2003,2001,2003,2001,1996,1946,1951,1960,1952,1952,1952)
  treal <- c(27,54,48,32,38,23,37,1,34,1,15,27,15,6,48,36,1,27,49,47,25,1,6,15,5,8,13)
  oldil <- c(29,60,54,36,42,25,41,1,38,1,16,29,16,6,56,41,1,30,50,55,28,1,6,16,5,9,16)
  warsl <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,8,3,0,2,2,2)
  warel <- c(31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,14,9,0,8,8,8)
  dfasl <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
  dffml <- c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,7,2,15,1,1,1)
  dfsml <- c(16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,15,10,16,9,9,9)
  
  s <- data.frame(casel,yearl,treal,oldil,warsl,warel,dfasl,dffml,dfsml)
  
  # Judicial
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Oldc = s$oldil[k]
    Wars = s$warsl[k]
    Ware = s$warel[k] 
    Dfas = s$dfasl[k] 
    Dffm = s$dffml[k] 
    Dfsm = s$dfsml[k] 
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- 15 & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0) 
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(judicial) | cid==Oldc ))
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, judicial)
    names(tysub)[names(tysub) == 'judicial'] <- 'ijudicial'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = judicial-ijudicial, t = year-Year+15)
    data$d[data$t >= Wars & data$t <= Ware] <- NA  
    data <- transform(data, index = as.numeric(factor(country)))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", 
                    outcome.var = "d", period.pre = period.pre, period.post = period.post,   unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,   e.order = e.order, e.lags = e.lags,  u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, 
                    rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(Dfas:Dffm, Dfsm:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Cass)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    final   <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  finaldata <- rbind( final_Berlusconi1994, final_Bucaram1996, final_Echeverria1970, final_Berlusconi2001,   final_Chavez1999, final_Chen2000, final_Collor1990, 
                      final_Erdogan2003, final_Estenssoro1952,  final_Estrada1998, final_Fujimori1990, final_Gandhi1966,   final_Garcia1985,  final_Ibanez1952, final_Kirchner2003, final_Koizumi2001, 
                      final_Menem1989, final_Muldoon1975,   final_Netanyahu1996, final_Peron1946, final_Peron1973,   final_Roh2003, final_Shinawatra2001, final_Vargas1951, 
                      final_Velasco1952, final_Velasco1960, final_Velasco1968)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata['all']   <- 1
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  judicial_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.25,0.15), breaks = c(-0.25,-0.2,-0.15, -0.10, -0.05, 0, 0.05, 0.1, 0.15), labels = format(c("-25 pt","-20 pt","-15 pt", "-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt", "+15 pt"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Judicial constraints", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  judicial_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30,0.10), breaks = c(-0.30,-0.25,-0.2,-0.15, -0.10, -0.05, 0, 0.05, 0.1), labels = format(c("-30 pt","-25 pt","-20 pt","-15 pt", "-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Judicial constraints", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  # Electoral
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Oldc = s$oldil[k]
    Wars = s$warsl[k]
    Ware = s$warel[k] 
    Dfas = s$dfasl[k] 
    Dffm = s$dffml[k] 
    Dfsm = s$dfsml[k] 
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- 15 & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0) 
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(electoral) | cid==Oldc ))
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, electoral)
    names(tysub)[names(tysub) == 'electoral'] <- 'ielectoral'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = electoral-ielectoral, t = year-Year+15)
    data$d[data$t >= Wars & data$t <= Ware] <- NA  
    data <- transform(data, index = as.numeric(factor(country)))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", 
                    outcome.var = "d", period.pre = period.pre, period.post = period.post,   unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,   e.order = e.order, e.lags = e.lags,  u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, 
                    rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(Dfas:Dffm, Dfsm:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Cass)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    final   <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  finaldata <- rbind( final_Berlusconi1994, final_Bucaram1996, final_Echeverria1970, final_Berlusconi2001, 
                      final_Chavez1999, final_Chen2000, final_Collor1990, 
                      final_Erdogan2003, final_Estenssoro1952, 
                      final_Estrada1998, final_Fujimori1990, final_Gandhi1966,
                      final_Garcia1985,  final_Ibanez1952, final_Kirchner2003, final_Koizumi2001, 
                      final_Menem1989, final_Muldoon1975, 
                      final_Netanyahu1996, final_Peron1946, final_Peron1973, 
                      final_Roh2003, final_Shinawatra2001, final_Vargas1951, 
                      final_Velasco1952, final_Velasco1960, final_Velasco1968)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata['all']   <- 1
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  electoral_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.25,0.15), breaks = c(-0.25,-0.2,-0.15, -0.10, -0.05, 0, 0.05, 0.1, 0.15), labels = format(c("-25 pt","-20 pt","-15 pt", "-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt", "+15 pt"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Free elections", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  electoral_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30,0.10), breaks = c(-0.30,-0.25,-0.2,-0.15, -0.10, -0.05, 0, 0.05, 0.1), labels = format(c("-30 pt","-25 pt","-20 pt","-15 pt", "-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Free elections", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  # Medial
  
  s<-s[!(s$casel=="Echeverria1970"),] # Model will fail 
  
  for (k in 1:length(s$yearl)) {
    options(warn=0)
    Trea = s$treal[k]
    Year = s$yearl[k]
    Cass = s$casel[k]
    Oldc = s$oldil[k]
    Wars = s$warsl[k]
    Ware = s$warel[k] 
    Dfas = s$dfasl[k] 
    Dffm = s$dffml[k] 
    Dfsm = s$dfsml[k] 
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- 15 & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul == 0) 
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(medial) | cid==Oldc ))
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, medial)
    names(tysub)[names(tysub) == 'medial'] <- 'imedial'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = medial-imedial, t = year-Year+15)
    data$d[data$t >= Wars & data$t <= Ware] <- NA  
    data <- transform(data, index = as.numeric(factor(country)))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <-   scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", time.var = "t", 
                    outcome.var = "d", period.pre = period.pre, period.post = period.post,   unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,   e.order = e.order, e.lags = e.lags,  u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, 
                    rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(Dfas:Dffm, Dfsm:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Cass)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    final   <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste('final_', Cass, sep = ""), final)
  }
  
  finaldata <- rbind( final_Berlusconi1994, final_Bucaram1996, final_Berlusconi2001, 
                      final_Chavez1999, final_Chen2000, final_Collor1990, 
                      final_Erdogan2003, final_Estenssoro1952, 
                      final_Estrada1998, final_Fujimori1990, final_Gandhi1966,
                      final_Garcia1985,  final_Ibanez1952, final_Kirchner2003, final_Koizumi2001, 
                      final_Menem1989, final_Muldoon1975, 
                      final_Netanyahu1996, final_Peron1946, final_Peron1973, 
                      final_Roh2003, final_Shinawatra2001, final_Vargas1951, 
                      final_Velasco1952, final_Velasco1960, final_Velasco1968)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata['all']   <- 1
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  medial_combinedbands <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.25,0.15), breaks = c(-0.25,-0.2,-0.15, -0.10, -0.05, 0, 0.05, 0.1, 0.15), labels = format(c("-25 pt","-20 pt","-15 pt", "-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt", "+15 pt"),nsmall=1), expand=c(0.02,0.02))+  
    labs(title = "Media freedom", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  medial_trends <- ggplot(dataforplot_all) + 
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30,0.10), breaks = c(-0.30,-0.25,-0.2,-0.15, -0.10, -0.05, 0, 0.05, 0.1), labels = format(c("-30 pt","-25 pt","-20 pt","-15 pt", "-10 pt", "-5 pt", "0 pt", "+5 pt", "+10 pt"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Media freedom", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  (judicial_combinedbands + electoral_combinedbands + medial_combinedbands)
  
  outpath <- file.path("figures", "Figure14.pdf")
  ggsave(outpath, (judicial_combinedbands + electoral_combinedbands + medial_combinedbands), width = 18, height = 6, units = "cm")
  
  (judicial_trends + electoral_trends + medial_trends) 
  
  outpath <- file.path("figures", "FigureC6.pdf")
  ggsave(outpath, (judicial_trends + electoral_trends + medial_trends), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

# Appendix-only figures (B6-B9) skipped -- not required for main paper replication
quit(save = "no")

##############################################################################################################

#FIGURE B6

##############################################################################################################

try({
  
  wsoll1 <- function(X0,X1,V,pen=0.0){
    n = ncol(X0)
    Delta = diag(t(X0 - matrix(rep(1,n),ncol=n)%x%X1)%*%V%*%(X0 - matrix(rep(1,n),ncol=n)%x%X1))
    P = 2*t(X0)%*%V%*%X0
    q = t(-2*t(X0)%*%V%*%X1 + pen*Delta)
    sol = LowRankQP(Vmat=P,dvec=q,Amat=matrix(1, ncol=n),bvec=1,uvec=rep(1,n), method="LU")
    return(sol$alpha)
  }
  
  TZero <- function(x,tol=1e-6,scale=T){
    if(!all(x > 0)) stop("Some elements are negative!")
    y = ifelse(x < tol,0,x)
    if(scale) y = y/sum(y)
    return(y)
  }
  
  regsynth <- function(X0,X1,Y0,Y1,V,pen,tol=1e-6){
    func_list = c("wsoll1","TZero")
    sol = wsoll1(X0,X1,V,pen)
    sol = TZero(sol,tol)
    sol = round(sol,3)
    return(sol)
  }
  
  country <- c(1,1,1,1,5,6,6,9,16,16,16,16,25,28,29,29,30,36,38,41,41,42,47,50,54,55,56,60)
  year <- c(1946,1973,1989,2003,1952,1951,1990,1952,1952,1960,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,1990,2003,2000,2001,2003,1999)
  left <- c(1,1,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1)
  populists_core_total <- data.frame(country,year,left)
  populists_core_total$case <- paste(as.character(populists_core_total$country), as.character(populists_core_total$year), sep=".")
  populists_core_total$minus = -15
  populists_core_total$plus = 15
  cov_year = 5
  populists_core_total[which(populists_core_total$country==47 & populists_core_total$year==1990),"minus"] = -5
  data <- read_dta("ple_dataset.dta") 
  data = data %>% group_by(cid) %>%  dplyr::mutate(ipinstitutions = na.approx(institutions, na.rm=FALSE))
  data$institutions <- ifelse(is.na(data$institutions), data$ipinstitutions, data$institutions)
  mea <-  data %>%  group_by(year) %>%  summarise(ymi = mean(institutions, na.rm = T))
  data <- merge(data,mea)
  data$institutions <- ifelse(is.na(data$institutions), data$ymi, data$institutions)
  data$lgfstgdp <- log(data$fstgdp)
  data <- data[which(data$year <= 1913 | (data$year >=1919 & data$year <=1938)|data$year >=1946),]
  Y_synth = data.frame(Y0hat=numeric(), time=numeric())
  s = populists_core_total
  lambda= c(0,.00001,.01,.1,.15,seq(.25,5,.1)) 
  curve_RMSE =  matrix(NA,nrow=length(s$country),ncol=length(lambda))
  curve_bias = matrix(NA,nrow=length(s$country),ncol=length(lambda))
  Y_synth = data.frame(Y0hat=numeric(), time=numeric())
  
  for(l in 1:length(lambda)){
    for (k in 1:length(s$country)) { 
      i = s$country[k]
      date = s$year[k]
      other_pop = s[which(s$year==date & s$country != i),"country"]
      scm_data <- data[which(data$year>= date+s$minus[k] & data$year<=date+s$plus[k]),]
      scm_data <- scm_data[which(!(scm_data$cid %in% other_pop)),]
      na_index = which(is.na(scm_data[,"lgfstgdp"]))
      na_cid = scm_data$cid[na_index]
      na_cid = na_cid[!duplicated(na_cid)]
      scm_data <- scm_data[which(!(scm_data$cid %in% na_cid) | scm_data$cid==i),]
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp-lgfstgdp[which(year==date)])
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X3= mean(institutions[which(year < date)]))
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X4= bankcrisis[which(year == date)-1])
      scm_data_y = scm_data[c("cid","year","Y")]
      scm_data_x = scm_data_y[which(scm_data_y$year < date),]
      scm_data_y = spread(scm_data_y, year, Y)
      scm_data_x = dcast(setDT(scm_data_x), cid  ~year, value.var = c("Y"), sep = "")
      scm_data_cov = scm_data[c("cid","X3","X4")]
      scm_data_cov= scm_data_cov[!duplicated(scm_data_cov),]
      scm_data_x = merge(scm_data_x, scm_data_cov, by="cid")
      scm_mat_x = as.matrix(scm_data_x)
      scm_mat_y = as.matrix(scm_data_y)
      X_0 = t(subset(scm_mat_x, scm_mat_x[,"cid"]!=i))[-1,]
      X_1 = t(subset(scm_mat_x, scm_mat_x[,"cid"]==i))[-1]
      Y_0 = subset(scm_mat_y, scm_mat_y[,"cid"]!=i)[,-1]
      Y_1 = subset(scm_mat_y, scm_mat_y[,"cid"]==i)[-1]
      V = diag(nrow(X_0))
      W = regsynth(X_0,X_1,Y_0,Y_1,V,pen=lambda[l])
      Y_0hat = t(t(W)%*%Y_0)
      destr <- as.data.frame(Y_0hat)
      destr <- rownames_to_column(destr, "tyears")
      destr = subset(destr, select = c(1))
      destr$tyears <- as.numeric(destr$tyears) 
      destrvec = as.vector(unlist(destr$tyears))
      time = destrvec - date
      Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
      Y$left = ifelse(s$left[k]==1,1,0)
      Y$case = s$case[k]
      Y_synth = rbind(Y_synth, Y)
      tau = Y_1 - Y_0hat
      tau = do.call(rbind, Map(data.frame, tau=tau, time= time))
      tau = tau$tau[which(tau$time<=0 & tau$time>= -15)]
      RMSE = sqrt(mean(tau^2))
      Bias = abs(mean(tau))
      curve_RMSE[k,l] = RMSE
      curve_bias[k,l] = Bias
    }
  }
  
  mean_RMSE = apply(curve_RMSE,2,mean)
  mean_bias = apply(curve_bias,2,mean)
  lambda_opt_RMSE = min(lambda[which(mean_RMSE==min(mean_RMSE))])
  lambda_opt_bias= min(lambda[which(mean_bias==min(mean_bias))])
  
  lambda_final = c(0, 0.1, lambda_opt_RMSE)
  for(l in 1:length(lambda_final)){
    Y_synth = data.frame(Y0hat=numeric(), time=numeric())
    for (k in 1:length(s$country)) { 
      i = s$country[k]
      date = s$year[k]
      other_pop = s[which(s$year==date & s$country != i),"country"]
      scm_data <- data[which(data$year>= date+s$minus[k] & data$year<=date+s$plus[k]),]
      scm_data <- scm_data[which(!(scm_data$cid %in% other_pop)),]
      na_index = which(is.na(scm_data[,"lgfstgdp"]))
      na_cid = scm_data$cid[na_index]
      na_cid = na_cid[!duplicated(na_cid)]
      scm_data <- scm_data[which(!(scm_data$cid %in% na_cid) | scm_data$cid==i),]
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp-lgfstgdp[which(year==date)])
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X3= mean(institutions[which(year < date)]))
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X4= bankcrisis[which(year == date)-1])
      scm_data_y = scm_data[c("cid","year","Y")]
      scm_data_x = scm_data_y[which(scm_data_y$year < date),]
      scm_data_y = spread(scm_data_y, year, Y)
      scm_data_x = dcast(setDT(scm_data_x), cid  ~year, value.var = c("Y"), sep = "")
      scm_data_cov = scm_data[c("cid","X3","X4")]
      scm_data_cov= scm_data_cov[!duplicated(scm_data_cov),]
      scm_data_x = merge(scm_data_x, scm_data_cov, by="cid")
      scm_mat_x = as.matrix(scm_data_x)
      scm_mat_y = as.matrix(scm_data_y)
      X_0 = t(subset(scm_mat_x, scm_mat_x[,"cid"]!=i))[-1,]
      X_1 = t(subset(scm_mat_x, scm_mat_x[,"cid"]==i))[-1]
      Y_0 = subset(scm_mat_y, scm_mat_y[,"cid"]!=i)[,-1]
      Y_1 = subset(scm_mat_y, scm_mat_y[,"cid"]==i)[-1]
      V = diag(nrow(X_0))
      W = regsynth(X_0,X_1,Y_0,Y_1,V,pen=lambda_final[l])
      Y_0hat = t(t(W)%*%Y_0)
      destr <- as.data.frame(Y_0hat)
      destr <- rownames_to_column(destr, "tyears")
      destr = subset(destr, select = c(1))
      destr$tyears <- as.numeric(destr$tyears) 
      destrvec = as.vector(unlist(destr$tyears))
      time = destrvec - date
      Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
      Y$left = ifelse(s$left[k]==1,1,0)
      Y$case = s$case[k]
      Y_synth = rbind(Y_synth, Y)
    }
    assign(paste0("Y_Synth",l), Y_synth, envir = .GlobalEnv)
  }
  
  no_penalty <- Y_Synth1
  fixed_penalty <- Y_Synth2
  optimal_penalty <- Y_Synth3
  
  no_penalty <- no_penalty %>% group_by(case) %>% dplyr::mutate(all = 1)
  no_penalty <- no_penalty %>% group_by(case) %>% dplyr::mutate(right = 0)
  no_penalty$right[no_penalty$left == 0] <- 1
  
  Y0hat_all <- ddply(no_penalty, .(time), summarise, Y0hat_all=mean(Y0hat[all==1], na.rm=TRUE))
  Y0hat_left <- ddply(no_penalty, .(time), summarise, Y0hat_left=mean(Y0hat[left==1], na.rm=TRUE)) 
  Y0hat_right <- ddply(no_penalty, .(time), summarise, Y0hat_right=mean(Y0hat[right==1], na.rm=TRUE))
  
  Y1_all <- ddply(no_penalty, .(time), summarise, Y1_all=mean(Y1[all==1], na.rm=TRUE))
  Y1_left <- ddply(no_penalty, .(time), summarise, Y1_left=mean(Y1[left==1], na.rm=TRUE)) 
  Y1_right <- ddply(no_penalty, .(time), summarise, Y1_right=mean(Y1[right==1], na.rm=TRUE))
  
  mergelist_all <- list(Y0hat_all, Y1_all)
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_all)
  
  mergelist_left <- list(Y0hat_left, Y1_left)
  dataforplot_left <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_left)
  
  mergelist_right <- list(Y0hat_right, Y1_right)
  dataforplot_right <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_right)
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all <- ggplot(dataforplot_all) + 
    geom_line(aes(y=Y0hat_all, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_all, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.40), breaks = c(-0.4,-0.2,0,0.2,0.4), labels = format(c("-40%","-20%","0%","+20%","+40%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gdp_trends_left <- ggplot(dataforplot_left) + 
    geom_line(aes(y=Y0hat_left, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_left, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.40), breaks = c(-0.4,-0.2,0,0.2,0.4), labels = format(c("-40%","-20%","0%","+20%","+40%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right <- ggplot(dataforplot_right) + 
    geom_line(aes(y=Y0hat_right, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_right, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.40), breaks = c(-0.4,-0.2,0,0.2,0.4), labels = format(c("-40%","-20%","0%","+20%","+40%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  (gdp_trends_all + gdp_trends_left + gdp_trends_right) 
  
  outpath <- file.path("figures", "FigureB6.pdf")
  ggsave(outpath, (gdp_trends_all + gdp_trends_left + gdp_trends_right), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE B7

##############################################################################################################

try({
  
  cov.adj  <- NULL 
  features <- c("d", "bankcrisis", "institutions")
  constant <- FALSE 
  rho      <- 'type-1'                                      
  rho.max  <- 1                                            
  u.order  <- 0                                            
  e.order  <- 0                                           
  u.lags   <- 0                                             
  e.lags   <- 0                                          
  u.sigma  <- "HC1"                                          
  e.sigma  <- "HC1"                                         
  u.missp  <- T                                           
  u.alpha  <- 0.1                                           
  e.alpha  <- 0.1                                         
  cointegrated.data <- TRUE                                
  cores    <- 1                                              
  sims     <- 200                                      
  e.method = "gaussian"                                     
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)   
  
  year <- c(1973,1989,2003,1990,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,2003,2000,2001,2003,1999,1952,1952,1952,1946,1951,1960, 1990)
  left <- c(1   ,0   ,1   ,0   ,0   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,0   ,1   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,1   ,1   ,0   ,1   ,1   ,0    ,0)
  oid <-  c(1   ,1   ,1   ,6   ,16  ,16  ,25  ,28  ,29  ,29  ,30  ,36  ,38  ,41  ,41  ,42  ,50  ,54  ,55  ,56  ,60  ,5   ,9   ,16  ,1   ,6   ,16   ,47)
  nid <-  c(1   ,1   ,1   ,6   ,15  ,16  ,22  ,27  ,29  ,29  ,29  ,32  ,34  ,38  ,40  ,42  ,49  ,54  ,53  ,53  ,59  ,5   ,8   ,11  ,1   ,6   ,11   ,45)
  
  s <- data.frame(nid,oid,year,left)
  s$case <- paste(as.character(s$nid), as.character(s$year), sep=".")
  s$final <- paste("_", as.character(s$case))
  s$fr1 = 0
  s$fr2 = 14
  s$fr3 = 15
  s$sta = 15
  
  s[which(s$year==1952),"fr2"] = 1
  s[which(s$year==1952),"fr3"] = 9
  s[which(s$year==1946),"fr2"] = 7
  s[which(s$year==1951),"fr2"] = 2
  s[which(s$year==1951),"fr3"] = 10
  s[which(s$year==1960),"fr1"] = 1
  s[which(s$year==1990 & s$oid==47),"fr1"] = 10
  
  for (k in 1:length(s$nid)) {
    options(warn=0)
    Oldc = s$oid[k]  
    Trea = s$nid[k]
    Year = s$year[k]
    Case = s$case[k]
    Left= s$left[k]
    Set = s$final[k]
    
    data <- read_dta("ple_dataset.dta")
    data <- data[which(data$year >=Year- s$sta & data$year <=Year+15),]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul)))
    donors <- donors %>%  dplyr::filter(msimul != 1)
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(fstgdp) | cid==Oldc ))
    data$lgfstgdp <- log(data$fstgdp)
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, lgfstgdp)
    names(tysub)[names(tysub) == 'lgfstgdp'] <- 'ilgfstgdp'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = lgfstgdp-ilgfstgdp, t = year-Year+15)
    data <- transform(data, index = as.numeric(factor(country)))
    data <-  data %>% dplyr::mutate(d = replace(d, war==1, NA))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <- scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", 
                  time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,  e.order = e.order, e.lags = e.lags,  
                    u.alpha = u.alpha, e.alpha = e.alpha, rho = rho,  rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(s$fr1[k]:s$fr2[k], s$fr3[k]:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Case, left = Left)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    scl.insample  <- result$inference.results$CI.in.sample[, 1, drop = FALSE]
    scr.insample  <- result$inference.results$CI.in.sample[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclinsample = c(scl.insample), scrinsample = c(scr.insample),  sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    series  <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste(Set), series) 
  } 
  
  dfs <- lapply(ls(pattern="_"), function(x) get(x))
  finaldata <- rbindlist(dfs)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclinsample[finaldata$ti == 0] <- 0
  finaldata$scrinsample[finaldata$ti == 0] <- 0
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata$all <- 1
  finaldata$right <- NA
  finaldata$right <- ifelse(finaldata$left == 1, 0, 1)
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrinsample_all=mean(scrinsample[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclinsample_all=mean(sclinsample[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  dataforplot_left <- Reduce(function(x, y) merge(x, y, left=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_left=mean(yfit[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, yact_left=mean(yact[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrinsample_left=mean(scrinsample[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclinsample_left=mean(sclinsample[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrgauss_left=mean(scrgauss[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclgauss_left=mean(sclgauss[left==1], na.rm=TRUE))))
  
  dataforplot_right <- Reduce(function(x, y) merge(x, y, right=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_right=mean(yfit[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, yact_right=mean(yact[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrinsample_right=mean(scrinsample[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclinsample_right=mean(sclinsample[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrgauss_right=mean(scrgauss[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclgauss_right=mean(sclgauss[right==1], na.rm=TRUE))))
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all_gauss <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclgauss_all, ymax=scrgauss_all, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_left_gauss <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclgauss_left, ymax=scrgauss_left, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right_gauss <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclgauss_right, ymax=scrgauss_right, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_trends_all_insample <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclinsample_all, ymax=scrinsample_all, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_all, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_all, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))     
  
  gdp_trends_left_insample <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclinsample_left, ymax=scrinsample_left, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_left, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_left, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_right_insample <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclinsample_right, ymax=scrinsample_right, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_right, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_right, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_trends_insample = (gdp_trends_all_insample + gdp_trends_left_insample + gdp_trends_right_insample) &
    plot_annotation(title = 'Panel A: Accounting for in-sample uncertainty', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  gdp_trends_gauss = (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss) & 
    plot_annotation(title = 'Panel B: Accounting for out-of-sample uncertainty', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  wrap_elements(gdp_trends_insample) / wrap_elements(gdp_trends_gauss) 
  
  outpath <- file.path("figures", "FigureB7.pdf")
  ggsave(outpath, wrap_elements(gdp_trends_insample) / wrap_elements(gdp_trends_gauss), width = 23, height = 16, units = "cm")
  
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE B8

##############################################################################################################

try({
  
  wsoll1 <- function(X0,X1,V,pen=0.0){
    n = ncol(X0)
    Delta = diag(t(X0 - matrix(rep(1,n),ncol=n)%x%X1)%*%V%*%(X0 - matrix(rep(1,n),ncol=n)%x%X1))
    P = 2*t(X0)%*%V%*%X0
    q = t(-2*t(X0)%*%V%*%X1 + pen*Delta)
    sol = LowRankQP(Vmat=P,dvec=q,Amat=matrix(1, ncol=n),bvec=1,uvec=rep(1,n), method="LU")
    return(sol$alpha)
  }
  
  TZero <- function(x,tol=1e-6,scale=T){
    if(!all(x > 0)) stop("Some elements are negative!")
    y = ifelse(x < tol,0,x)
    if(scale) y = y/sum(y)
    return(y)
  }
  
  regsynth <- function(X0,X1,Y0,Y1,V,pen,tol=1e-6){
    func_list = c("wsoll1","TZero")
    sol = wsoll1(X0,X1,V,pen)
    sol = TZero(sol,tol)
    sol = round(sol,3)
    return(sol)
  }
  
  country <- c(1,1,1,1,5,6,6,9,16,16,16,16,25,28,29,29,30,36,38,41,41,42,47,50,54,55,56,60)
  year <- c(1946,1973,1989,2003,1952,1951,1990,1952,1952,1960,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,1990,2003,2000,2001,2003,1999)
  left <- c(1,1,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1)
  populists_core_total <- data.frame(country,year,left)
  populists_core_total$case <- paste(as.character(populists_core_total$country), as.character(populists_core_total$year), sep=".")
  populists_core_total$minus = -15
  populists_core_total$plus = 15
  cov_year = 5
  populists_core_total[which(populists_core_total$country==47 & populists_core_total$year==1990),"minus"] = -5
  data <- read_dta("ple_dataset.dta") 
  data = data %>% group_by(cid) %>%  dplyr::mutate(ipinstitutions = na.approx(institutions, na.rm=FALSE))
  data$institutions <- ifelse(is.na(data$institutions), data$ipinstitutions, data$institutions)
  mea <-  data %>%  group_by(year) %>%  summarise(ymi = mean(institutions, na.rm = T))
  data <- merge(data,mea)
  data$institutions <- ifelse(is.na(data$institutions), data$ymi, data$institutions)
  data$lgfstgdp <- log(data$fstgdp)
  data <- data[which(data$year <= 1913 | (data$year >=1919 & data$year <=1938)|data$year >=1946),]
  Y_synth = data.frame(Y0hat=numeric(), time=numeric())
  s = populists_core_total
  lambda= c(0,.00001,.01,.1,.15,seq(.25,5,.1)) 
  curve_RMSE =  matrix(NA,nrow=length(s$country),ncol=length(lambda))
  curve_bias = matrix(NA,nrow=length(s$country),ncol=length(lambda))
  Y_synth = data.frame(Y0hat=numeric(), time=numeric())
  
  for(l in 1:length(lambda)){
    for (k in 1:length(s$country)) { 
      i = s$country[k]
      date = s$year[k]
      other_pop = s[which(s$year==date & s$country != i),"country"]
      scm_data <- data[which(data$year>= date+s$minus[k] & data$year<=date+s$plus[k]),]
      scm_data <- scm_data[which(!(scm_data$cid %in% other_pop)),]
      na_index = which(is.na(scm_data[,"lgfstgdp"]))
      na_cid = scm_data$cid[na_index]
      na_cid = na_cid[!duplicated(na_cid)]
      scm_data <- scm_data[which(!(scm_data$cid %in% na_cid) | scm_data$cid==i),]
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp-lgfstgdp[which(year==date)])
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X3= mean(institutions[which(year < date)]))
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X4= bankcrisis[which(year == date)-1])
      scm_data_y = scm_data[c("cid","year","Y")]
      scm_data_x = scm_data_y[which(scm_data_y$year < date),]
      scm_data_y = spread(scm_data_y, year, Y)
      scm_data_x = dcast(setDT(scm_data_x), cid  ~year, value.var = c("Y"), sep = "")
      scm_data_cov = scm_data[c("cid","X3","X4")]
      scm_data_cov= scm_data_cov[!duplicated(scm_data_cov),]
      scm_data_x = merge(scm_data_x, scm_data_cov, by="cid")
      scm_mat_x = as.matrix(scm_data_x)
      scm_mat_y = as.matrix(scm_data_y)
      X_0 = t(subset(scm_mat_x, scm_mat_x[,"cid"]!=i))[-1,]
      X_1 = t(subset(scm_mat_x, scm_mat_x[,"cid"]==i))[-1]
      Y_0 = subset(scm_mat_y, scm_mat_y[,"cid"]!=i)[,-1]
      Y_1 = subset(scm_mat_y, scm_mat_y[,"cid"]==i)[-1]
      V = diag(nrow(X_0))
      W = regsynth(X_0,X_1,Y_0,Y_1,V,pen=lambda[l])
      Y_0hat = t(t(W)%*%Y_0)
      destr <- as.data.frame(Y_0hat)
      destr <- rownames_to_column(destr, "tyears")
      destr = subset(destr, select = c(1))
      destr$tyears <- as.numeric(destr$tyears) 
      destrvec = as.vector(unlist(destr$tyears))
      time = destrvec - date
      Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
      Y$left = ifelse(s$left[k]==1,1,0)
      Y$case = s$case[k]
      Y_synth = rbind(Y_synth, Y)
      tau = Y_1 - Y_0hat
      tau = do.call(rbind, Map(data.frame, tau=tau, time= time))
      tau = tau$tau[which(tau$time<=0 & tau$time>= -15)]
      RMSE = sqrt(mean(tau^2))
      Bias = abs(mean(tau))
      curve_RMSE[k,l] = RMSE
      curve_bias[k,l] = Bias
    }
  }
  
  mean_RMSE = apply(curve_RMSE,2,mean)
  mean_bias = apply(curve_bias,2,mean)
  lambda_opt_RMSE = min(lambda[which(mean_RMSE==min(mean_RMSE))])
  lambda_opt_bias= min(lambda[which(mean_bias==min(mean_bias))])
  
  lambda_final = c(0, 0.1, lambda_opt_RMSE)
  for(l in 1:length(lambda_final)){
    Y_synth = data.frame(Y0hat=numeric(), time=numeric())
    for (k in 1:length(s$country)) { 
      i = s$country[k]
      date = s$year[k]
      other_pop = s[which(s$year==date & s$country != i),"country"]
      scm_data <- data[which(data$year>= date+s$minus[k] & data$year<=date+s$plus[k]),]
      scm_data <- scm_data[which(!(scm_data$cid %in% other_pop)),]
      na_index = which(is.na(scm_data[,"lgfstgdp"]))
      na_cid = scm_data$cid[na_index]
      na_cid = na_cid[!duplicated(na_cid)]
      scm_data <- scm_data[which(!(scm_data$cid %in% na_cid) | scm_data$cid==i),]
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp-lgfstgdp[which(year==date)])
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X3= mean(institutions[which(year < date)]))
      scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(X4= bankcrisis[which(year == date)-1])
      scm_data_y = scm_data[c("cid","year","Y")]
      scm_data_x = scm_data_y[which(scm_data_y$year < date),]
      scm_data_y = spread(scm_data_y, year, Y)
      scm_data_x = dcast(setDT(scm_data_x), cid  ~year, value.var = c("Y"), sep = "")
      scm_data_cov = scm_data[c("cid","X3","X4")]
      scm_data_cov= scm_data_cov[!duplicated(scm_data_cov),]
      scm_data_x = merge(scm_data_x, scm_data_cov, by="cid")
      scm_mat_x = as.matrix(scm_data_x)
      scm_mat_y = as.matrix(scm_data_y)
      X_0 = t(subset(scm_mat_x, scm_mat_x[,"cid"]!=i))[-1,]
      X_1 = t(subset(scm_mat_x, scm_mat_x[,"cid"]==i))[-1]
      Y_0 = subset(scm_mat_y, scm_mat_y[,"cid"]!=i)[,-1]
      Y_1 = subset(scm_mat_y, scm_mat_y[,"cid"]==i)[-1]
      V = diag(nrow(X_0))
      W = regsynth(X_0,X_1,Y_0,Y_1,V,pen=lambda_final[l])
      Y_0hat = t(t(W)%*%Y_0)
      destr <- as.data.frame(Y_0hat)
      destr <- rownames_to_column(destr, "tyears")
      destr = subset(destr, select = c(1))
      destr$tyears <- as.numeric(destr$tyears) 
      destrvec = as.vector(unlist(destr$tyears))
      time = destrvec - date
      Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
      Y$left = ifelse(s$left[k]==1,1,0)
      Y$case = s$case[k]
      Y_synth = rbind(Y_synth, Y)
    }
    assign(paste0("Y_Synth",l), Y_synth, envir = .GlobalEnv)
  }
  
  no_penalty <- Y_Synth1
  fixed_penalty <- Y_Synth2
  optimal_penalty <- Y_Synth3
  
  optimal_penalty <- optimal_penalty %>% group_by(case) %>% dplyr::mutate(all = 1)
  optimal_penalty <- optimal_penalty %>% group_by(case) %>% dplyr::mutate(right = 0)
  optimal_penalty$right[optimal_penalty$left == 0] <- 1
  
  Y0hat_all <- ddply(optimal_penalty, .(time), summarise, Y0hat_all=mean(Y0hat[all==1], na.rm=TRUE))
  Y0hat_left <- ddply(optimal_penalty, .(time), summarise, Y0hat_left=mean(Y0hat[left==1], na.rm=TRUE)) 
  Y0hat_right <- ddply(optimal_penalty, .(time), summarise, Y0hat_right=mean(Y0hat[right==1], na.rm=TRUE))
  
  Y1_all <- ddply(optimal_penalty, .(time), summarise, Y1_all=mean(Y1[all==1], na.rm=TRUE))
  Y1_left <- ddply(optimal_penalty, .(time), summarise, Y1_left=mean(Y1[left==1], na.rm=TRUE)) 
  Y1_right <- ddply(optimal_penalty, .(time), summarise, Y1_right=mean(Y1[right==1], na.rm=TRUE))
  
  mergelist_all <- list(Y0hat_all, Y1_all)
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_all)
  
  mergelist_left <- list(Y0hat_left, Y1_left)
  dataforplot_left <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_left)
  
  mergelist_right <- list(Y0hat_right, Y1_right)
  dataforplot_right <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_right)
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all <- ggplot(dataforplot_all) + 
    geom_line(aes(y=Y0hat_all, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_all, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.40), breaks = c(-0.4,-0.2,0,0.2,0.4), labels = format(c("-40%","-20%","0%","+20%","+40%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gdp_trends_left <- ggplot(dataforplot_left) + 
    geom_line(aes(y=Y0hat_left, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_left, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.40), breaks = c(-0.4,-0.2,0,0.2,0.4), labels = format(c("-40%","-20%","0%","+20%","+40%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right <- ggplot(dataforplot_right) + 
    geom_line(aes(y=Y0hat_right, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_right, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.40), breaks = c(-0.4,-0.2,0,0.2,0.4), labels = format(c("-40%","-20%","0%","+20%","+40%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  (gdp_trends_all + gdp_trends_left + gdp_trends_right) 
  
  outpath <- file.path("figures", "FigureB8.pdf")
  ggsave(outpath, (gdp_trends_all + gdp_trends_left + gdp_trends_right), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

##############################################################################################################

#FIGURE B9

##############################################################################################################

try({
  
  ###FORMAT
  format_data_stag <- function(outcome, trt, unit, time, data) {
    trt_time <- data %>% group_by(unit) %>% summarise(trt_time=as.double((time)[(trt) == 1][1])) %>% dplyr::mutate(trt_time=replace_na(as.numeric(trt_time), Inf))
    t_int <- trt_time %>% dplyr::filter(is.finite(trt_time)) %>% summarise(t_int=max(trt_time)) %>% pull(t_int)
    mask <- data %>% inner_join(trt_time %>% dplyr::filter(is.finite(trt_time))) %>% dplyr::filter(time <= t_int) %>% dplyr::mutate(trt=1-trt) %>% dplyr::select(unit, time, trt_time, trt) %>% spread(time, trt) %>% dplyr::select(-trt_time, -unit) %>% as.matrix()
    Xy <- data %>% dplyr::select(unit, time, outcome) %>% spread(time, outcome) %>% dplyr::select(-unit) %>% as.matrix()
    pre_times <- data %>% dplyr::filter(time <= t_int) %>% dplyr::distinct(time) %>% pull(time)
    post_times <- data %>% dplyr::filter(time > t_int) %>% dplyr::distinct(time) %>% pull(time)
    X <- Xy[, as.character(pre_times), drop = F]
    y <- Xy[, as.character(post_times), drop = F]
    t_vec <- data %>% pull(time) %>% unique() %>% sort()
    trt <- sapply(trt_time$trt_time, function(x) which(t_vec == x)-1) %>% as.numeric() %>% replace_na(Inf)
    units <- data %>% dplyr::filter(time < t_int) %>% dplyr::select(unit, time, outcome) %>% spread(time, outcome) %>% pull(unit)
    return(list(X=X, trt=trt, y=y, mask=mask, t_vec=t_vec, units=units))}
  
  ###ELIGIBLE DONORS
  get_donors <- function(X, y, trt, units, Z, n_lags, n_leads, how = "knn", exact_covariates = NULL, ...) { donors <- get_eligible_donors(trt, units)
  nona_donors <- get_nona_donors(X, y, trt, n_lags, n_leads)
  donors <- lapply(1:length(donors), function(j) donors[[j]] & nona_donors[[j]])
  return(donors)}
  get_eligible_donors <- function(trt,units) { grps <- trt[is.finite(trt)]
  unitk <- units[is.finite(trt)]
  J <- length(grps)
  donors <- lapply(1:J, function(j) trt!= grps[j] & !(units %in% c(1.1,1.2,1.3,1.4,1.5,14.1,14.2,16.1,16.2,16.3,16.4,16.5,16.6, 22.1,22.2,22.3,28.1,
                                                                   28.2,29.1,29.2,29.3,30.1,36.1,36.2,36.3,36.4,40.1, 41.1,41.2,41.3,41.4,41.5, 41.6,42.1,42.2,42.3,43.1,43.2,47.1,48.1,5.1,55.1,56.1,57.1,58.1,58.2,
                                                                   58.3,58.4,59.1,6.1,6.2,6.3,6.4,6.5,6.6,60.1,60.2,9.1,9.2)) & !(units %in% floor(units[which(trt ==grps[j])])))
  return(donors)}
  get_nona_donors <- function(X, y, trt, n_lags, n_leads) { n <- length(trt)
  fulldat <- cbind(X, y)
  is_na <- is.na(fulldat[is.finite(trt), , drop = F])
  grps <- trt[is.finite(trt)]
  not_na <- !is.na(fulldat)
  J <- length(grps)
  lapply(1:J,function(j) { yr =(as.numeric(colnames(fulldat)[grps[j]])-n_lags):(as.numeric(colnames(fulldat)[grps[j]])+n_leads)
  idxs = which(colnames(fulldat) %in% yr)
  isna_j <- is_na[j, idxs]
  apply(not_na[, idxs, drop = F][, !isna_j, drop = F], 1, all) }) -> donors
  return(donors)}
  
  ###MULTI SYNTH QP MEAN
  multisynth_qp <- function(X, trt, mask, Z = NULL, n_leads=NULL, n_lags=NULL, relative=T, nu=0, lambda=0, V = NULL, time_cohort = FALSE, donors = NULL, norm_pool = NULL, norm_sep = NULL, verbose = FALSE, eps_rel=1e-4, eps_abs=1e-4,data) {
    if(!is.null(Z)) { if(ncol(Z) == 0) { Z <- NULL} }
    n <- if(typeof(X) == "list") dim(X[[1]])[1] else dim(X)[1]
    d <- if(typeof(X) == "list") dim(X[[1]])[2] else dim(X)[2]
    if(is.null(n_leads)) { n_leads <- d+1 } else if(n_leads > d) { n_leads <- d+1 }
    if(is.null(n_lags)) { n_lags <- d } else if(n_lags > d) { n_lags <- d }
    V <- make_V_matrix(n_lags, V)
    if(time_cohort) { grps <- unique(trt[is.finite(trt)])
    which_t <- lapply(grps, function(tj) (1:n)[trt == tj])
    mask <- unique(mask) } else { grps <- trt[is.finite(trt)]
    which_t <- (1:n)[is.finite(trt)] }
    J <- length(grps)
    if(is.null(norm_sep)) { norm_sep <- 1  }
    if(is.null(norm_pool)) { norm_pool <- 1  }
    n1 <- sapply(1:J, function(j) length(which_t[[j]]))
    if(is.null(donors)) { units <- data %>% arrange(!!unit) %>% dplyr::distinct(!!unit) %>% pull(!!unit)
    donors <- get_eligible_donors(trt, units) }
    if(typeof(X) == "list") { x_t <- lapply(1:J, function(j) colSums(X[[j]][which_t[[j]], mask[j,]==1, drop=F]))
    Xc <- lapply(1:nrow(mask), function(j) X[[j]][donors[[j]], mask[j,]==1, drop=F])
    sdx <- sd(X[[1]][is.finite(trt)])
    } else { x_t <- lapply(1:J, function(j) colSums(X[which_t[[j]], mask[j,]==1, drop=F]))
    x_t = lapply(1:J, function(j) x_t[[j]]-X[[which_t[[j]],grps[j]+1]])
    Xc <- lapply(1:nrow(mask), function(j) X[donors[[j]], mask[j,]==1, drop=F])
    Xc <- lapply(1:J, function(j) Xc[[j]]-X[donors[[j]],grps[j]+1])
    sdx <- sd(X[is.finite(trt)], na.rm=TRUE) }
    if(!is.null(Z)) {z_t <- lapply(1:J, function(j) colSums(Z[which_t[[j]], , drop = F]))
    z_t <- lapply(1:J, function(j)
      if(grps[j] == 78){ t(rbind(mean( z_t[[j]][(grps[j]-13):grps[j]]), z_t[[j]][138+grps[j]], mean( z_t[[j]][which(colnames(Z) == paste0("e",as.character(as.numeric(gsub("[^[:digit:]]", "", colnames(Z)[grps[j]+1])) - 14))):(grps[j]+276)])))
      }else {t(rbind(mean(z_t[[j]][which(colnames(Z) == paste0("i",as.character(as.numeric(gsub("[^[:digit:]]", "", colnames(Z)[grps[j]+1])) - 15))):grps[j]]),
                     z_t[[j]][138+grps[j]],mean( z_t[[j]][which(colnames(Z) == paste0("e",as.character(as.numeric(gsub("[^[:digit:]]", "", colnames(Z)[grps[j]+1])) - 15))):(grps[j]+276)])))})
    Zc <- lapply(1:J, function(j) Z[donors[[j]], , drop = F])
    Zc <- lapply(1:J, function(j)
      if(grps[j] == 78){t(rbind(rowMeans(Zc[[j]][,(grps[j]-13):grps[j]]),
                                t(t(Zc[[j]])[138+grps[j],]),rowMeans(Zc[[j]][,which(colnames(Z) == paste0("e",as.character(as.numeric(gsub("[^[:digit:]]", "", colnames(Z)[grps[j]+1])) - 14))):(grps[j]+276)])))
      }else {t(rbind(rowMeans( Zc[[j]][,which(colnames(Z) == paste0("i",as.character(as.numeric(gsub("[^[:digit:]]", "", colnames(Z)[grps[j]+1])) - 15))):grps[j]]),
                     t(t(Zc[[j]])[138+grps[j],]),rowMeans( Zc[[j]][,which(colnames(Z) == paste0("e",as.character(as.numeric(gsub("[^[:digit:]]", "", colnames(Z)[grps[j]+1])) - 15))):(grps[j]+276)])))})
    } else { z_t <- lapply(1:J, function(j) c(0))
    Zc <- lapply(1:J, function(j) Matrix::Matrix(0,nrow = sum(donors[[j]]),ncol = 1))}
    dz <- ncol(Zc[[1]])
    x_t <- lapply(x_t, function(xtk) tidyr::replace_na(xtk, 0))
    Xc <- lapply(Xc, function(xck) apply(xck, 2, tidyr::replace_na, 0))
    n0s <- sapply(Xc, nrow)
    n0 <- sum(n0s)
    const_mats <- make_constraint_mats(trt, grps, n_leads, n_lags, Xc, Zc, d, n1)
    Amat <- const_mats$Amat
    lvec <- const_mats$lvec
    uvec <- const_mats$uvec
    qvec <- make_qvec(grps, Xc, x_t, z_t, nu, n_lags, d, V, norm_pool, norm_sep)
    Pmat <- make_Pmat(grps,Xc, x_t, dz, nu, n_lags, lambda, d, V, norm_pool, norm_sep)
    settings <- do.call(osqp::osqpSettings, c(list(verbose = verbose, eps_rel = eps_rel, eps_abs = eps_abs)))
    out <- osqp::solve_osqp(Pmat, qvec, Amat, lvec, uvec, pars = settings)
    total_ctrls <- n0 * J
    weights <- matrix(out$x[1:total_ctrls], nrow = n0)
    nj0 <- as.numeric(lapply(Xc, nrow))
    nj0cumsum <- c(0, cumsum(nj0))
    imbalance <- vapply(1:J, function(j) { dj <- length(x_t[[j]])
    if(grps[j] == (64|67|69|70|71|72)){ ndim<-8 }else if(grps[j] == (58|78)){ ndim <- 14 } else if(grps[j] == 55){ ndim <- 11
    } else if(grps[j] == (45|47|53)){ ndim <- 200 }else{  ndim <- min(dj, n_lags)}
    c(numeric(d-ndim),  x_t[[j]][(dj-ndim+1):dj] - t(Xc[[j]][,(dj-ndim+1):dj, drop = F]) %*%  out$x[(nj0cumsum[j] + 1):nj0cumsum[j + 1]]) }, numeric(d))
    avg_imbal <- rowMeans(t(t(imbalance)))
    Vsq <- t(V) %*% V
    global_l2 <- c(sqrt(t(avg_imbal[(d - n_lags + 1):d]) %*% Vsq %*% avg_imbal[(d - n_lags + 1):d])) / sqrt(d)
    avg_l2 <- mean(apply(imbalance, 2,  function(x) c(sqrt(t(x[(d - n_lags + 1):d]) %*% Vsq %*%  x[(d - n_lags + 1):d]))))
    ind_l2 <- sqrt(mean( apply(imbalance, 2,  function(x) c(x[(d - n_lags + 1):d] %*% Vsq %*% x[(d - n_lags + 1):d]) /  sum(x[(d - n_lags + 1):d] != 0))))
    vapply(1:J, function(j) {  weightj <- numeric(n)
    weightj[donors[[j]]] <- out$x[(nj0cumsum[j] + 1):nj0cumsum[j + 1]]
    weightj  },  numeric(n)) -> weights
    weights <- t(t(weights) / n1)
    output <- list(weights = weights, imbalance = cbind(avg_imbal, imbalance), global_l2 = global_l2,  ind_l2 = ind_l2, avg_l2 = avg_l2,  V = V)
    if(!is.null(Z)) { z_t <- sapply(1:J, function(j) colMeans(Z[which_t[[j]], , drop = F]))
    imbal_z <- z_t - t(Z) %*% weights
    avg_imbal_z <- rowSums(t(t(imbal_z) * n1)) / sum(n1)
    global_l2_z <- sqrt(sum(avg_imbal_z ^ 2))
    ind_l2_z <- sum(apply(imbal_z, 2, function(x) sqrt(sum(x ^ 2))))
    imbal_z <- cbind(avg_imbal_z, imbal_z)
    rownames(imbal_z) <- colnames(Z)
    output$imbalance_aux <- imbal_z
    output$global_l2_aux <- global_l2_z
    output$ind_l2_aux <- ind_l2_z}
    return(output)}
  make_constraint_mats <- function(trt, grps, n_leads, n_lags, Xc, Zc, d, n1) { J <- length(grps)
  idxs0 <- trt > n_leads + min(grps)
  n0 <- sum(idxs0)
  A1 <- do.call(Matrix::bdiag, lapply(1:(J), function(x) rep(1, n0)))
  A1 <- Matrix::bdiag(lapply(1:J, function(j) rep(1, nrow(Xc[[j]]))))
  Amat <- as.matrix(Matrix::t(A1))
  Amat <- Matrix::rbind2(Matrix::t(A1), Matrix::Diagonal(nrow(A1)))
  dz <- ncol(Zc[[1]])
  A_trans1 <- do.call(Matrix::bdiag, lapply(1:J, function(j) { dj <- ncol(Xc[[j]])
  if(grps[j] == (64|67|69|70|71|72)){ ndim<-8 }else if(grps[j] == (58|78)){ ndim <- 14
  } else if(grps[j] == 55){ ndim <- 11  } else if(grps[j] == (45|47|53)){  ndim <- 200 }else{ ndim <- min(dj, n_lags)}
  max_dim <- min(d, n_lags)
  mat <- Xc[[j]][, (dj - ndim + 1):dj, drop = F]
  n0 <- nrow(mat)
  zero_mat <- Matrix::Matrix(0, n0, max_dim - ndim)
  Matrix::t(cbind(zero_mat, mat)) }))
  sum_tj <- min(d, n_lags) * J
  A_trans2 <- - Matrix::Diagonal(sum_tj)
  A_trans <- Matrix::cbind2(
    Matrix::cbind2(A_trans1, A_trans2),
    Matrix::Matrix(0, nrow = nrow(A_trans1), ncol = dz * J))
  A_transz <- Matrix::t(Matrix::bdiag(Zc))
  A_transz <- Matrix::cbind2(
    Matrix::cbind2(A_transz, Matrix::Matrix(0, nrow = nrow(A_transz), ncol = sum_tj)), -Matrix::Diagonal(dz * J))
  Amat <- Matrix::cbind2(Amat,  Matrix::Matrix(0, nrow = nrow(Amat),  ncol = sum_tj + dz * J))
  Amat <- Matrix::rbind2(Matrix::rbind2(Amat, A_trans), A_transz)
  lvec <- c(n1,  numeric(nrow(A1)), numeric(sum_tj), numeric(dz * J) )
  uvec <- c(n1, rep(Inf, nrow(A1)), numeric(sum_tj), numeric(dz * J) )
  return(list(Amat = Amat, lvec = lvec, uvec = uvec))}
  make_qvec <- function(grps,Xc, x_t, z_t, nu, n_lags, d, V, norm_pool, norm_sep) { J <- length(x_t)
  Vsq <- t(V) %*% V
  qvec <- lapply(1:J, function(j) { dj <- length(x_t[[j]])
  if(grps[j] == (64|67|69|70|71|72)){ ndim<-8 }else if(grps[j] == (58|78)){  ndim <- 14
  } else if(grps[j] == 55){  ndim <- 11 } else if(grps[j] == (45|47|53)){ ndim <- 200
  }else{ ndim <- min(dj, n_lags)}
  max_dim <- min(d, n_lags)
  vec <- x_t[[j]][(dj - ndim + 1):dj] / ndim
  Vsq %*% c(numeric(max_dim - ndim), vec) })
  avg_target_vec <- lapply(1:J,  function(j) { dk <- length(x_t[[j]])
  if(grps[j] == (64|67|69|70|71|72)){   ndim<-8 }else if(grps[j] == (58|78)){ ndim <- 14
  } else if(grps[j] == 55){ ndim <- 11 } else if(grps[j] == (45|47|53)){ ndim <- 200 }else{ ndim <- min(dk, n_lags)}
  max_dim <- min(d, n_lags)
  x_t[[j]][(dk - ndim + 1):dk]}) %>% reduce(`+`) %*% Vsq
  qvec_avg <- rep(avg_target_vec, J)
  qvec <- - (nu * qvec_avg / (norm_pool * n_lags * J ^ 2) + (1 - nu) * reduce(qvec, c) / (norm_sep * J))
  qvec_avg_z <- z_t %>% reduce(`+`)
  qvec_avg_z <- rep(qvec_avg_z, J)
  qvec_z <- - (nu * qvec_avg_z / (norm_pool * J ^ 2) +  (1 - nu) * reduce(z_t, c) / (norm_sep * J)) / length(z_t[[1]])
  total_ctrls <- lapply(Xc, nrow) %>% reduce(`+`)
  return(c(numeric(total_ctrls), qvec, qvec_z))}
  make_Pmat <- function(grps,Xc, x_t, dz, nu, n_lags, lambda, d, V, norm_pool, norm_sep) { J <- length(x_t)
  Vsq <- t(V) %*% V
  ndims <- vapply(1:J,  function(j) if(grps[j] <= 70){8 } else{  min(length(x_t[[j]]), n_lags)} ,  numeric(1))
  max_dim <- min(d, n_lags)
  total_dim <- sum(ndims)
  total_dim <- max_dim * J
  V1 <- Matrix::bdiag(lapply(ndims, function(ndim) Matrix::Diagonal(max_dim, 1 / ndim)))
  V1 <- Matrix::bdiag(lapply(ndims, function(ndim) Vsq / ndim))
  tile_sparse <- function(j) { kronecker(Matrix::Matrix(1, nrow = j, ncol = j), Vsq)}
  tile_sparse_cov <- function(d, j) { kronecker(Matrix::Matrix(1, nrow = j, ncol = j), Matrix::Diagonal(d))}
  V2 <- tile_sparse(J) / n_lags
  Pmat <- nu * V2 / (norm_pool * J ^ 2) + (1 - nu) * V1 / (norm_sep * J)
  V1_z <- Matrix::Diagonal(dz * J, 1 / dz)
  V2_z <- tile_sparse_cov(dz, J) / dz
  Pmat_z <- nu * V2_z / (norm_pool * J ^ 2) + (1 - nu) * V1_z / (norm_sep * J)
  total_ctrls <- lapply(Xc, nrow) %>% reduce(`+`)
  Pmat <- Matrix::bdiag(Matrix::Matrix(0, nrow = total_ctrls,  ncol = total_ctrls), Pmat, Pmat_z)
  I0 <- Matrix::bdiag(Matrix::Diagonal(total_ctrls),  Matrix::Matrix(0, nrow = total_dim + dz * J, ncol = total_dim + dz * J))
  return(Pmat + lambda * I0)}
  
  ###MULTISYNTH FORMATTED
  multisynth_formatted <- function(wide, relative=T, n_leads, n_lags, nu, lambda, V, force, n_factors,
                                   scm, time_cohort, time_w, lambda_t,  fit_resids, eps_abs, eps_rel, verbose, long_df, how_match,data, ...) {
    if(time_cohort) { grps <- unique(wide$trt[is.finite(wide$trt)]) } else {  grps <- wide$trt[is.finite(wide$trt)]  }
    J <- length(grps)
    if(time_w) { out <- fit_time_reg(cbind(wide$X, wide$y), wide$trt, n_leads, lambda_t, ...)
    y0hat <- out$y0hat
    residuals <- out$residuals
    params <- out$time_weights } else if(is.null(n_factors)) { out <- tryCatch({
      fit_gsynth_multi(long_df, cbind(wide$X, wide$y), wide$trt, force=force) })
    y0hat <- out$y0hat
    params <- out$params
    n_factors <- ncol(params$factor)
    residuals <- cbind(wide$X, wide$y) - y0hat
    } else if (n_factors != 0) { out <- fit_gsynth_multi(long_df, cbind(wide$X, wide$y), wide$trt, r=n_factors, CV=0, force=force)
    y0hat <- out$y0hat
    params <- out$params    
    residuals <- cbind(wide$X, wide$y) - y0hat } else if(force == 0 & n_factors == 0) {
      pure_ctrl <- cbind(wide$X, wide$y)[!is.finite(wide$trt), , drop = F]
      y0hat <- matrix(colMeans(pure_ctrl, na.rm = TRUE),  nrow = nrow(wide$X), ncol = ncol(pure_ctrl),  byrow = T)
      residuals <- cbind(wide$X, wide$y) - y0hat
      params <- NULL} else { fullmask <- cbind(wide$mask, matrix(0, nrow=nrow(wide$mask),  ncol=ncol(wide$y)))
      out <- fit_feff(cbind(wide$X, wide$y), wide$trt, fullmask, force)
      y0hat <- out$y0hat
      residuals <- out$residuals
      params <- NULL}
    if(fit_resids) {
      if(time_w) { fullmask <- cbind(wide$mask, matrix(0, nrow=nrow(wide$mask),ncol=ncol(wide$y)))
      out <- fit_feff(cbind(wide$X, wide$y), wide$trt, fullmask, force)
      bal_mat <- lapply(out$residuals, function(x) x[,1:ncol(wide$X)])} else if(typeof(residuals) == "list") {
        bal_mat <- lapply(residuals, function(x) x[,1:ncol(wide$X)])} else { bal_mat <- residuals[,1:ncol(wide$X)]}
    } else {ctrl_avg <- matrix(colMeans(wide$X[!is.finite(wide$trt), , drop = F]), nrow = nrow(wide$X), ncol = ncol(wide$X), byrow = T)
    bal_mat <- wide$X - ctrl_avg
    bal_mat <- wide$X}
    if(scm) {units <- data %>% arrange(unit) %>% dplyr::distinct(unit) %>% pull(unit)
    donors <- get_donors(wide$X, wide$y, wide$trt,units,  wide$Z, n_lags, n_leads, how = how_match, exact_covariates = wide$exact_covariates)
    sep_fit <- multisynth_qp(X=bal_mat, trt=wide$trt, mask=wide$mask,  Z = wide$Z, n_leads=n_leads, n_lags=n_lags, relative=relative, 
                             nu=0, lambda=lambda, V = V,time_cohort = time_cohort, donors = donors,eps_rel = eps_rel,eps_abs = eps_abs,verbose = verbose,data=data)
    if(is.null(nu)) { glbl <- sep_fit$global_l2 * sqrt(nrow(sep_fit$imbalance))
    ind <- sep_fit$avg_l2
    nu <- glbl / ind}
    msynth <- multisynth_qp(X=bal_mat, trt=wide$trt, mask=wide$mask, Z = wide$Z, n_leads=n_leads, n_lags=n_lags, relative=relative,
                            nu=nu, lambda=lambda,V = V, time_cohort = time_cohort, donors = donors,norm_pool = sep_fit$global_l2 ^ 2,
                            norm_sep = sep_fit$ind_l2 ^ 2, eps_rel = eps_rel, eps_abs = eps_abs, verbose = verbose) } else {
                              msynth <- list(weights = matrix(0, nrow = nrow(wide$X), ncol = J),  imbalance=NA, global_l2=NA, ind_l2=NA)}
    msynth$data <- wide
    msynth$relative <- relative
    msynth$n_leads <- n_leads
    msynth$n_lags <- n_lags
    msynth$nu <- nu
    msynth$lambda <- lambda
    msynth$scm <- scm
    msynth$time_cohort <- time_cohort
    msynth$grps <- grps
    msynth$y0hat <- y0hat
    msynth$residuals <- residuals
    msynth$n_factors <- n_factors
    msynth$force <- force
    msynth$params <- params
    msynth$scm <- scm
    msynth$time_w <- time_w
    msynth$lambda_t <- lambda_t
    msynth$fit_resids <- fit_resids
    msynth$extra_pars <- c(list(eps_abs = eps_abs, eps_rel = eps_rel, verbose = verbose),  list(...))
    msynth$long_df <- long_df
    msynth$how_match <- how_match
    msynth$donors <- donors
    class(msynth) <- "multisynth"
    return(msynth)}
  
  ###FIT SYNTH
  make_V_matrix <- function(t0, V) {
    if(is.null(V)) { V <- diag(rep(1, t0)) } else if(is.vector(V)) {
      if(length(V) != t0) { stop(paste("`V` must be a vector with", t0, "elements or a", t0, "x", t0, "matrix"))}
      V <- diag(V) } else if(ncol(V) == 1 & nrow(V) == t0) { V <- diag(c(V)) } else if(ncol(V) == t0 & nrow(V) == 1) { V <- diag(c(V))
      } else if(nrow(V) == t0) {} else {stop(paste("`V` must be a vector with", t0, "elements or a", t0,  "x", t0, "matrix")) }
    return(V)}
  fit_synth_formatted <- function(synth_data, V = NULL) {t0 <- dim(synth_data$Z0)[1]
  V <- make_V_matrix(t0, V)
  weights <- synth_qp(synth_data$X1, t(synth_data$X0), V)
  l2_imbalance <- sqrt(sum((synth_data$Z0 %*% weights - synth_data$Z1)^2))
  uni_w <- matrix(1/ncol(synth_data$Z0), nrow=ncol(synth_data$Z0), ncol=1)
  unif_l2_imbalance <- sqrt(sum((synth_data$Z0 %*% uni_w - synth_data$Z1)^2))
  scaled_l2_imbalance <- l2_imbalance / unif_l2_imbalance
  return(list(weights=weights,l2_imbalance=l2_imbalance,scaled_l2_imbalance=scaled_l2_imbalance))}
  synth_qp <- function(X1, X0, V) { Pmat <- X0 %*% V %*% t(X0)
  qvec <- - t(X1) %*% V %*% t(X0)
  n0 <- nrow(X0)
  A <- rbind(rep(1, n0), diag(n0))
  l <- c(1, numeric(n0))
  u <- c(1, rep(1, n0))
  settings = osqp::osqpSettings(verbose = FALSE, eps_rel = 1e-8, eps_abs = 1e-8)
  sol <- osqp::solve_osqp(P = Pmat, q = qvec, A = A, l = l, u = u, pars = settings)
  return(sol$x)}
  
  ###OUCOME MULTI
  fit_gsynth_multi <- function(long_df, X, trt, r=0, force=3, CV=1) {
    ttot <- ncol(X)
    n <- nrow(X)
    labels <- colnames(long_df)
    gsyn <- gsynth::gsynth(data = long_df, Y = labels[4], D = labels[3], index = c(labels[1], labels[2]), force = force, CV = CV, r=r)
    y0hat <- matrix(0, nrow=n, ncol=ttot)
    y0hat[!is.finite(trt),] <- t(gsyn$Y.co - gsyn$est.co$residuals)
    y0hat[is.finite(trt),] <- t(gsyn$Y.ct)
    gsyn$est.co$Y.ct <- gsyn$Y.ct
    return(list(y0hat=y0hat, params=gsyn$est.co))}
  fit_feff <- function(X, trt, mask, force) { ttot <- ncol(X)
  n <- nrow(X)
  grps <- unique(trt[is.finite(trt)])
  J <- length(grps)
  which_t <- (1:n)[is.finite(trt)]
  if(force %in% c(2,3)) { time_eff <- matrix(colMeans(X[!is.finite(trt),, drop = F],na.rm = TRUE),nrow=nrow(X),ncol=ncol(X),byrow=T)
  } else {time_eff <- matrix(0, nrow = nrow(X), ncol = ncol(X))}
  residuals <- X - time_eff
  y0hat <- time_eff
  if(force %in% c(1,3)) { unit_eff <- lapply(grps, function(tj) matrix( rowMeans(residuals[, 1:tj, drop = F], na.rm = TRUE), nrow=nrow(X), ncol=ncol(X)))
  residuals <- lapply(1:J, function(j) residuals -  unit_eff[[j]])
  y0hat <- unit_eff}
  if(force == 3) { y0hat <- lapply(unit_eff, function(ufj) time_eff + ufj)}
  if(force %in% c(1,3)) { names(residuals) <- as.character(grps)
  residuals <- residuals[as.character(trt[is.finite(trt)])]
  names(y0hat) <- as.character(grps)
  y0hat <- y0hat[as.character(trt[is.finite(trt)])]}
  return(list(y0hat = y0hat,residuals = residuals))}
  
  
  cid <- c(1,1.1,1.2,1.3,5,6,6.1,9,16,16.1,16.2,16.3,25,28,29,29.1,30,36,38,41,41.1,42,47,50,54,55,56,60)
  year <- c(1946,1973,1989,2003,1952,1951,1990,1952,1952,1960,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,1990,2003,2000,2001,2003,1999)
  left <- c(1,1,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1)
  populists_core_total <- data.frame(cid,year,left)
  populists_core_total$case <- paste(as.character(populists_core_total$cid), as.character(populists_core_total$year), sep=".")
  populists_core_total$minus = -15
  populists_core_total$plus = 15
  cov_year = 5
  populists_core_total[which(populists_core_total$cid==47 & populists_core_total$year==1990),"minus"] = -5
  populists_core_total$treatment = 1
  data <- read_dta("ple_dataset.dta")
  data$lgfstgdp <- log(data$fstgdp)
  data <- dplyr::select(data,   country, cid, iso, year, fstgdp, lgfstgdp, institutions, bankcrisis, gini, inflation, global)
  setnames(data, "fstgdp", "fstgdp2")
  setnames(data, "lgfstgdp", "lgfstgdp2")
  setnames(data, "institutions", "i")
  setnames(data, "bankcrisis", "c")
  setnames(data, "gini", "new_c")
  setnames(data, "inflation", "e")
  setnames(data, "global", "_merge")
  datafm <- data
  data <- data[which(data$year <= 1913 | (data$year >=1919 & data$year <=1938) |data$year >=1946),]
  cid_1.1 = data[which(data$cid==1),]
  cid_1.2 = data[which(data$cid==1),]
  cid_1.3 = data[which(data$cid==1),]
  cid_1.1$cid = 1.1
  cid_1.2$cid = 1.2
  cid_1.3$cid = 1.3
  cid_16.1 = data[which(data$cid==16),]
  cid_16.2 = data[which(data$cid==16),]
  cid_16.3 = data[which(data$cid==16),]
  cid_16.1$cid = 16.1
  cid_16.2$cid = 16.2
  cid_16.3$cid = 16.3
  cid_6.1 = data[which(data$cid==6),]
  cid_6.1$cid = 6.1
  cid_29.1 = data[which(data$cid==29),]
  cid_29.1$cid = 29.1
  cid_41.1 = data[which(data$cid==41),]
  cid_41.1$cid = 41.1
  populists=populists_core_total
  data_dup = rbind(data, cid_1.1, cid_1.2, cid_1.3, cid_6.1, cid_16.1, cid_16.2, cid_16.3, cid_29.1, cid_41.1)
  data_dup = data_dup %>% arrange(cid)
  data_dup <- merge(data_dup, populists, by = c("cid", "year"), all = TRUE)
  data_nopop = data_dup[-which(data_dup$cid %in% populists$cid),]
  data_nopop$trt = 0
  data_pop =data_dup %>% group_by(cid) %>% dplyr::filter(cid %in% populists$cid) %>% dplyr::mutate( trt = 1*(year >= year[which(treatment==1)])) 
  data_dup = rbind(data_nopop, data_pop)
  data_dup = data_dup %>% arrange(cid)
  remove("year")
  colnames(data_dup)[1] = "unit"
  colnames(data_dup)[6] = "outcome"
  colnames(data_dup)[2] = "time"
  data = data_dup
  form = outcome ~ trt | i + c
  form <- Formula::Formula(form)
  outcome <- terms(formula(form, rhs=1))[[2]]
  trt <- terms(formula(form, rhs=1))[[3]]
  wide <- format_data_stag(outcome, trt, unit, time, data)
  long_df <- data[c("unit" ,"time", "trt", "outcome")]
  cov = data[c("unit", "time", "i", "c", "e")]
  n_leads=15
  n_lags=15
  lambda=0
  eps_abs = 1e-4
  eps_rel = 1e-4
  verbose = FALSE
  time_cohort=FALSE
  V=NULL
  scm = TRUE
  n_factors = 0
  fixedeff = FALSE
  how_match = "knn"
  force <- if(fixedeff) 3 else 2
  msynth <- multisynth_formatted(wide = wide, relative = T, n_factors=n_factors,
                                 force=force, n_leads = n_leads, n_lags = n_lags,
                                 nu = NULL, lambda = lambda, V = V,
                                 scm = scm, time_cohort = time_cohort,
                                 time_w = F, lambda_t = 0,
                                 eps_abs = eps_abs,fit_resids = TRUE,how_match=how_match,data=data,
                                 eps_rel = eps_rel, verbose = verbose, long_df = long_df
  )
  weight=round(msynth[["weights"]],3)
  units <- data %>% arrange(unit) %>% dplyr::distinct(unit) %>% pull(unit)
  id = which(units %in% c(1.1,1.2,1.3,6.1,16.1,16.2,16.3,29.1,41.1))
  assign(paste0("weights",1), weight[-id,])
  data <- datafm
  data <- data[which(data$year <= 1913 | (data$year >=1919 & data$year <=1938) |data$year >=1946),]
  populists$cid = floor(populists$cid)
  Y_synth = data.frame(Y0hat=numeric(), time=numeric())
  for(k in 1:length(populists$cid)){ 
    i = populists$cid[k]
    date = populists$year[k]
    scm_data <- data[which(data$year>= date+populists$minus[k] & data$year<=date+populists$plus[k]),]
    scm_data = scm_data %>% group_by(cid) %>% dplyr::mutate(Y = lgfstgdp2-lgfstgdp2[which(year==date)])
    scm_data = scm_data[c("cid","year","Y")]
    scm_data = spread(scm_data, year,Y)
    rownames(scm_data) = scm_data$cid
    scm_mat = as.matrix(scm_data)
    Y_0 = subset(scm_mat)[,-1] %>% replace_na(0)
    Y_1 = subset(scm_mat, scm_mat[,"cid"]==i)[,-1]
    weights = eval(parse(text=paste0("weights",1)))
    Y_0hat = t(t(weights[,k])%*%Y_0)
    destr <- as.data.frame(Y_0hat)
    destr <- rownames_to_column(destr, "tyears")
    destr = subset(destr, select = c(1))
    destr$tyears <- as.numeric(destr$tyears) 
    destrvec = as.vector(unlist(destr$tyears))
    time = destrvec - date
    Y = do.call(rbind, Map(data.frame, Y0hat = Y_0hat, Y1 = Y_1, time= time))
    Y$left = ifelse(populists$left[k]==1,1,0)
    Y$case = populists$case[k]
    Y_synth = rbind(Y_synth, Y)
  }
  Y_synth = na.omit(Y_synth)
  Y_synth <- Y_synth %>% group_by(case) %>% dplyr::mutate(all = 1)
  Y_synth <- Y_synth %>% group_by(case) %>% dplyr::mutate(right = 0)
  Y_synth$right[Y_synth$left == 0] <- 1
  
  Y0hat_all <- ddply(Y_synth, .(time), summarise, Y0hat_all=mean(Y0hat[all==1], na.rm=TRUE))
  Y0hat_left <- ddply(Y_synth, .(time), summarise, Y0hat_left=mean(Y0hat[left==1], na.rm=TRUE)) 
  Y0hat_right <- ddply(Y_synth, .(time), summarise, Y0hat_right=mean(Y0hat[right==1], na.rm=TRUE))
  
  Y1_all <- ddply(Y_synth, .(time), summarise, Y1_all=mean(Y1[all==1], na.rm=TRUE))
  Y1_left <- ddply(Y_synth, .(time), summarise, Y1_left=mean(Y1[left==1], na.rm=TRUE)) 
  Y1_right <- ddply(Y_synth, .(time), summarise, Y1_right=mean(Y1[right==1], na.rm=TRUE))
  
  mergelist_all <- list(Y0hat_all, Y1_all)
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_all)
  
  mergelist_left <- list(Y0hat_left, Y1_left)
  dataforplot_left <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_left)
  
  mergelist_right <- list(Y0hat_right, Y1_right)
  dataforplot_right <- Reduce(function(x, y) merge(x, y, all=TRUE), mergelist_right)
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all <- ggplot(dataforplot_all) + 
    geom_line(aes(y=Y0hat_all, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_all, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_fill_manual(name = '', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.60, 0.60), breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-60%","-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gdp_trends_left <- ggplot(dataforplot_left) + 
    geom_line(aes(y=Y0hat_left, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_left, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_fill_manual(name = '', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.60, 0.60), breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-60%","-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right <- ggplot(dataforplot_right) + 
    geom_line(aes(y=Y0hat_right, x=time, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=Y1_right, x=time, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_fill_manual(name = '', values=c("Populist avg." = "black", "Doppelganger avg." = "black")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.60, 0.60), breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-60%","-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  (gdp_trends_all + gdp_trends_left + gdp_trends_right) 
  
  outpath <- file.path("figures", "FigureB9.pdf")
  ggsave(outpath, (gdp_trends_all + gdp_trends_left + gdp_trends_right), width = 18, height = 6, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = TRUE)

file.remove("ple_dataset.dta")

####Remove newly installed libraries####
# Load the installed.packages function to get a list of currently installed packages
installed <- rownames(installed.packages())

# List of packages you want to keep
# Add more package names as needed
keep_packages <- c("LowRankQP")

# Determine the packages to remove
remove_packages <- setdiff(installed, keep_packages)

# Remove the packages
if(length(remove_packages) > 0) {
  message("Removing packages from the project library: ", paste(remove_packages, collapse = ", "))
  remove.packages(remove_packages, lib = .libPaths()[1])
} else {
  message("No packages to remove from the project library.")
}

file.remove('allstata.log')

file.remove(file.path('programs','.Rhistory'))
unlink(file.path('programs','.Rproj.user'), recursive = TRUE)
