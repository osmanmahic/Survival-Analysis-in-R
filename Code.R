#######################################################################################
# File name: Code.R
# Author: Osman Mahic
# Date: 24-06-2024
# Description: R code for performing survival analysis
#######################################################################################

#Set-up data
#Calculate survival times for all-cause mortality  
library(dplyr)
comp_data_check <- comp_data %>% mutate(status=ifelse(!is.na(deceased_date),
1,0)) %>%
mutate(time=ifelse(!is.na(deceased_date), deceased_date-index_dt,
censor_dt-index_dt)) %>%
mutate(time = round(time/30.417,digit=2)) %>%
mutate(time=as.numeric(time)) %>%
mutate(periode=case_match(periode,'post'~'Post','pre'~'Pre')) %>%
mutate(periode=as.factor(periode)) %>% filter(time>=0)

library(survival)
library(survminer)
library(gtsummary)
#Fit Cox PH
cox <- coxph(Surv(time,status)~factor(periode),data=comp_data_check)
summary(cox)
coxph(Surv(time,status)~factor(periode),data=comp_data_check) %>%
tbl_regression(exp=T)

#Calculate absolute risks with 95% CIs for all-cause mortality
survfit(Surv(time,status)~periode,data=comp_data_check) %>%
tbl_survfit(times=24,reverse=T,statistic =
'{estimate}({conf.low},{conf.high})'
est_24<- summary(survfit(Surv(time,status)~periode,
data=comp_data_check),times=24)
,style_percent(digits=3))
est_pre <- est_24$surv[1]
est_sd_pre <- est_24$std.err[1]
est_post <-est_24$surv[2]
est_sd_post <-est_24$std.err[2]
diff_est <- est_post- est_pre
diff_est*100
diff_std <- sqrt(est_sd_postˆ2 + est_sd_preˆ2)
store <- diff_est + c(-1,1) * 1.96 * diff_std
store*100

#Caclulate events and median follow-up times
table(comp_data_check$status,comp_data_check$periode)
aggregate(comp_data_check$time, by=list(factor(comp_data_check$periode)),
FUN=median)

###Create survival plots
library(ggsurvfit)
library(ggplot2)
library(showtext)
font_add_google('Nunito Sans')
font_add('Montserrat', 'MONTS.TTF')
font_paths()
font_files()
showtext_auto()

fit2 <- survfit2(Surv(time, status) ~ periode, data = comp_data_check)

windows()
plot_def <- fit2 %>% 
  ggsurvfit(type = 'risk', linewidth = 1) +
  add_confidence_interval() +
  add_risktable(
    risktable_stats = 'n.risk',
    stats_label = list(n.risk = 'Number at risk'),
    risktable_group = 'auto',
    family = 'Nunito Sans',
    size = 4,
    theme = list(
      theme_risktable_default(axis.text.y.size = 12, plot.title.size = 12),
      theme(plot.title = element_text(face = 'bold'),
            text = element_text(family = 'Nunito Sans', size = 4))
    )
  )

plot_def + 
  coord_cartesian(xlim = c(0, 24)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black'),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_ggsurvfit(x_scales = list(breaks = seq(0, 24, by = 6))) +
  labs(
    y = substitute(paste(bold('Cumulative incidence (%)'))),
    x = substitute(paste(bold('Months since AKI'))),
    size = 4
  ) +
  theme(
    axis.title.x = element_text(vjust = -0.2),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12, colour = 'black')
  ) +
  scale_color_manual(values = c('#d85489', '#d4d151'), labels = c('Post', 'Pre')) +
  scale_fill_manual(values = c('#d85489', '#d4d151'), labels = c('Post', 'Pre')) +
  theme(legend.position = 'none') +
  geom_text(
    data = annotation, aes(x = x, y = y, label = label),
    family = 'Nunito Sans',
    size = 4
  )

annotation <- data.frame(
  x = c(12, 18),
  y = c(0.36, 0.27),
  label = c('Pre', 'Post')
)




