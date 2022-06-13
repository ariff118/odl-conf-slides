## @knitr Code

# set working directory
# setwd("E:/UiTM Project/Dr Azahari/")

# load packages
library(readxl)
library(data.table)
library(ggplot2)
library(likert)
library(rmarkdown)
library(tidyverse) # for all things data wrangling
library(kableExtra) # make nice looking output tables
library(stargazer)
library(gt)

# read data file
# df_north <- read.csv("df_north.csv")
df_north <- read_excel("df_north.xlsx")
# head(df_north)
dt <- df_north[,c(2,4,6:16)]
dt$campus <- dplyr::recode(dt$campus, "3"="Perlis", "5"="Kedah")
dt[dt == 999] <- NA
dt <- tidyr::drop_na(dt)
dt$semester <- as.factor(dt$semester)
# str(dt)
# head(dt)
summary_report <- summary(dt)
# summary_report

attach(dt)

# create frequency table
# library(janitor)
# c_freq <- tabyl(campus, sort = TRUE)
# c_freq

library(epiDisplay)
# campus_freq <- tab1(campus, sort.group = "increasing", cum.percent = TRUE)
# semester_freq <- tab1(semester, sort.group = F, cum.percent = TRUE)
# gender_freq <- tab1(gender, sort.group = "increasing", cum.percent = TRUE)
# 
# campus_freq
# semester_freq
# gender_freq

# gt_freq <- table1::table1(~ gender+semester | campus, data=dt)
# gt_freq
# 
# gt_freq_main <- table1::table1(~ enough_info_delivery+
#                                  enough_info_assessment+
#                                  enough_time+
#                                  online_access+
#                                  resource_access+
#                                  scheduled_learning+
#                                  learning_duration+
#                                  collaborative_study+
#                                  constant_feedback+
#                                  comfortably_study,
#                                data=dt)
# gt_freq_main
# 
# gt_freq_all <- table1::table1(~ campus+
#                                 semester+
#                                 gender+
#                                 enough_info_delivery+
#                                 enough_info_assessment+
#                                 enough_time+
#                                 online_access+
#                                 resource_access+
#                                 scheduled_learning+
#                                 learning_duration+
#                                 collaborative_study+
#                                 constant_feedback+
#                                 comfortably_study,
#                               data=dt)
# gt_freq_all
# 
# gt_freq_all_by_campus <- table1::table1(~ semester+
#                                 gender+
#                                 enough_info_delivery+
#                                 enough_info_assessment+
#                                 enough_time+
#                                 online_access+
#                                 resource_access+
#                                 scheduled_learning+
#                                 learning_duration+
#                                 collaborative_study+
#                                 constant_feedback+
#                                 comfortably_study |
#                                   campus,
#                               data=dt, 
#                               rowname_col = "rowname",
#                               groupname_col = dplyr::group_vars(dt),
#                               total = TRUE,
#                               caption = "Descriptive Summary by Campus",
#                               title = "DESCRIPTIVE SUMMARY",
#                               rownames_to_stub = TRUE)
# gt_freq_all_by_campus

##################
##################
# 1. Descriptive Summary by Campus (Demography)
gt_freq_demo_by_campus <- table1::table1(~ gender+semester | campus,
                                        data=dt, 
                                        rowname_col = "rowname",
                                        groupname_col = dplyr::group_vars(dt),
                                        total = TRUE,
                                        caption = "Campus Branch",
                                        title = "DESCRIPTIVE SUMMARY",
                                        rownames_to_stub = TRUE)
# gt_freq_demo_by_campus


# 2. Descriptive Summary by Campus (ODL)
gt_freq_odl_by_campus_1 <- table1::table1(~ enough_info_delivery+
                                          enough_info_assessment+
                                          enough_time+
                                          online_access+
                                          resource_access |
                                          campus,
                                        data=dt, 
                                        rowname_col = "rowname",
                                        groupname_col = dplyr::group_vars(dt),
                                        total = TRUE,
                                        caption = "Branch Campus",
                                        title = "DESCRIPTIVE SUMMARY",
                                        rownames_to_stub = TRUE)

gt_freq_odl_by_campus_2 <- table1::table1(~ scheduled_learning+
                                          learning_duration+
                                          collaborative_study+
                                          constant_feedback+
                                          comfortably_study |
                                          campus,
                                        data=dt, 
                                        rowname_col = "rowname",
                                        groupname_col = dplyr::group_vars(dt),
                                        total = TRUE,
                                        caption = "Branch Campus",
                                        title = "DESCRIPTIVE SUMMARY",
                                        rownames_to_stub = TRUE)

# gt_freq_odl_by_campus

# Test for equal variances (Levene's Test)
v1 <- var.test(enough_info_delivery ~ campus)
v2 <- var.test(enough_info_assessment ~ campus)
v3 <- var.test(enough_time ~ campus)
v4 <- var.test(online_access ~ campus)
v5 <- var.test(resource_access ~ campus)
v6 <- var.test(scheduled_learning ~ campus)
v7 <- var.test(learning_duration ~ campus)
v8 <- var.test(collaborative_study ~ campus)
v9 <- var.test(constant_feedback ~ campus)
v10 <- var.test(comfortably_study ~ campus)

levene_eq_var <- rbind(v1$p.value, 
                       v2$p.value, 
                       v3$p.value, 
                       v4$p.value, 
                       v5$p.value, 
                       v6$p.value, 
                       v7$p.value, 
                       v8$p.value, 
                       v9$p.value, 
                       v10$p.value)

# levene_eq_var

t1 <- t.test(enough_info_delivery ~ campus, data = dt, var.equal = T)
t2 <- t.test(enough_info_assessment ~ campus, data = dt, var.equal = T)
t3 <- t.test(enough_time ~ campus, data = dt, var.equal = T)
t4 <- t.test(online_access ~ campus, data = dt, var.equal = T)
t5 <- t.test(resource_access ~ campus, data = dt, var.equal = T)
t6 <- t.test(scheduled_learning ~ campus, data = dt, var.equal = T)
t7 <- t.test(learning_duration ~ campus, data = dt, var.equal = T)
t8 <- t.test(collaborative_study ~ campus, data = dt)
t9 <- t.test(constant_feedback ~ campus, data = dt, var.equal = T)
t10 <- t.test(comfortably_study ~ campus, data = dt, var.equal = T)

# Obtain estimates, p-value, and 95% CI
t_test_summary <- rbind(c(t1$estimate,t1$p.value, t1$conf.int),
                        c(t2$estimate,t2$p.value, t2$conf.int),
                        c(t3$estimate,t3$p.value, t3$conf.int),
                        c(t4$estimate,t4$p.value, t4$conf.int),
                        c(t5$estimate,t5$p.value, t5$conf.int),
                        c(t6$estimate,t6$p.value, t6$conf.int),
                        c(t7$estimate,t7$p.value, t7$conf.int),
                        c(t8$estimate,t8$p.value, t8$conf.int),
                        c(t9$estimate,t9$p.value, t9$conf.int),
                        c(t10$estimate,t10$p.value, t10$conf.int))
t_test_summary <- cbind(levene_eq_var,t_test_summary)
t_test_summary <- as.data.frame(t_test_summary)
names(t_test_summary) <- c('p-value LT', 'Mean Kedah', 'Mean Perlis', 'p-value', '95%LCI', '95%UCI')
# t_test_summary
final_t_test <- t_test_summary %>%
  mutate(t_test_summary, Mean_Diff = `Mean Kedah`- `Mean Perlis`)

final_t_test <- final_t_test %>%
  mutate(final_t_test, Decision = c("Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null",
                        "Fail to reject null"))
final_t_test <- final_t_test %>%
  relocate(Mean_Diff, .after = `Mean Perlis`)
final_t_test <- final_t_test[,c(1:5,8)]
# final_t_test
# stargazer(final_t_test)

# create likert graphs
dt_main <- dt[,4:13]
# head(dt_main)
dt_main <- as.data.frame(dt_main)
# head(dt_main)
choices = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
for(i in 1:ncol(dt_main)) {
  dt_main[,i] = factor(dt_main[,i], levels=1:5, labels=choices, ordered=TRUE)
}
# head(dt_main)

plot<- plot(likert(dt_main))
# plot
title1 <- "Students ODL Experience"
final_plot <- plot + ggtitle(title1)
# final_plot

# # create likert graph by campus
# dt_full_likert <- dt
# choices = c("Strongly Disgree", "Disagree", "Neutral", "Agree", "Strongly Agree")
# for(i in 1:ncol(dt_full_likert[,4:13])) {
#   dt_full_likert[,i] = factor(dt_full_likert[,i], levels=1:5, labels=choices, ordered=TRUE)
# }
# head(dt_full_likert)
# 
# # Descriptive Summary ODL
# library(psych)
# describe.by(enough_info_delivery, campus)
# 
# library(doBy)
