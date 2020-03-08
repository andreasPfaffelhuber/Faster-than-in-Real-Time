# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)

# Load Libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(gtools)

options("scipen"=100, "digits"=15)

# Read Data
df <- read.csv("Daten/Questionnaires/Ablauf_log_ausgefüllt_statistik.csv", header=TRUE, sep=";")

# create questionaire subscales 
df$EQ_Ownership <- ((df$EQ_01 - df$EQ_02) - df$EQ_03) / 3
df$EQ_Agency <- (df$EQ_04 + df$EQ_05 + df$EQ_06 - df$EQ_07) / 4
df$EQ_Location <- (df$EQ_08 - df$EQ_09 + df$EQ_10) /3

df$EQ_Scale <- (df$EQ_Ownership + df$EQ_Agency + df$EQ_Location) / 3
df$IPQ_Scale <- (df$IPQ_01 + df$IPQ_02 + df$IPQ_03 + df$IPQ_04 + df$IPQ_05 + df$IPQ_06 + df$IPQ_07 +  df$IPQ_08 +df$IPQ_09 + df$IPQ_10 + df$IPQ_11 + df$IPQ_12 + df$IPQ_13 + df$IPQ_14) / 14

df$MS_Scale <- (df$MS_01 + df$MS_02 + df$MS_03 + df$MS_04)/4


# melt
questionnaires <- melt(df, id.vars=c("Proband_ID", "Model"),
                  measure.vars=c("IPQ_Scale", "EQ_Scale", "MS_Scale", "EQ_Ownership", "EQ_Agency", "EQ_Location"),
                  variable.name="Questionnaire",
                  value.name="Value")

# Filter for the questionnaire we are currently examining
#questionnaires <- filter(questionnaires, Questionnaire == "IPQ_Scale")
#questionnaires <- filter(questionnaires, Questionnaire == "EQ_Scale")
#questionnaires <- filter(questionnaires, Questionnaire == "MS_Scale")
#questionnaires <- filter(questionnaires, Questionnaire == "EQ_Ownership")
#questionnaires <- filter(questionnaires, Questionnaire == "EQ_Agency")
questionnaires <- filter(questionnaires, Questionnaire == "EQ_Location")

# Change grouping columns into factors for Anova with repeated measures
questionnaires$Proband_ID <- factor(questionnaires$Proband_ID)
questionnaires$Model <- factor(questionnaires$Model)

# Get Simple Summarizing Statistics
questionnaires %>%
  group_by(Model) %>%
  get_summary_stats(Value, type = "mean_sd")

# Get Simple Boxplot
bxp <- ggboxplot(questionnaires, x = "Model", y = "Value", add = "point")
bxp

# As suggested by the questionnaires creator, we use non-parametric tests (the Friedmann-Test) to evaluate this non-continouus data
res.fried <- questionnaires %>% friedman_test(Value ~ Model |Proband_ID)
res.fried

# p > 0.05 => There are significant differences between the groups

# Compute effect size
res.fried.effect <- questionnaires %>% friedman_effsize(Value ~ Model |Proband_ID)
res.fried.effect

# Compute group comparisons using pairwise Wilcoxon signed-rank tests with  Bonferroni multiple testing correction method
pwc <- questionnaires %>% wilcox_test(Value ~ Model, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Model")
ggboxplot(questionnaires, x = "Model", y = "Value", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )