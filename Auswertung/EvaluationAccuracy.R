# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)

options("scipen"=100, "digits"=10)

# Prepare Data Frame
summary <- data.frame(PredictionModel=integer(),TotalTime=integer(), Proband=integer(),stringsAsFactors=FALSE)

# Read original CSV Data
filenames <- c() 
for(i in 1:24) {
  filenames <- c(filenames, paste("Daten/FittsLaw_Unity/Proband_",i, "_Unity_FittsLawTask.csv", sep=""))
}

# Prepare Data for Evaluation, Compute signifikant Values (ID, IDe, SDx, Ae, Throughput)
for(i in 1:length(filenames)) {
  print(filenames[i])
  df <- read.table(filenames[i],header = TRUE,sep = ";")
  head(df)
  # Remove unimportant columns
  df$CircleCounter <- NULL
  df$StartPointX <- NULL
  df$StartPointY <- NULL
  df$StartPointZ <- NULL
  df$EndPointX <- NULL
  df$EndPointY <- NULL
  df$EndPointZ <- NULL
  df$SelectPointX <- NULL
  df$SelectPointY <- NULL
  df$SelectpointZ <- NULL
  df$NormalizedSelectionPointX
  df$NormalizedSelectionPointY <- NULL
  df$NormalizedSelectionPointZ
  df$DX <- NULL
  df$AE <- NULL
  df$TrialCounter <- NULL
  df$Trial <- NULL
  df$DisplayedCircleNr <- NULL
  df$TrialTime <- NULL
  df$TrialErrors <- NULL
  df$TrialDx <- NULL
  df$TrialAE <- NULL
  
  
  library(dplyr) 
  
  # Filter on only the important entry after each trial was ended
  # Read rows in correct type
  df[, 1] <- as.numeric( df[, 1] )
  df[, 5] <- gsub(",", ".",df[, 5])
  df[, 5] <- as.numeric(( df[, 5] ))
  df[, 6] <- gsub(",", ".",df[, 6])
  df[, 6] <- as.numeric(( df[, 6] ))
  
  
  # Trial Time Computation
  #totalTrialTime <- aggregate(df$TrialTime, by=list(df$PredictionModel), FUN=sum)
  #totalTrialTime["Proband"] <- c(df[1,2], df[1,2],df[1,2],df[1,2],df[1,2],df[1,2])
  #colnames(totalTrialTime) <- c("PredictionModel", "TotalTime","Proband")
  summary <- rbind(summary, df)
  
}

# Create Dataframe for Throughput Evaluation
accuracy_distance <- data.frame(matrix(ncol = 3, nrow = 0))

summary <- filter(summary, Notice == "goalCirclePressed")

# Fill Dataframe for Throughput Evaluation with average Throughput for each Participant per Conditon
number_of_participants <- 24
prediction_models <- c(-12, 0, 12, 24, 36, 48)
for (participant in 1:number_of_participants) {
  for (predicton_model in prediction_models) {
    condition_subset <- summary[summary$PredictionModel == predicton_model, ]
    participant_and_condition_subset <- condition_subset[condition_subset$ProbandenID == participant, ]
    average_distance <- mean(sqrt(
      (participant_and_condition_subset$NormalizedSelectionPointX*participant_and_condition_subset$NormalizedSelectionPointX)
      +(participant_and_condition_subset$NormalizedSelectionPointZ*participant_and_condition_subset$NormalizedSelectionPointZ)
    ))
    
    accuracy_distance <- rbind(accuracy_distance, c(participant, predicton_model,average_distance ))
  }
}
accuracy_distance_names <- c("ProbandenID", "PredictionModel", "AccuracyDistance")
colnames(accuracy_distance) <- accuracy_distance_names

# Change grouping columns into factors for Anova with repeated measures
accuracy_distance$ProbandenID <- factor(accuracy_distance$ProbandenID)
accuracy_distance$PredictionModel <- factor(accuracy_distance$PredictionModel)

# Get Simple Summarizing Statistics
accuracy_distance %>%
  group_by(PredictionModel) %>%
  get_summary_stats(AccuracyDistance, type = "mean_sd")

# Get Simple Boxplot
#bxp <- ggboxplot(accuracy_distance, x = "PredictionModel", y = "AccuracyDistance", add = "point")
#bxp

# make plot
ggplot(accuracy_distance,aes(x=PredictionModel, y=AccuracyDistance, fill=PredictionModel)) + 
  geom_boxplot(outlier.shape=16,outlier.size=2, position=position_dodge2(width=0.9, preserve="single"), width=0.9) +
  ylab(label = "Distance from Center") + 
  scale_x_discrete("Prediction Model", position = "bottom", labels = c("-48ms", "Base", "+48ms", "+96ms", "+144ms", "+192ms"))+
  scale_fill_discrete(name = "Prediction Model", labels = c("-48ms", "Base", "+48ms", "+96ms", "+144ms", "+192ms")) +
  stat_summary(fun.y=mean,  geom="point", shape=4, size=5, color="red") +
  theme_light() +
  theme(legend.position = "none") +
  ggsave("boxplotchart_Accuracy.pdf", width=5, height=3, device=cairo_pdf)

# Check Assumptions for repeated measures Anova
accuracy_distance %>%
  group_by(PredictionModel) %>%
  identify_outliers(AccuracyDistance)
# No extreme outliers => Outlier Assumption is met

# Check Normality Assumption
accuracy_distance %>%
  group_by(PredictionModel) %>%
  shapiro_test(AccuracyDistance)
# No condition with p < 0.05 => Normality Assumption is met

ggqqplot(accuracy_distance, "AccuracyDistance", facet.by = "PredictionModel")

# This would be the repeated measures anova code, but is not used here since the Prerequisits are not met 
# (Assumption of Normality is not given for total throughput)

#res.aov <- anova_test(data = total_throughputs, dv = Throughput, wid = ProbandenID, within = PredictionModel)
#get_anova_table(res.aov)

# Would compute group comparisons using pairwise t tests with  Bonferroni multiple testing correction method if Anova is significant

#pwc <- total_throughputs %>% wilcox_test(Throughput ~ PredictionModel, paired = TRUE, p.adjust.method = "bonferroni")
#pwc

# Since the prerequisits for a repeated measures anova are not met, we use a non-parametric alternative, the Friedmann-Test
res.fried <- accuracy_distance %>% friedman_test(AccuracyDistance ~ PredictionModel |ProbandenID)
res.fried

# p > 0.05 => There are significant differences between the groups

# Compute effect size
res.fried.effect <- accuracy_distance %>% friedman_effsize(AccuracyDistance ~ PredictionModel |ProbandenID)
res.fried.effect

# Compute group comparisons using pairwise Wilcoxon signed-rank tests with  Bonferroni multiple testing correction method
pwc <- accuracy_distance %>% wilcox_test(AccuracyDistance ~ PredictionModel, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "PredictionModel")
ggboxplot(accuracy_distance, x = "PredictionModel", y = "AccuracyDistance", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )