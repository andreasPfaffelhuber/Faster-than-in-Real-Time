# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggpmisc)

options("scipen"=100, "digits"=5)

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
  df$NormalizedSelectionPointX <- NULL
  df$NormalizedSelectionPointY <- NULL
  df$NormalizedSelectionPointZ <- NULL
  df$DX <- NULL
  df$AE <- NULL

  
  library(dplyr) 
  
  # Filter on only the important entry after each trial was ended
  df <- filter(df, Notice == "endedTrial")
  
  # Read rows in correct type
  df[, 1] <- as.numeric( df[, 1] )
  df[, 9] <- gsub(",", ".",df[, 9])
  df[, 10] <- gsub(",", ".",df[, 10])
  df[, 9] <- as.numeric(( df[, 9] ))
  df[, 10] <- as.numeric(( df[, 10] ))
  df[, 8] <- as.numeric(as.character( df[, 8] ))
  df[, 7] <- as.numeric(as.character( df[, 7] ))
  df[, 2] <- as.numeric(as.character( df[, 2] ))
  df[, 13] <- as.character( df[, 13] )
  df[, 13] <- gsub(",", ".",df[, 13])
  df[, 14] <- as.character( df[, 14] )
  df[, 14] <- gsub(",", ".",df[, 14])
  
  # Compute signifikant values for evalution
  for(i in 1:nrow(df)) {
    dx <- ((strsplit(df[i,13], "_")))
    df[i,15] <- sd(as.numeric(unlist(dx)))
    ae <- ((strsplit(df[i,14], "_")))
    df[i,16] <- mean(as.numeric(unlist(ae)))
    
    id_shannon <- log2(df[i, 9]/(df[i, 10])+1)
    id_shannon_e <- log2((df[i, 16]/(df[i, 15] * 4.133))+1)
    
    mean_movement_time <- (df[i, 11]/16.0)/1000
    throughput <- id_shannon_e/mean_movement_time
    df[i,17] <- id_shannon
    df[i,18] <- id_shannon_e
    df[i,19] <- mean_movement_time
    df[i,20] <- throughput
  }

  # Trial Time Computation

  #totalTrialTime["Proband"] <- c(df[1,2], df[1,2],df[1,2],df[1,2],df[1,2],df[1,2])
  #colnames(totalTrialTime) <- c("PredictionModel", "TotalTime","Proband")
  
  # Append values
  colnames(df)[15] <- "SDx"
  colnames(df)[16] <- "Mean AE"
  colnames(df)[17] <- "ID_Shannon"
  colnames(df)[18] <- "IDE"
  colnames(df)[19] <- "MeanMovementTime"
  colnames(df)[20] <- "Throughput"
  df[, 15] <- as.numeric( df[ ,15] )
  df[, 16] <- as.numeric( df[ ,16] )
  df[, 17] <- as.numeric( df[ ,17] )
  df[, 18] <- as.numeric( df[ ,18] )
  df[, 19] <- as.numeric( df[, 19] )
  df[, 20] <- as.numeric( df[, 20] )
  summary <- rbind(summary, df)

}

# Create Dataframe for Throughput Evaluation
total_throughputs <- data.frame(matrix(ncol = 3, nrow = 0))


# Fill Dataframe for Throughput Evaluation with average Throughput for each Participant per Conditon
number_of_participants <- 24
prediction_models <- c(-12, 0, 12, 24, 36, 48)
for (participant in 1:number_of_participants) {
  for (predicton_model in prediction_models) {
    condition_subset <- summary[summary$PredictionModel == predicton_model, ]
    participant_and_condition_subset <- condition_subset[condition_subset$ProbandenID == participant, ]
    mean_throughput_for_proband_and_condition <- mean(participant_and_condition_subset$Throughput)
    
    total_throughputs <- rbind(total_throughputs, c(participant, predicton_model,mean_throughput_for_proband_and_condition ))
  }
}
total_throughputs_names <- c("ProbandenID", "PredictionModel", "Throughput")
colnames(total_throughputs) <- total_throughputs_names

# Change grouping columns into factors for Anova with repeated measures
total_throughputs$ProbandenID <- factor(total_throughputs$ProbandenID)
total_throughputs$PredictionModel <- factor(total_throughputs$PredictionModel)

# Get Simple Summarizing Statistics
total_throughputs %>%
  #group_by(PredictionModel) %>%
  get_summary_stats(Throughput, type = "mean_sd")

# Get Simple Boxplot
#bxp <- ggboxplot(total_throughputs, x = "PredictionModel", y = "Throughput", add = "point")
#bxp

total_throughputs$PredictionModel <- factor(total_throughputs$PredictionModel, levels = c("-12", "0", "12", "24", "36", "48"),
                                            labels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms"))


ggplot(total_throughputs,aes(x=PredictionModel, y=Throughput, fill=PredictionModel)) + 
  geom_boxplot(outlier.shape=16,outlier.size=2, position=position_dodge2(width=0.9, preserve="single"), width=0.9) +
  ylab(label = "Throughput [bit/s]") + 
  xlab(label ="") +
  scale_x_discrete(position = "bottom", labels = NULL)+
  stat_summary(fun.y=mean,  geom="point", shape=4, size=5, color="black") +
  #ggtitle("Throughput")
  theme_light() +
  #theme(legend.position = "none") +
  guides(fill=guide_legend(title="Prediction Time Offset")) +
  theme(legend.position="bottom", text = element_text(size=20)) +
  ggsave("boxplotchart_Throughput.pdf", width=10, height=6, device=cairo_pdf)

# Check Assumptions for repeated measures Anova
total_throughputs %>%
  group_by(PredictionModel) %>%
  identify_outliers(Throughput)
# No extreme outliers => Outlier Assumption is met

# Check Normality Assumption
total_throughputs %>%
  group_by(PredictionModel) %>%
  shapiro_test(Throughput)
# No condition with p < 0.05 => Normality Assumption is met

ggqqplot(total_throughputs, "Throughput", facet.by = "PredictionModel")

# This would be the repeated measures anova code, but is not used here since the Prerequisits are not met 
# (Assumption of Normality is not given for total throughput)

#res.aov <- anova_test(data = total_throughputs, dv = Throughput, wid = ProbandenID, within = PredictionModel)
#get_anova_table(res.aov)

# Would compute group comparisons using pairwise t tests with  Bonferroni multiple testing correction method if Anova is significant

#pwc <- total_throughputs %>% pairwise_t_test(Throughput ~ PredictionModel, paired = TRUE, p.adjust.method = "bonferroni")
#pwc

# Since the prerequisits for a repeated measures anova are not met, we use a non-parametric alternative, the Friedmann-Test
res.fried <- total_throughputs %>% friedman_test(Throughput ~ PredictionModel |ProbandenID)
res.fried

# p > 0.05 => There are significant differences between the groups

# Compute effect size
res.fried.effect <- total_throughputs %>% friedman_effsize(Throughput ~ PredictionModel |ProbandenID)
res.fried.effect

# Compute group comparisons using pairwise Wilcoxon signed-rank tests with  Bonferroni multiple testing correction method
pwc <- total_throughputs %>% wilcox_test(Throughput ~ PredictionModel, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "PredictionModel")
ggboxplot(total_throughputs, x = "PredictionModel", y = "Throughput", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


# Visualize Fitts Slope for Throughput
total_throughputs_per_condition_and_id <- data.frame(matrix(ncol = 3, nrow = 0))

fitts_width <- c(1.5,2.5,3.0)
fitts_target_width <- c(0.15,0.30,0.70)

prediction_models <- c(-12, 0, 12, 24, 36, 48)
for (width in fitts_width) {
  for (target_width in fitts_target_width) {
    for (predicton_model in prediction_models) {
      subset_id_and_cond <- filter(summary, BigCircleRadius == width, SmallCircleRadius == target_width, PredictionModel == predicton_model)
      id_s <- subset_id_and_cond[1,17]
      mean_throughput_for_id_and_cond <- mean(subset_id_and_cond$Throughput)
      total_throughputs_per_condition_and_id <- rbind(total_throughputs_per_condition_and_id, c(id_s, predicton_model,mean_throughput_for_id_and_cond ))
    }
  }
}
total_throughputs_per_condition_and_id_names <- c("ID_Shannon", "PredictionModel", "Throughput")
colnames(total_throughputs_per_condition_and_id) <- total_throughputs_per_condition_and_id_names

total_throughputs_per_condition_and_id$PredictionModel <- factor(total_throughputs_per_condition_and_id$PredictionModel)

my.formula <- y ~ x

ggplot(total_throughputs_per_condition_and_id, aes(x=ID_Shannon, y = Throughput, color=PredictionModel)) +
  coord_fixed(ratio = 1) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE, formula = my.formula) +
  ggtitle("Fitts' Law Model: Movement time over ID") +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               vstep = 0.03,
               show.legend = TRUE,
               size = 2.4,
               parse = TRUE) +   
  ylab(label = "Throughput [bit/s]") + 
  scale_x_continuous("ID [bit]", position = "bottom")+
  theme_light() +
  ggsave("lin_Throughput_ID.pdf", width=8, height=6, device=cairo_pdf)



## regression

descStats <- function(x) c(mean = mean(x), 
                           sd = sd(x), se = sd(x)/sqrt(length(x)),
                           ci = qt(0.95,df=length(x)-1)*sd(x)/sqrt(length(x)))

total_throughputs_2 <- total_throughputs

total_throughputs_2$PredictionModel <- factor(total_throughputs_2$PredictionModel, levels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms"),
                                            labels =c("-12", "0", "12", "24", "36", "48"))

total_throughputs_2$PredictionModel <- as.numeric(levels(total_throughputs_2$PredictionModel))[total_throughputs_2$PredictionModel]

total_throughputs_2_means <- aggregate(total_throughputs_2$Throughput, by=list(total_throughputs_2$PredictionModel, total_throughputs_2$ProbandenID), FUN=mean) 
total_throughputs_2_means <- do.call(data.frame, total_throughputs_2_means)



ggplot() + 
  geom_boxplot(data=total_throughputs,aes(x=PredictionModel, y=Throughput, fill=PredictionModel), outlier.shape=16,outlier.size=2, position=position_dodge2(width=0.9, preserve="single"), width=0.9) +
  geom_smooth(data = total_throughputs_2_means,aes(x=PredictionModel, y=Throughput), method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  ylab(label = "Throughput [bit/s]") + 
  xlab(label ="") +
  stat_summary(fun.y=mean,  geom="point", shape=4, size=5, color="black") +
  #ggtitle("Throughput")
  theme_light() +
  #theme(legend.position = "none") +
  guides(fill=guide_legend(title="Prediction Time Offset")) +
  theme(legend.position="bottom", text = element_text(size=20)) +
  ggsave("rrrr", width=10, height=6, device=cairo_pdf)




