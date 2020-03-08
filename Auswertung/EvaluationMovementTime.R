# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggpmisc)
library(reshape2)
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
    id_shannon <- log2((df[i, 9]/df[i, 10]) +1) 
    id_shannon_e <- log2((df[i, 16]/(df[i, 15] * 4.133))+1)
    mean_movement_time <- (df[i, 11]/16.0)
    throughput <- id_shannon_e/mean_movement_time
    df[i,17] <- id_shannon
    df[i,18] <- id_shannon_e
    df[i,19] <- mean_movement_time
    df[i,20] <- throughput
  }
  
  # Trial Time Computation
  #totalTrialTime <- aggregate(df$TrialTime, by=list(df$PredictionModel), FUN=sum)
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

# Create Dataframe for Movement Time Evaluation
total_movement_times <- data.frame(matrix(ncol = 3, nrow = 0))


# To do the evaluation not only for the total Movement Time over all the Fitts Circle Configurations of each participant, 
# we filter and let the following code run once for each of the 9 configurations (3 Widths * 3 Target sizes)
# and once unfiltered for the total movement times
# BigCircleRadius = width, Small CircleRadius = Target Size

#summary <- filter(summary, BigCircleRadius == 1.5, SmallCircleRadius == 0.15)
#summary <- filter(summary, BigCircleRadius == 1.5, SmallCircleRadius == 0.30)
#summary <- filter(summary, BigCircleRadius == 1.5, SmallCircleRadius == 0.70)
#summary <- filter(summary, BigCircleRadius == 2.5, SmallCircleRadius == 0.15)
#summary <- filter(summary, BigCircleRadius == 2.5, SmallCircleRadius == 0.30)
#summary <- filter(summary, BigCircleRadius == 2.5, SmallCircleRadius == 0.70)
#summary <- filter(summary, BigCircleRadius == 3.0, SmallCircleRadius == 0.15)
#summary <- filter(summary, BigCircleRadius == 3.0, SmallCircleRadius == 0.30)
#summary <- filter(summary, BigCircleRadius == 3.0, SmallCircleRadius == 0.70)

# Fill Dataframe for Movement Times Evaluation with average Movement Time for each Participant per Conditon
number_of_participants <- 24
prediction_models <- c(-12, 0, 12, 24, 36, 48)
for (participant in 1:number_of_participants) {
  for (predicton_model in prediction_models) {
    condition_subset <- summary[summary$PredictionModel == predicton_model, ]
    participant_and_condition_subset <- condition_subset[condition_subset$ProbandenID == participant, ]
    mean_movement_time_for_proband_and_condition <- mean(participant_and_condition_subset$MeanMovementTime)
    
    total_movement_times <- rbind(total_movement_times, c(participant, predicton_model,mean_movement_time_for_proband_and_condition ))
  }
}
total_movement_times_names <- c("ProbandenID", "PredictionModel", "MeanMovementTime")
colnames(total_movement_times) <- total_movement_times_names

# Change grouping columns into factors for Anova with repeated measures
total_movement_times$ProbandenID <- factor(total_movement_times$ProbandenID)
total_movement_times$PredictionModel <- factor(total_movement_times$PredictionModel)

# Get Simple Summarizing Statistics
total_movement_times %>%
  group_by(PredictionModel) %>%
  get_summary_stats(MeanMovementTime, type = "mean_sd")

# Get Simple Boxplot
#bxp <- ggboxplot(total_movement_times, x = "PredictionModel", y = "MeanMovementTime", add = "point")

total_movement_times$PredictionModel <- factor(total_movement_times$PredictionModel, levels = c("-12", "0", "12", "24", "36", "48"),
                                            labels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms"))

ggplot(total_movement_times,aes(x=PredictionModel, y=MeanMovementTime, fill=PredictionModel)) + 
  geom_boxplot(outlier.shape=16,outlier.size=2, position=position_dodge2(width=0.9, preserve="single"), width=0.9) +
  ylab(label = "Movement Time [ms]") + 
  xlab(label="") +
  #scale_x_discrete("Prediction Model", position = "bottom", labels = c("-48ms", "Base", "+48ms", "+96ms", "+144ms", "+192ms"))+
  stat_summary(fun.y=mean,  geom="point", shape=4, size=5, color="black") +
  theme_light() +
  guides(fill=guide_legend(title="Prediction Time Offset")) +
  theme(legend.position="bottom", text = element_text(size=20),
        axis.text.x=element_blank()) +
  ggsave("boxplotchart_MovementTime.pdf", width=10, height=6, device=cairo_pdf)

#bxp

# Check Assumptions for repeated measures Anova
total_movement_times %>%
  group_by(PredictionModel) %>%
  identify_outliers(MeanMovementTime)
# No extreme outliers => Outlier Assumption is met

# Check Normality Assumption
total_movement_times %>%
  group_by(PredictionModel) %>%
  shapiro_test(MeanMovementTime)
# No condition with p < 0.05 => Normality Assumption is met

#ggqqplot(total_movement_times, "MeanMovementTime", facet.by = "PredictionModel")

# This would be the repeated measures anova code, but is not used here since the Prerequisits are not met 
# (Assumption of Normality is not given for total movement time)

#res.aov <- anova_test(data = total_movement_times, dv = MeanMovementTime, wid = ProbandenID, within = PredictionModel)
#get_anova_table(res.aov)

# Would compute group comparisons using pairwise t tests with  Bonferroni multiple testing correction method if Anova is significant

#pwc <- total_movement_times %>% pairwise_t_test(MeanMovementTime ~ PredictionModel, paired = TRUE, p.adjust.method = "bonferroni")
#pwc

# Since the prerequisits for a repeated measures anova are not met, we use a non-parametric alternative, the Friedmann-Test
res.fried <- total_movement_times %>% friedman_test(MeanMovementTime ~ PredictionModel |ProbandenID)
res.fried

# p > 0.05 => There are significant differences between the groups

# Compute effect size
res.fried.effect <- total_movement_times %>% friedman_effsize(MeanMovementTime ~ PredictionModel |ProbandenID)
res.fried.effect

# Compute group comparisons using pairwise Wilcoxon signed-rank tests with  Bonferroni multiple testing correction method
pwc <- total_movement_times %>% wilcox_test(MeanMovementTime ~ PredictionModel, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "PredictionModel")
ggboxplot(total_movement_times, x = "PredictionModel", y = "MeanMovementTime", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Visualize Fitts Slope for Throughput
total_movement_times_per_condition_and_id <- data.frame(matrix(ncol = 3, nrow = 0))

total_movement_times_per_participant <- data.frame()

fitts_width <- c(1.5,2.5,3.0)
fitts_target_width <- c(0.15,0.30,0.70)

prediction_models <- c(-12, 0, 12, 24, 36, 48)
for (width in fitts_width) {
  for (target_width in fitts_target_width) {
    for (predicton_model in prediction_models) {
      subset_id_and_cond <- filter(summary, BigCircleRadius == width, SmallCircleRadius == target_width, PredictionModel == predicton_model)
      total_movement_times_per_participant <- rbind(total_movement_times_per_participant, subset_id_and_cond)
      id_s <- subset_id_and_cond[1,17]
      mean_movement_time_for_id_and_cond <- mean(subset_id_and_cond$MeanMovementTime)
      total_movement_times_per_condition_and_id <- rbind(total_movement_times_per_condition_and_id, c(id_s, predicton_model,mean_movement_time_for_id_and_cond ))
    }
  }
}

## CALC
total_movement_times_per_participant$PredictionModel <- as.factor(total_movement_times_per_participant$PredictionModel)  

descStats <- function(x) c(mean = mean(x), 
                           sd = sd(x), se = sd(x)/sqrt(length(x)),
                           ci = qt(0.95,df=length(x)-1)*sd(x)/sqrt(length(x)))

tmtpp_melt <- melt(total_movement_times_per_participant,
                                id.vars=c("PredictionModel", "ID_Shannon"),
                                measure.vars=c("MeanMovementTime"),
                                variable.name="Variable",
                                value.name="Value")

tmtpp_melt$PredictionModel <- factor(tmtpp_melt$PredictionModel, levels = c("-12", "0", "12", "24", "36", "48"),
                             labels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms"))


# calc means and stats
tmtpp_means <- aggregate(tmtpp_melt$Value, by=list(tmtpp_melt$PredictionModel, tmtpp_melt$ID_Shannon), FUN=descStats) 
tmtpp_means <- do.call(data.frame, tmtpp_means)
colnames(tmtpp_means) <- c("PredictionModel","ID_Shannon","Mean","SD","SE","CI")


total_movement_times_per_condition_and_id_names <- c("ID_Shannon", "PredictionModel", "MeanMovementTime")
colnames(total_movement_times_per_condition_and_id) <- total_movement_times_per_condition_and_id_names

total_movement_times_per_condition_and_id$PredictionModel <- factor(total_movement_times_per_condition_and_id$PredictionModel)

my.formula <- y ~ x

total_movement_times_per_condition_and_id$PredictionModel <- factor(total_movement_times_per_condition_and_id$PredictionModel, levels = c("-12", "0", "12", "24", "36", "48"),
                                            labels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms"))

total_movement_times_per_condition_and_id$PredictionModel

ggplot() +
  geom_point() +
  geom_smooth(data = total_movement_times_per_condition_and_id,
              aes(x=ID_Shannon, y=MeanMovementTime, color=PredictionModel),
              method="lm",
              se=FALSE,
              formula = my.formula) +
  stat_poly_eq(data= total_movement_times_per_condition_and_id,
               formula = my.formula,
               aes(x=ID_Shannon, y=MeanMovementTime, color=PredictionModel, label = paste(..eq.label.., ..rr.label.., ..adj.rr.label.., sep = "~~~")), 
               vstep = 0.06,
               size = 6,
               parse = TRUE) +   
  #geom_boxplot(data=total_movement_times_per_participant, aes(x=ID_Shannon, y=MeanMovementTime, fill=PredictionModel, group = interaction(ID_Shannon, PredictionModel)),outlier.shape=16,outlier.size=0.5,notch = TRUE,position=position_dodge2(width=0.3, preserve="single"),width=0.1 ) +
  geom_point(data=tmtpp_means, aes(x = ID_Shannon, y=Mean, shape=PredictionModel, color=PredictionModel, fill=PredictionModel)) +
  geom_errorbar(data=tmtpp_means, aes(x = ID_Shannon, ymin=Mean-CI, ymax=Mean+CI, width=0.1, color = PredictionModel), width=0.05) +
  ylab(label = "Movement Time [ms]") + 
  # guides(size=FALSE) +
  scale_x_continuous("ID [bit]", position = "bottom")+
  theme_light() +
  theme(legend.position="bottom", text = element_text(size=20))+
  guides(fill=guide_legend(title="Prediction Time Offset")) +
  guides(color=guide_legend(title="Prediction Time Offset")) +
  guides(shape=guide_legend(title="Prediction Time Offset")) +
ggsave("lin_Movementtime_ID.pdf", width=12, height=8, device=cairo_pdf)

