# Load Libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(gtools)

# Read Data
df <- read.csv("Daten/Questionnaires/Ablauf_log_ausgefüllt_statistik.csv", header=TRUE, sep=";")

head(df)
str(df)

print("Durchschnitt Alter")
print(mean(df$Age))
print("Standardabweichung Alter")
print(sd(df$Age))

# create questionaire subscales   
df$MS_Scale <- (df$MS_01 + df$MS_02 + df$MS_03 + df$MS_04)/4

df$EQ_Ownership <- ((df$EQ_01 - df$EQ_02) - df$EQ_03) / 3
df$EQ_Agency <- (df$EQ_04 + df$EQ_05 + df$EQ_06 - df$EQ_07) / 4
df$EQ_Location <- (df$EQ_08 - df$EQ_09 + df$EQ_10) /3

df$EQ_Scale <- (df$EQ_Ownership + df$EQ_Agency + df$EQ_Location) / 3
df$IPQ_Scale <- (df$IPQ_01 + df$IPQ_02 + df$IPQ_03 + df$IPQ_04 + df$IPQ_05 + df$IPQ_06 + df$IPQ_07 +  df$IPQ_08 +df$IPQ_09 + df$IPQ_10 + df$IPQ_11 + df$IPQ_12 + df$IPQ_13 + df$IPQ_14) / 14



# make condition a factor
head(df)
df$Model <- as.factor(df$Model)

# check validity 
#for 
  mean_EQ_01_02 <- mean((df$EQ_01 +df$EQ_02) / 2)
  mean_EQ_04_05 <- mean((df$EQ_04 +df$EQ_05) / 2)
  print("Durschnitt aus 01 und 02")
  print(mean_EQ_01_02)
  print("Durschnitt aus 04 und 05")
  print(mean_EQ_04_05)

# chart
# Custom Functions
descStats <- function(x) c(mean = mean(x), 
                           sd = sd(x), se = sd(x)/sqrt(length(x)),
                           ci = qt(0.95,df=length(x)-1)*sd(x)/sqrt(length(x)))


# melt
data_long <- melt(df, id.vars=c("Proband_ID", "Model"),
                  measure.vars=c("IPQ_Scale", "EQ_Agency" , "EQ_Ownership", "EQ_Location", "EQ_Scale" ,"MS_Scale"),
                  variable.name="Questionnaire",
                  value.name="Value")

head(data_long)
nrow(data_long)

data_long$Questionnaire_type <- ifelse(data_long$Questionnaire = "EQ_Agency","EQ", "")
data_long$Questionnaire_type <- ifelse(data_long$Questionnaire = "EQ_Location","EQ", "")
data_long$Questionnaire_type <- ifelse(data_long$Questionnaire = "EQ_Ownership","EQ", "")
data_long$Questionnaire_type <- ifelse(data_long$Questionnaire = "EQ_Scale","EQ", "")
data_long$Questionnaire_type <- ifelse(data_long$Questionnaire = "IPQ_Scale","IPQ", "")
data_long$Questionnaire_type <- ifelse(data_long$Questionnaire = "MS_Scale","EQ", "")


# calc means and stats
means <- aggregate(data_long$Value, by=list(data_long$Model, data_long$Questionnaire), FUN=descStats) 
means <- do.call(data.frame, means)
head(means)
colnames(means) <- c("Model","Questionnaire","Mean","SD","SE","CI")

# balebs
data_long$Questionnaire <- factor(data_long$Questionnaire, levels = c("IPQ_Scale","EQ_Scale", "EQ_Location", "EQ_Agency","EQ_Ownership", "MS_Scale"),
                  labels = c("Presence", "Avatar Embodiment", "AE Location of the body",  "AE Agency and motor controll",  "AE Body ownership", "Motion Sickness"))

# make plot
ggplot(data_long,aes(x=Model, y=Value, fill=Model)) + 
  geom_boxplot(outlier.shape=16,outlier.size=2, position=position_dodge2(width=0.9, preserve="single"), width=0.9) +
  facet_wrap(.~Questionnaire) +
  ylab(label = "") + 
  xlab(label = "") + 
  scale_fill_discrete(name = "Prediction Model", labels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms")) +
  stat_summary(fun.y=mean,  geom="point", shape=4, size=5, color="black", position=position_dodge2(width=0.9, preserve="single")) +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom", text = element_text(size=20))+
  ggsave("boxplotchart_allQuestionnaires.pdf", width=8, height=6, device=cairo_pdf)

if(FALSE){
# make plot
ggplot(means,aes(x=Model, y=Mean, fill=Questionnaire)) + 
  geom_bar(stat="identity", position=position_dodge2(width=0.9, preserve="single"), width=0.9) +
  geom_errorbar(aes(ymin=Mean-CI, ymax=Mean+CI, width=0.1), 
                position=position_dodge(0.9), width=0.5) +
  scale_fill_manual(values=c("#d83c49","#68bdda","#b3b3b3","#123456"), name="Questionnaire") +
  ylab(label = "") + 
  xlab(label = "") +
  scale_x_discrete("Prediction Model", position = "bottom", labels =c("-48ms", "Base", "+48ms", "+96ms", "+144ms", "+192ms"))+
  theme_light() +
  theme(legend.position="bottom", text = element_text(size=20)) +
  ggsave("barchart_allQuestionnaires.pdf", width=5, height=2, device=cairo_pdf)
}  
