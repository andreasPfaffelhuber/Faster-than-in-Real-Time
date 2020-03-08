# Load Libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(gtools)

# adapted from https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat_circ <- circleFun(diameter = 1.4)

x <- c(0 , 0)
y <- c(-1, 0)

dat_arrow <- data.frame(x, y)


# Read File
df <- read.csv2("Daten/FittsLaw_Unity/Proband_1_Unity_FittsLawTask.csv", header=TRUE, sep=";")

# read all files
if (TRUE) {
filenames <- c() 
for(i in 2:24) {
  filenames <- c(filenames, paste("Daten/FittsLaw_Unity/Proband_",i, "_Unity_FittsLawTask.csv", sep=""))
}

for(i in 1:23) {
  print(filenames[i])
  df_csv <- read.csv2(filenames[i],header = TRUE, sep = ";")
  df <- rbind(df, df_csv)
}
}

# Filter only for hits
df <- filter(df, Notice=="goalCirclePressed")

print("Nach dem Filtern")
str()

# Convert Calculate ID
df$BigCircleRadius <- as.numeric(df$BigCircleRadius)
df$SmallCircleRadius <- as.numeric(df$SmallCircleRadius)
df$ID <- log((df$BigCircleRadius/df$SmallCircleRadius) + 1)

# Convert Back
df$BigCircleRadius <- as.factor(df$BigCircleRadius)
df$SmallCircleRadius <- as.factor(df$SmallCircleRadius)#

df$PredictionModel <- as.factor(df$PredictionModel)

df$BigCircleRadius <- factor(df$BigCircleRadius, levels = c("1.5", "2.5", "3"), 
                  labels = c("Narrow", "Medium", "Wide"))

df$SmallCircleRadius <- factor(df$SmallCircleRadius, levels = c("0.15", "0.3", "0.7"),
                  labels = c("Small target", "Medium target", "Big target"))

df$PredictionModel <- factor(df$PredictionModel, levels = c("-12", "0", "12", "24", "36", "48"),
                               labels = c("-48 ms", "Base", "+48 ms", "+96 ms", "+144 ms", "+192 ms"))

df_big <- df %>% filter(SmallCircleRadius == "Big target")

# draw plot
ggplot(df_big, aes(x = NormalizedSelectionPointX,y = NormalizedSelectionPointZ, color=PredictionModel)) +
  coord_fixed(ratio = 1, ylim = c(-1,1),xlim = c(-1,1)) +
  geom_point() +
  geom_line(size=0.6) +
  scale_x_continuous(breaks = c(0)) +
  scale_y_continuous(breaks = c(0)) +
  facet_grid(BigCircleRadius ~ PredictionModel, switch = "y", labeller = )+
  geom_path(data = dat_circ, aes(x,y), color="#FF0000") +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour = 'black', size = 0.3)) +

  ggsave("Precisionplot_all.pdf", width=11, height=7, device=cairo_pdf)

library(dplyr)
# make average
dfgrouped <- df_big %>%group_by(ProbandenID, PredictionModel, BigCircleRadius) %>% summarise(mean_x = mean(NormalizedSelectionPointX), mean_z = mean(NormalizedSelectionPointZ))
dfgrouped <- as.data.frame(dfgrouped)


# draw plot
ggplot(dfgrouped, aes(x = mean_x,y = mean_z, color=PredictionModel)) +
  coord_fixed(ratio = 1, ylim = c(-1,1),xlim = c(-1,1)) +
  #coord_polar(theta = "y", start=0, direction = -1)
  geom_point() +
  scale_x_continuous(breaks = c(0)) +
  scale_y_continuous(breaks = c(0)) +
  facet_grid(BigCircleRadius ~ PredictionModel, switch = "y")+
  geom_path(data = dat_circ, aes(x,y), color="#FF0000") +
  geom_path(data = dat_arrow, aes(x,y, alpha=.4), size=.6, lineend="butt", arrow=arrow(length=unit(.15, "npc")), color="#FF2222") +
  guides(alpha=FALSE) +
  ggtitle("Precision on big targets on average per participant") +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour = 'black', size = 0.3)) +
  ggsave("Precisionplot_average.pdf", width=11, height=7, device=cairo_pdf)


## Plot nach Dozent
df$DistanceFromCenter <- sqrt((df$SelectPointX - df$EndPointX)^2 + (df$SelectpointZ - df$EndPointZ)^2)

descStats <- function(x) c(mean = mean(x), 
                           sd = sd(x), se = sd(x)/sqrt(length(x)),
                           ci = qt(0.95,df=length(x)-1)*sd(x)/sqrt(length(x)))

df_melt <- melt(df,
                   id.vars=c("PredictionModel", "BigCircleRadius", "SmallCircleRadius"),
                   measure.vars=c("DistanceFromCenter"),
                   variable.name="Variable",
                   value.name="Value")

# calc means and stats
df_melt <- aggregate(df_melt$Value, by=list(df_melt$PredictionModel, df_melt$BigCircleRadius, df_melt$SmallCircleRadius), FUN=descStats) 
df_melt <- do.call(data.frame, df_melt)
colnames(df_melt) <- c("PredictionModel","BigCircleRadius","SmallCircleRadius" ,"Mean","SD","SE","CI")


ggplot(df_melt) +
  facet_grid(BigCircleRadius ~ SmallCircleRadius, switch = "y")+
  geom_bar(aes(x= PredictionModel, y=Mean, color = PredictionModel, fill=PredictionModel), stat="identity") +
  geom_errorbar(aes(x= PredictionModel, ymin=Mean-SD, ymax=Mean+SD)) +
  ylab("Mean deviation from center") +
  xlab("") +
  theme_light() +
  theme(legend.position="bottom") +
  ggsave("Precisionplot_Barchart.pdf", width=11, height=7, device=cairo_pdf)



