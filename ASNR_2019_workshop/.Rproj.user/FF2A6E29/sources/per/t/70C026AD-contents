library(ggplot2); library(lme4); library(tidyverse)
# By treating this workshop as an R project, we can use relative file paths that
# allow you to open the data anywhere on any computer, provided you have downloaded 
# the whole workshop folder.
getwd()

# Anscombe's Quartet and the Importance of Checking Assumptions
DAT1 <- read.csv("./data_ANSCOMBE.csv", header = TRUE, sep = ",")
head(DAT1)


## Regression Coefficients ---- 
COEFS<-DAT1 %>%
  group_by(group) %>%
  summarise(Intercept=lm(yVal~xVal, data=DAT1)$coefficients[1],
            Slope=lm(yVal~xVal, data=DAT1)$coefficients[2])
COEFS


# Visualizing All the Data
ggplot(DAT1, aes(x = xVal, y = yVal)) +
  geom_point(aes(fill=as.factor(group)), pch=21, color="black", size=2)+
  stat_smooth(aes(col=as.factor(group)), method="lm", se=FALSE, lwd=1)+
  facet_wrap(~group, ncol=2)+
  scale_x_continuous(name = "X Values") +
  scale_y_continuous(name = "Y Values") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        #axis.text.y=element_blank(),
        #axis.title.y=element_blank(),
        #axis.ticks.y=element_blank(),
        legend.position = "none")




## Categorical Data
DAT2 <- read.csv("./data_FINAL_RATINGS.csv", header = TRUE, sep = ",")
head(DAT2)


MEANS<-DAT2 %>%
  group_by(Elevation, Speed) %>%
  summarise(ave_Effort=mean(Effort),
            N = length(Effort),
            SD = sd(Effort))

MEANS

# Just the means
ggplot(MEANS, aes(x = Elevation, y = ave_Effort)) +
  geom_bar(aes(fill=Elevation), stat="identity", width = 0.5)+
  facet_wrap(~Speed) +
  scale_y_continuous(name = "Effort (%)", limits = c(0,100)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")


# Means with Standard errors
ggplot(MEANS, aes(x = Elevation, y = ave_Effort)) +
  geom_bar(aes(fill=Elevation), stat="identity", width = 0.5)+
  geom_errorbar(aes(ymin = ave_Effort-SD/sqrt(N), ymax=ave_Effort+SD/sqrt(N)),
                width = 0.2)+
  facet_wrap(~Speed) +
  scale_y_continuous(name = "Effort (%)", limits = c(0,100)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")


# All the data
ggplot(DAT2, aes(x = Elevation, y = Effort)) +
  geom_point(aes(fill=Elevation), pch=21, size=2,
               position=position_jitter(w=0.2, h=0))+
  facet_wrap(~Speed) +
  scale_y_continuous(name = "Effort (%)", limits = c(0,100)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")


# All the data
ggplot(DAT2, aes(x = Elevation, y = Effort)) +
  geom_point(aes(fill=Elevation), pch=21, size=2,
             position=position_jitter(w=0.2, h=0))+
  geom_boxplot(fill="white", col="black", outlier.shape = "na",
               alpha=0.4, width=0.5)+
  facet_wrap(~Speed) +
  scale_y_continuous(name = "Effort (%)", limits = c(0,100)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")

# Connect the dots
head(DAT2)
ggplot(DAT2, aes(x = Elevation, y = Effort)) +
  geom_point(aes(fill=Elevation), pch=21, size=2)+
  #geom_boxplot(fill="white", col="black", outlier.shape = "na",
  #             alpha=0.4, width=0.5)+
  geom_line(aes(group=SUBJ))+
  facet_wrap(~Speed) +
  scale_y_continuous(name = "Effort (%)", limits = c(0,100)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none") +
  stat_smooth(aes(group=Speed, lty=Speed), col="blue", lwd=2, se=FALSE)



