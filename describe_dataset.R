
# ACA IMPACT

library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(foreign)

# 'describe_dataset.R'

summary(d$Rate)
summary(d$vl)

ggplot(d, aes(Year, Rate, colour = state)) +
  geom_smooth(se = FALSE)

p0 <- ggplot(d, aes(Year, Rate, group = state)) +
  geom_smooth(se = FALSE) + 
  xlab("Year") +
  ylab("HIV Cases Per 100,000") +
  ggtitle("HIV Prevalence in US States") +
  theme_classic()
p0

# concept for regression discontinuity design
p1 <- ggplot(d, aes(Year, Rate)) +
  geom_smooth(data = d[d$Year <= 2011,], aes(Year, Rate), se = FALSE, method='lm') + 
  geom_smooth(data = d[d$Year >= 2011,], aes(Year, Rate), se = FALSE, method='lm') +
  ylab("HIV Cases Per 100,000") +
  ggtitle("Regression Discontinuity at ACA Implementation") +
  theme_classic()
p1

# concept for regression discontinuity design
p2 <- ggplot(d, aes(Year, vl)) +
  geom_smooth(data = d[d$Year <= 2011,], aes(Year, vl), se = FALSE, method='lm') + 
  geom_smooth(data = d[d$Year >= 2011,], aes(Year, vl), se = FALSE, method='lm') +
  ylab("VS") +
  ggtitle("Regression Discontinuity at ACA Implementation") +
  theme_classic()
p2

multiplot(p1,p2)

# this is okay evidence
p3 <- ggplot(d, aes(pLinked, colour = exp)) +
  geom_density() +
  xlab("Probability of Linkage to Care") +
  ggtitle("Linkage to Care in 2015") +
  theme_classic()
p3 # LINKAGE TO CARE

p4 <- ggplot(d[d$Year >=2010 & d$Year <2015,], aes(pEngaged, colour = exp)) +  # >= 2 VL measurements in 12 months
  geom_density() +
  facet_grid(yr ~ .)  +
  xlab("Probability of Engagement in Care in 2015") +
  ggtitle("Engagement in Care in 2015") +
  theme_classic()
p4 # ENGAGEMENT IN CARE


p5 <- ggplot(d[d$Year >=2010 & d$Year <2015,], aes(vl, fill = yr)) +
  geom_histogram(colour = "black") +
  facet_grid(yr ~ .)  +
  xlab("Viral Suppression %") +
  xlim(0,100) +
  ggtitle("Viral Suppression (%) in US States") +
  theme_classic()
p5 # VIRAL SUPPRESSION


# THIS IS GOLD!
p6 <- ggplot(d[d$Year >=2010 & d$Year <2015,], aes(vl, colour = exp, linetype = expand2014)) +
  geom_density() +
  facet_grid(yr ~ .)  +
  xlab("Viral Suppression (% of Diagnosed PLWH)") +
  ggtitle("Viral Suppression") +
  xlim(0,100)+
  theme_classic()
p6 # VIRAL SUPPRESSION


p7 <- ggplot(d[d$Year <= 2015 & d$Year >= 2011,], aes(Rate_delta, colour = exp)) +  # >= 2 VL measurements in 12 months
  geom_density() +
  facet_grid(yr ~ .)  +
  xlab("Change in Rate of HIV Infections Compared to Previous Year") +
  theme_classic()
p7 # CHANGE IN RATES (does this show the opposite of what we would hope?)

ggplot(d, aes(Year, pVS, colour = expand2014)) +
  geom_smooth(se = FALSE, method = 'lm') + 
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic()

ggplot(d, aes(Year, pVS)) +
  geom_smooth(data = d[d$Year <=2013,], aes(Year, pVS, colour = expand2014), se = FALSE, method = 'lm') + 
  geom_smooth(data = d[d$Year >=2013,], aes(Year, pVS, colour = expand2014), se = FALSE, method = 'lm') + 
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic()

ggplot(d, aes(Year, pVS, group = state)) +
  geom_smooth(data = d[d$Year <=2013,], aes(Year, pVS, colour = expand2014), se = FALSE, method = 'lm') + 
  geom_smooth(data = d[d$Year >=2013,], aes(Year, pVS, colour = expand2014), se = FALSE, method = 'lm') + 
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic()

multiplot(p3, p6, cols=2)

plotmeans(vl ~ Year, main="Heterogeineity across years", data=d)
