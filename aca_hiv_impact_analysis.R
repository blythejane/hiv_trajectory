
############################################################################
### Blythe Adamson
### April 2018
### CURRENT HIV/AIDS REPORTS REVIEW
############################################################################

library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(choroplethr)
library(RColorBrewer)

source('multiplot.R')

# source(create_dataset.R)
load('dataset.RData')

# ------------------------------------------------------------------
# create dataset
# ------------------------------------------------------------------

st <- read.csv("states.csv")
st$state <- as.character(st$State) # two letter state abbreviations for figure labels
d <- d %>% left_join(st)
d <- d[!is.na(d$Abbreviation),]
  
# ------------------------------------------------------------------------
# DESCRIBE DATA
# ------------------------------------------------------------------------

source('describe_dataset.R')

p1 # Rate discontinuity
p2 # VS discontinuity
p3 # Linkage to Care
p4 # Engagement in Care
p5 # VS histograms
p6 # VS by medicaid expansion
p7 # Change in rate compared to previous year

# ------------------------------------------------------------------------
# FIXED-EFFECTS REGRESSION
# ------------------------------------------------------------------------

# regression discontinuity at 2011 when ACA was implemented
# difference in differences for medicaid expansion
#' year is centered at 2011, when the ACA was implemented so that we can interpret
#' the aca variable alone in the regression model

# Rate of new HIV diagnoses ------------------
summary(fit_rate <- glm(data = d, Rate ~ Year_c + state + exp, family = poisson(link= "log")))
exp(coef(fit_rate)) # simple fit

summary(fit_rate <- glm(data = d, Rate ~ Year_c + state + exp + aca, family = poisson(link= "log")))
exp(coef(fit_rate)) # add aca

summary(fit_rate <- glm(data = d, Rate ~ Year_c + state + exp + aca + aca*Year_c, family = poisson(link= "log")))
exp(coef(fit_rate)) # add aca interaction with time
exp(confint(fit_rate)) # confidence intervals

#' Interpret: Medicaid expansion was associated with a 3.9% reduction in the 
#' rate of new HIV infections, adjusting for trends over time and within states

Impact_Rate <- data.frame(exp(coef(fit_rate)), exp(confint(fit_rate)))
colnames(Impact_Rate) <- c('OR', '95ci_lo', '95ci_hi')


# HIV Cases---------------
summary(fit_cases <- glm(data = d, case ~ Year_c + state + aca + aca*Year_c + exp, family = poisson(link= "log")))
exp(coef(fit_cases))
exp(confin(fit_cases))
Impact_Cases <- data.frame(exp(coef(fit_cases)), exp(confint(fit_cases)))
colnames(Impact_Cases) <- c('OR', '95ci_lo', '95ci_hi')

#' next calculate the number of infections that could have been avoided in 2014 
#' in each state that did not expand medicaid, if the 2014 trend continues
#' apply the difference in rate to the population size. Sum for a national 
#' estimate

# Viral Suppression and ACA plus Medicaid
summary(fit_vs <- glm(data = d, pVS ~ Year_c + state + aca + exp, family = binomial(link = "logit")))
exp(coef(fit_vs)) # OR = 1.073 
exp(confint(fit_vs))
    #' Interpret: Medicaid expansion was associated with a 7.3% increase in number
    #' viral suppressed 
Impact_VS <- data.frame(exp(coef(fit_vs)), exp(confint(fit_vs)))
colnames(Impact_VS) <- c('OR', '95ci_lo', '95ci_hi')

  
# Describe the number of states with VL measurements reported over time
  summary(d$Year)
  length(unique(d$state[d$Year == 2010 & d$vl >0]))
  length(unique(d$state[d$Year == 2012 & d$vl >0]))
  length(unique(d$state[d$Year == 2014 & d$vl >0]))
  unique(d$state[d$Year == 2014 & d$vl >0])
  table(d$state, d$exp)

# ----------------------------------------------------------------------
# RECYCLED PREDICTIONS
# ----------------------------------------------------------------------
  
  
  # RECYCLED PREDICTIONS: rates
  d$rate_hat <- exp(predict(fit_rate, d))
  ggplot(d, aes(Year, Rate)) + 
    geom_point() + 
    geom_point(aes(Year, rate_hat, color = "red")) 
  d$exp_real <- d$exp
  d$exp <- "Not Expanded"
  d$rate_hat0 <- exp(predict(fit_rate, d))
  d$exp[d$Year >= 2014] <- "Expanded"
  d$rate_hat1 <- exp(predict(fit_rate, d))
  d$exp <- d$exp_real
  d$d_rate_diff <- d$rate_hat1 - d$rate_hat0
  summary(d$rate_hat0[d$Year==2016])
  summary(d$rate_hat1[d$Year==2016])
  summary(d$d_rate_diff[d$Year==2014])
  summary(d$d_rate_diff[d$Year==2016])
  
  
  # RECYCLED PREDICTIONS: cases
  d$cases_hat <- exp(predict(fit_cases, d))
  d$exp_real <- d$exp
  d$exp <- "Not Expanded"
  d$cases_hat0 <- exp(predict(fit_cases, d))
  d$exp[d$Year >= 2014] <- "Expanded"
  d$cases_hat1 <- exp(predict(fit_cases, d))
  d$exp <- d$exp_real
  d$marginal_cases <- d$cases_hat1 - d$cases_hat0
  summary(d$cases_hat0[d$Year==2016])
  summary(d$cases_hat1[d$Year==2016])
  summary(d$marginal_cases[d$Year==2014])
  summary(d$marginal_cases[d$Year==2016])
  
  # IMPACT ON NUMBER OF CASES
  sum(d$case[d$Year==2015])
  sum(d$marginal_cases[d$Year==2015])
  fraction_averted <- sum(d$marginal_cases[d$Year==2015])/sum(d$case[d$Year==2015])
  # 5% reduction in the number of PLWH in the US if all states had expanded medicaid in 2014
  # 48,500 fewer PLWH in 2015 if all states had expanded medicaid
  
  
  
# ----------------------------------------------------------------------
# FIGURES
# ----------------------------------------------------------------------

# Viral suppression
ggplot(d[d$Year>=2010,], aes(x = Year, y = vl, colour = expanded)) + 
  geom_smooth() +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic()
  
#' Interpret: comparing states that had not expanded medicaid before 2015
#' to states that would expand medicaid before 2015. 
#' Beware that the number of states inlcuded changes over time in this graph:
#' US States with VL: 20 in 2010; 29 in 2012; 39 in 2014

    # to overcome this problem, subset data to contain only states with VL data
    #  since 2010
    length(unique(d$state[d$Year==2010 & d$vl>0 & d$expanded==1]))
    states_2010vl <- unique(d$state[d$Year==2010 & d$vl>0])
    states_2010vl 
    d_2010vl <- d[d$state %in% states_2010vl,]

# Viral suppression
ggplot(d_2010vl[d_2010vl$Year>=2010,], aes(x = Year, y = vl, colour = expanded)) + 
  geom_smooth() +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic()


# to overcome this problem, subset data to contain only states with VL data
#  since 2012
length(unique(d$state[d$Year==2012 & d$vl>0 & d$expanded==1]))
states_2012vl <- unique(d$state[d$Year==2012 & d$vl>0])
states_2012vl 
d_2012vl <- d[d$state %in% states_2012vl,]

# Viral suppression
ggplot(d_2012vl[d_2012vl$Year>=2012,], aes(x = Year, y = vl, colour = expand2014)) + 
  geom_smooth() +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic()

ggplot(d[d$Year>=2010,], aes(x = yr, y = vl, fill = expand2014)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  scale_fill_discrete(name="Medicaid Expansion") +  
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic() +
  scale_color_brewer()

ggplot(d[d$Year>=2010,], aes(x = yr, y = vl, fill = expand2014)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  scale_fill_discrete(name="Medicaid Expansion") +  
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic() +
  scale_color_brewer()

ggplot(d, aes(Year, vl)) + 
  geom_line(aes(group = Geography, colour = expand2014)) +
  geom_point(data = d[d$Year == 2014,], aes(group = Geography, colour = expand2014)) +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  xlim(2010,2015) +
  ylim(20,80) +
  geom_text(data = d[d$Year == 2014,], aes(label=state),hjust=0, vjust=0, size = 2) +
  ggtitle("HIV Care Objective: Viral Suppression") +
  scale_color_discrete(name="Medicaid Expansion") +  
  theme_classic() +
  theme(text = element_text(size=12))


ggplot(d, aes(x = yr, y = Rate, fill = expand2014)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  scale_fill_discrete(name="Medicaid Expansion") +  
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic() +
  scale_color_brewer()

ggplot(d, aes(Year, vl)) + 
  geom_smooth(aes(group = Geography)) +
  geom_point(aes(colour = exp)) +
  xlab("Year") +
  ylab("Viral Suppression (% of PLWH with VL<200 copies/mL3)") +
  ggtitle("HIV Care Objective: Viral Suppression") +
  theme_classic() +
  theme(text = element_text(size=12))

ggplot(d, aes(x = yr, y = vl, fill = expanded)) + 
  geom_boxplot()+
  theme_classic()


ggplot(d, aes(Year, Rate)) + 
  geom_smooth(data = d[d$exp == 0,], color = "blue") +
  geom_smooth(data = d[d$exp == 1,], color = "red") +
  xlab("Year") +
  ylab("HIV Rate") +
  ggtitle("") +
  theme_classic() +
  theme(text = element_text(size=12))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 


#-----------------------------------------------------------------------------
# MAPS
#-----------------------------------------------------------------------------

df <- d[d$Year==2014,]
df$region <- tolower(as.character(df$state))
df$value <- as.numeric(df$Cases)

state_choropleth <- function(df = df, title = "", legend = "", num_colors = 7, zoom = NULL){
  c = StateChoropleth$new(df)
  c$title = title
  c$legend = legend
  c$set_num_colors(num_colors)
  c$set_zoom(zoom)
  c$render()
}

state_choropleth(df = df, legend = "HIV Prevalence")

c <- StateChoropleth$new(df)
c$legend = "HIV Prevalence"
c$set_num_colors(7)
c$set_zoom(NULL)
c$show_labels <- FALSE
fig1 <- c$render()
fig1


# ----------------------------------------------------------------------
# VECTOR PLOTS
# ----------------------------------------------------------------------

d2 <- d[d$Year == 2010 | d$Year == 2014,]
d3 <- data.frame(d2$Year, d2$state, d2$pEngaged, d2$pVS)
library("reshape2")
d_wide1 <- dcast(d3, d2.state ~ d2.Year, value.var = c("d2.pVS"))
state <- d_wide1[,1]
pVS_2010 <- d_wide1[,2]*100
pVS_2014 <- d_wide1[,3]*100
d_wide2 <- dcast(d3, d2.state ~ d2.Year, value.var = c("d2.pEngaged"))
pEn_2010 <- d_wide2[,2]
pEn_2014 <- d_wide2[,3]
d_wide <- data.frame(state, pEn_2010, pEn_2014, pVS_2010, pVS_2014)
d4 <- left_join(d_wide, d, by = "state")
d4$Medicaid <- d4$expand2014
head(d_wide)
head(d4)

# Arrows - black w/o labels
ggplot(data = d4, aes(x = pEn_2010, y = pVS_2010, group = state)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = pEn_2010, y = pVS_2010, xend = pEn_2014, yend =pVS_2014), size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_segment(aes(x = 65, y = 10, xend = 75, yend =10), size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_point(aes(x = 65, y = 10), size = 3) +
  geom_text(aes(x = 65, y = 10, label="2010"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  geom_text(aes(x = 75, y = 10, label="2014"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  xlab("Engaged in Care (%)") +
  ylab("Viral Suppression (%)") +
  ggtitle("Progress toward 90:90:90 goal") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  #geom_rect(data=NULL,aes(xmin=.9*.9*100,xmax=Inf,ymin=-Inf,ymax=Inf), fill="grey90", alpha = 0.05)+
  #geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=.9*.9*.9*100,ymax=Inf), fill="grey90", alpha = 0.05)+
  theme_classic()+ 
  #xlim(10,85) +
  ylim(10,85) +
  theme(text = element_text(size=20))

# steelblue arrows with state labels
ggplot(data = d4, aes(x = pEn_2010, y = pVS_2010, group = state)) + 
  geom_point(size = 3, colour = "steelblue") +
  geom_segment(aes(x = pEn_2010, y = pVS_2010, xend = pEn_2014, yend =pVS_2014), colour = "steelblue", size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_segment(aes(x = 65, y = 10, xend = 75, yend =10), size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_point(aes(x = 65, y = 10),  size = 3) +
  geom_text(aes(x = 65, y = 10, label="2010"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  geom_text(aes(x = 75, y = 10, label="2014"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  xlab("Engaged in Care (%)") +
  xlim(10,85) +
  ylim(10,85) +
  ylab("Viral Suppression (%)") +
  ggtitle("Progress toward 90:90:90 goal") +
  geom_hline(yintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = pEn_2010, y = pVS_2010, label=state),hjust=-0.1, vjust=1, size = 3) +
  theme_classic()+ 
  theme(text = element_text(size=20))


# VanHEM Disucssion
vanhem<- d4[d4$Geography == "Georgia" | d4$Geography == "Washington" | d4$Geography == "Maryland" | 
         d4$Geography == "California" | d4$Geography == "Florida" | d4$Geography == "New York" ,] 

ggplot(data = vanhem, aes(x = pEn_2010, y = pVS_2010, group = state)) + 
  geom_point(size = 3, colour = "steelblue") +
  geom_segment(aes(x = pEn_2010, y = pVS_2010, xend = pEn_2014, yend =pVS_2014), colour = "steelblue", size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_segment(aes(x = 65, y = 10, xend = 75, yend =10), size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_point(aes(x = 65, y = 10),  size = 3) +
  geom_text(aes(x = 65, y = 10, label="2010"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  geom_text(aes(x = 75, y = 10, label="2014"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  xlab("Engaged in Care (%)") +
  xlim(10,85) +
  ylim(10,85) +
  ylab("Viral Suppression (%)") +
  ggtitle("Changes in HIV Care Cascade") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = pEn_2010, y = pVS_2010, label=state),hjust=-0.1, vjust=1, size = 3) +
  theme_classic()+ 
  theme(text = element_text(size=20))


# States in 2014 - colored dots with labels
ggplot(data = d4, aes(x = pEn_2010, y = pVS_2010, group = Abbreviation)) + 
  geom_point(aes(x = pEn_2014, y = pVS_2014,colour = Medicaid), size = d4$case) + 
  xlab("Using ART (%)") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Cascade Status in 2014") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = pEn_2014, y = pVS_2014, label=Abbreviation, angle = 30),hjust=-0.2, vjust=.8, size = 3) +
  theme_classic()+ 
  #xlim(10,85) +
  #ylim(10,85) +
  theme(text = element_text(size=20))

# States in 2014 - colored dots with labels and bubbles sized by RATE of HIV
ggplot(data = d4[d4$Year==2014,], aes(x = pEn_2014, y = pVS_2014, group = state)) + 
  geom_point(aes(x = pEn_2014, y = pVS_2014,colour = Medicaid, size = Rate)) + 
  xlab("Engaged in Care (%)") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Cascade Status in 2014") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = pEn_2014, y = pVS_2014, label=state, angle = 30),hjust=-0.2, vjust=.8, size = 3) +
  theme_classic()+ 
  #xlim(10,85) +
  #ylim(10,85) +
  theme(text = element_text(size=20))

# States in 2014 - colored dots with labels and bubbles sized by CASES of HIV
ggplot(data = d[d$Year==2014,], aes(x = pEngaged, y = pVS*100, group = state)) + 
  geom_point(aes(colour = expand2014, size = Cases)) + 
  xlab("Engaged in Care (%)") +
  ylab("Viral Suppression (%)") +
  ggtitle("HIV Care Cascade Status in 2014") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = pEn_2014, y = pVS_2014, label=state, angle = 30),hjust=-0.2, vjust=.8, size = 3) +
  theme_classic()+ 
  #xlim(10,85) +
  #ylim(10,85) +
  theme(text = element_text(size=20))

#################################################################################
#################################################################################
# FIGURE FOR CURRENT HIV AIDS REPORTS -----------------------------------------
# 2 letter state abbreviations, bubble color by expansion and size by prevalence
ggplot(data = d[d$Year==2015,], aes(x = pEn_2014, y = pVS_2014, group = state)) + 
  geom_point(aes(colour = exp, size = case), position = "jitter", alpha = .8) + 
  xlab("Using ART (%)") +
  ylab("Viral Suppression (%)") +
  geom_rug() +
  geom_hline(yintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1) + 
  geom_vline(xintercept = 90, color = "orange", linetype = "dotted", size = 1) + 
  geom_text(aes(x = pEn_2014, y = pVS_2014, label=Abbreviation),hjust=0.3, vjust=.5, size = 1.5) +
  theme_classic()+ 
  labs(size = "HIV Prevalence") +
  labs(colour = "Medicaid Expansion") +
  xlim(30,92) +
  ylim(30,83) +
  scale_fill_discrete(name = "Medicaid Expansion", labels = c("Yes", "No")) +
  theme(text = element_text(size=15))
ggsave("Fig1.jpg", height=4.2, width=6.6, units='in', dpi=600)
# FOOTNOTES: 
# Based on ART and VS measurements reported by CDC surveillance in the 2014 calendar year
# color indicates medicaid expansion by the end of 2015
# "Using ART" is assumed to be the fraction reported as "engaged in care" among diagnosed cases
# VS is a percent of all diagnosed cases (equivalent to target of 90*90)
# gold dotted line represents the 90-90-90 targets
# the size of the bubble represents the number of cases in that state reported in 2015
# medicaid expansion indicator variable as reported by KFF report
#################################################################################
#################################################################################

# CHAR REVISON --------------------
ggplot(data = d4, aes(x = Year, y = pVS, group = state)) + 
  geom_point(aes(colour = exp, size = case), position = "jitter", alpha = .8) + 
  geom_line(colour = 'grey', position = "jitter", alpha = .8) + 
  xlab("Year") +
  ylab("Viral Suppression (%)") +
  xlim(2012,2014) +
  ylim(.2,.8) +
  geom_text(aes(x = Year, y = pVS, label=Abbreviation),hjust=0.3, vjust=.5, size = 1.5) +
  theme_classic() + 
  labs(size = "HIV Prevalence") +
  labs(colour = "Medicaid Expansion") +
  scale_fill_discrete(name = "Medicaid Expansion", labels = c("Yes", "No")) +
  theme(text = element_text(size=15))

library(dplyr)
d <- d %>%
  mutate(case)

# CHAR REVISON v2 --------------------
ggplot(data = d[d$Year==2015,], aes(x = case, y = pVS_2014, group = state)) + 
  geom_point(aes(colour = exp), position = "jitter", alpha = .8, size = 3) + 
  xlab("HIV Prevalence") +
  ylab("Viral Suppression (%)") +
  geom_text(aes(x = case, y = pVS_2014, label=Abbreviation),hjust=-.3, vjust=.5, size = 3) +
  theme_classic()+ 
  labs(size = "HIV Prevalence") +
  labs(colour = "Medicaid Expansion") +
  ylim(30,83) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_discrete(name = "Medicaid Expansion", labels = c("Yes", "No")) +
  theme(text = element_text(size=15))
ggsave("Fig1v2.jpg", height=4.2, width=6.6, units='in', dpi=600)


# CHAR REVISON v3--------------------

d_2015 = d[d$Year==2015,]

ggplot(data = d_2015, aes(x = case, y = pVS_2014, group = state)) + 
  geom_point(data = d_2015[d$exp == 'Not Expanded',], position = "jitter", shape = 1) + 
  xlab("HIV Prevalence") +
  ylab("Viral Suppression (%)") +
  geom_text(aes(x = case, y = pVS_2014, label=Abbreviation),hjust=-.3, vjust=.5, size = 3) +
  theme_classic()+ 
  labs(size = "HIV Prevalence") +
  labs(colour = "Medicaid Expansion") +
  ylim(30,83) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_discrete(name = "Medicaid Expansion", labels = c("Yes", "No")) +
  theme(text = element_text(size=15))
ggsave("Fig1v2.jpg", height=4.2, width=6.6, units='in', dpi=600)

#################################################################################
#################################################################################
# BLOG POST
ggplot(d[d$Year >=2010 & d$Year <2015,], aes(vl, fill = yr)) +
  geom_histogram(colour = "black") +
  facet_grid(yr ~ .)  +
  xlab("Viral Suppression %") +
  xlim(0,100) +
  ggtitle("Viral Suppression (%) in US States") +
  theme_classic()
ggsave("blog_fig1.jpg", height=6.8, width=5.3, units='in', dpi=600)

brewer.pal(n = 5, name = "BrBG")
# "#A6611A" "#DFC27D" "#F5F5F5" "#80CDC1" "#018571"

ggplot(d[d$Year >=2010 & d$Year <2015,], aes(vl, fill = yr)) +
  scale_fill_brewer(palette = "BrBG") +
  geom_density(alpha = 0.5) +
  xlab("Viral Suppression (%) in US States") +
  xlim(0,100) +
  geom_vline(xintercept = mean(d$vl[d$Year == 2010], na.rm = TRUE), color = "#A6611A", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(d$vl[d$Year == 2011], na.rm = TRUE), color = "#DFC27D", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(d$vl[d$Year == 2012], na.rm = TRUE), color = "#F5F5F5", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(d$vl[d$Year == 2013], na.rm = TRUE), color = "#80CDC1", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(d$vl[d$Year == 2014], na.rm = TRUE), color = "#018571", linetype = "dashed", size = 1) + 
  labs(fill = "Year") +
  theme_classic() +
  theme(text = element_text(size=20)) +
ggsave("blog_fig2.jpg", height=6, width=12, units='in', dpi=600)
#################################################################################
#################################################################################

#################################################################################
#################################################################################
# BLOG POST - revised with median
brewer.pal(n = 5, name = "BrBG")
# "#A6611A" "#DFC27D" "#F5F5F5" "#80CDC1" "#018571"

ggplot(d[d$Year >=2010 & d$Year <2015,], aes(vl, fill = yr, color = yr)) +
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  geom_density(alpha = 0.3) +
  xlab("Viral Suppression (%) in US States") +
  xlim(0,100) +
  geom_vline(xintercept = median(d$vl[d$Year == 2010], na.rm = TRUE), color = "#A6611A", linetype = "dashed", size = .7) + 
  geom_vline(xintercept = median(d$vl[d$Year == 2011], na.rm = TRUE), color = "#DFC27D", linetype = "dashed", size = .7) + 
  geom_vline(xintercept = median(d$vl[d$Year == 2012], na.rm = TRUE), color = "#F5F5F5", linetype = "dashed", size = .7) + 
  geom_vline(xintercept = median(d$vl[d$Year == 2013], na.rm = TRUE), color = "#80CDC1", linetype = "dashed", size = .7) + 
  geom_vline(xintercept = median(d$vl[d$Year == 2014], na.rm = TRUE), color = "#018571", linetype = "dashed", size = .7) + 
  labs(fill = "Year") +
  guides(colour=FALSE) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", col = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) 
  ggsave("blog_fig3.jpg", height=6, width=12, units='in', dpi=600, bg = "transparent")
#################################################################################
#################################################################################


# New Diagnosed vs. pLinkage in 2015 colored by 2014 medicaid expansion
d$nNewDiagnosed <- as.numeric(d$nNewDiagnosed)
ggplot(data = d[d$Year==2015,], aes(x = nNewDiagnosed, y = pLinked, group = state)) + 
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=90,ymax=Inf), fill="gold", alpha = 0.1)+
  geom_point(data = d[d$Year==2015 & d$expand2014 == "No expansion before 2015",], aes(x = nNewDiagnosed, y = pLinked), shape = 21, colour = "black", fill = "white", size = 2.5, stroke = 1.5) + 
  geom_point(data = d[d$Year==2015 & d$expand2014 == "Medicaid Expansion in 2014",], aes(x = nNewDiagnosed, y = pLinked), shape = 21, colour = "black", fill = "black", size = 2.5, stroke = 1.5) + 
  xlab("New HIV Diagnoses in 2015") +
  ylab("Linkage to Care (%)") +
  ggtitle("Linkage to Care in 2015") +
  geom_hline(yintercept = 90, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = nNewDiagnosed, y = pLinked, label=state, angle = 30),hjust=-0.2, vjust=.8, size = 3) +
  theme_classic()+ 
  xlim(0,6000) +
  ylim(70,100) +
  theme(text = element_text(size=20))

# Colored Arrows
ggplot(data = d4, aes(x = pEn_2010, y = pVS_2010, group = state)) + 
  geom_segment(aes(x = pEn_2010, y = pVS_2010, xend = pEn_2014, yend =pVS_2014,colour = Medicaid), size = 1.2, alpha = .5, arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  geom_point(aes(colour = Medicaid), size = 3) + 
  xlab("Engaged in Care (%)") +
  ylab("Viral Suppression (%)") +
  ggtitle("Progress") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_segment(aes(x = 65, y = 10, xend = 75, yend =10), size = 1.2, arrow = arrow(length=unit(0.30,"cm"),type = "closed")) +
  geom_point(aes(x = 65, y = 10), size = 3) +
  geom_text(aes(x = 65, y = 10, label="2010"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  geom_text(aes(x = 75, y = 10, label="2014"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  xlim(10,85) +
  ylim(10,85) +
  #geom_rect(data=NULL,aes(xmin=.9*.9*100,xmax=100,ymin=0,ymax=100), fill = "gold",  alpha = 0.001) +
  #geom_rect(data=NULL,aes(xmin=0,xmax=100,ymin=.9*.9*.9*100,ymax=100), fill = "coral",  alpha = 0.001) +
  theme_classic() + 
  theme(text = element_text(size=20))

# Everything in one
ggplot(data = d4, aes(x = pEn_2010, y = pVS_2010, group = state)) + 
  geom_segment(aes(x = pEn_2010, y = pVS_2010, xend = pEn_2014, yend =pVS_2014), arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  geom_point()+ 
  geom_point(aes(x = pEn_2014, y = pVS_2014,colour = Medicaid)) + 
  xlab("Engaged in Care (%)") +
  ylab("Viral Suppression (%)") +
  ggtitle("Progress") +
  geom_hline(yintercept = 100*.9*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_vline(xintercept = 100*.9*.9, color = "orange", linetype = "dotted", size = 1.5) + 
  geom_text(aes(x = pEn_2014, y = pVS_2014, label=state),hjust=-0.1, vjust=1, size = 3) +
  geom_segment(aes(x = 95, y = 95, xend = 100, yend =95), size = 1.2, arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  geom_point(aes(x = 95, y = 95), size = 3) +
  geom_text(aes(x = 95, y = 95, label="2010"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  geom_text(aes(x = 100, y = 95, label="2014"),hjust=-0.2, vjust=-.5 ,angle=45, size = 5) +
  xlim(10,85) +
  ylim(10,85) +
  theme_classic()+ 
  theme(text = element_text(size=20))


#################################################################################
# TABLE 2: 
#################################################################################

d2$year_cat <- as.factor(d2$Year)
test <- acast(d2, Geography + year_cat ~ pVS, length)

state <- "California"
state <- levels(d$State)
m <- NULL
for(i in state){
    incDx <- (d$rate_diagnosed[d$Geography == i & d$Year == "2016"] - 
                d$rate_diagnosed[d$Geography == i & d$Year == "2015"])/d$rate_diagnosed[d$Geography == i & d$Year == "2015"]
    incART <- (d$pEngaged[d$Geography == i & d$Year == "2014"] - 
                 d$pEngaged[d$Geography == i & d$Year == "2012"])/ d$pEngaged[d$Geography == i & d$Year == "2012"]
    incVS <- (d$pVS[d$Geography == i & d$Year == "2014"] - 
                d$pVS[d$Geography == i & d$Year == "2012"]) / d$pVS[d$Geography == i & d$Year == "2012"]
    m <- rbind(m, c(i, mean(d$expand_yr[d$Geography==i]), incDx, incART, incVS))
  }
colnames(m) <- c('State', 'ExpYear', 'inc_Dx', 'inc_ART', 'inc_VS')
m <- data.frame(m)
head(m)
m$inc_Dx <- round(as.numeric(as.character(m$inc_Dx))*100, 1)
m$inc_ART <- round(as.numeric(as.character(m$inc_ART))*100, 1)
m$inc_VS <- round(as.numeric(as.character(m$inc_VS))*100, 1)
head(m)
summary(m)
m_exp <- m[!is.na(m$ExpYear) & m$ExpYear == "2014",]
m_mid <- m[!is.na(m$ExpYear) & m$ExpYear != "2014",]
m_no <- m[is.na(m$ExpYear),]
summary(m_exp)
summary(m_mid)
summary(m_no)

m <- rbind(m_exp, m_mid, m_no)
write.csv(m, file = "inc_impact.csv")
