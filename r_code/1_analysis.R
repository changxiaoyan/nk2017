rm(list = ls())
library(arm)
library(dplyr)
library(texreg)
library(ggplot2)
library(GGally)
library(MASS)
#read the data
load("./clean_data/state.panel.RData")

# EDA
#ggpairs(state.panel, columns = c("polity2", "pop","gdppc", "gdpgrowth", "miliper",
#                                 "state_vio", "nonstate_vio", "oneside_vio"))
ggpairs(state.panel, columns = c("pop","gdppc", "gdpgrowth"))
ggsave("./writeup/images/eda.png")

##transform var

state.panel <- state.panel %>% 
       mutate(pop = log(pop + 1),
              gdppc = log(gdppc + 1),
              miliper = log(miliper + 1)
       ) %>% 
       na.omit(.)

##include a binay 
state.panel <- state.panel %>% 
       dplyr::mutate(polity_dummy = ifelse(polity2 >=6, 1, 0))



#write to stata
#library(foreign)
#write.dta(state.panel, file = "./clean_data/state.panel.dta")

# first model
m1_lm <- lm(state_vio ~ polity2 + pop + gdppc + gdpgrowth + miliper + year,
            data = state.panel)
# NB model
m1_nb <- glm.nb(state_vio ~ polity2 + pop + gdppc + gdpgrowth + miliper + year,
                data = state.panel)

#second model
m2_lm <- lm(nonstate_vio ~ polity2 + pop + gdppc + gdpgrowth + miliper + year,
            data = state.panel)

# glm

m <- glm(vio ~ polity2 + pop + gdppc + gdpgrowth + miliper + year, 
         family = binomial(link = "logit"),
         data = state.panel)


m2 <- glm(vio ~ polity_dummy + pop + gdppc + gdpgrowth + miliper + year, 
          family = binomial(link = "logit"),
          data = state.panel)


## result to a table
screenreg(list(m1_lm, m1_nb, m2_lm, m, m2))
texreg(list(m1_lm, m1_nb, m2_lm, m, m2), file = "images/tab1.tex",
       caption = "Regression Results",
       custom.coef.names = c("(Intercept)", "Polity IV score","Population(log)",
                             "GDP per captia (log)", "GDP growth (%)",
                             "% of Armed forces personnel", "year", "Polity(dummy)"))
                            


