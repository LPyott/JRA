setwd("C:/DATA/CSV")
jra <- read.csv("jra.csv")

library(tidyverse)
library(dplyr)

#Imma wrangle this here data
#HC1 x30=Yes is respondents who identified as HC
HC1 <-filter(jra, X30=="Yes")
#HC2 x31=yes is respodents who live with a HC
HC2 <-filter(jra, X31=="Yes")
#HC is the union of HC! and HC2 removing duplicates
HC <- union(HC1, HC2)
#Surveys who responded yes to both questions
HCboth <- filter(HC, X30=="Yes" & X31=="Yes")
#Food insecurity questions only
foodins <- select(HC, X22, X23, X24, X25, X26)


#create food insecurity variable

foodins <- 
  foodins %>%
  mutate(X22 = case_when(
    foodins$X22 == "Often true" ~ 1,
    foodins$X22 == "Sometimes true" ~ 1,
    foodins$X22 == "Never true" ~ 0,
    foodins$X22 == "Don't know" ~ 0,
    foodins$X22 == "N/a" ~ 0),
  X23 = case_when(
    foodins$X23 == "Often true" ~ 1,
    foodins$X23 == "Sometimes true" ~ 1,
    foodins$X23 == "Never true" ~ 0,
    foodins$X23 == "Don't know" ~ 0,
    foodins$X23 == "N/a" ~ 0),
  X24 = case_when(
    foodins$X24 == "Yes:Almost every month" ~ 2,
    foodins$X24 == "Yes:Some months but not every month" ~ 2,
    foodins$X24 == "Yes:Only 1 or 2 months" ~ 1,
    foodins$X24 == "No" ~ 0,
    foodins$X24 == "Don't know" ~ 0,
    foodins$X24 == "N/a" ~ 0),
  X25 = case_when(
    foodins$X25 == "Yes" ~ 1,
    foodins$X25 == "No" ~ 0,
    foodins$X25 == "Don't know" ~ 0,
    foodins$X25 == "N/a" ~ 0),
  X26 = case_when(
    foodins$X26 == "Yes" ~ 1,
    foodins$X26 == "No" ~ 0,
    foodins$X26 == "Don't know" ~ 0,
    foodins$X26 == "N/a" ~ 0))

foodins <- foodins %>% mutate(Totals = rowSums(.[1:5]))
foodins <- foodins %>%
  mutate(Food_Security_Level = case_when(
    foodins$Totals == 0 ~ "High food Security",
    foodins$Totals <= 2 ~ "Marginal food security",
    foodins$Totals <= 4 ~ "Low food security",
    foodins$Totals > 4 ~ "Very low food security"))

foodins <- foodins %>% 
  mutate(Food_Security_Level = factor(Food_Security_Level,
                                      levels = c("Very low food security",
                                                 "Low food security",
                                                 "Marginal food security",
                                                 "High food Security")))

#barchart for food insecurity
ggplot(data=foodins, aes(x=factor(Food_Security_Level))) +
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  labs(y="Proportion", x=NULL)+
  ggtitle("Food Insecurity for Holocaust Survivors")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels=c("Very Low", "Low", "Marginal", "High", "No answer"))

#table for food insecurity
T_foodins <- table(foodins$Food_Security_Level)
T_foodins
