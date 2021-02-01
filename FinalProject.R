######################Install Packages#####################
install.packages("readxl")
install.packages("readr")
install.packages("dplyr")
install.packages("naniar")
instal.packages("tidyverse")
install.packages("psych")
install.packages("Hmisc")
install.packages("tidytext")
install.packages("tm")
install.packages("tidyr")
install.packages("stringr")
install.packages("MASS")
install.packages("faraway")
install.packages("olsrr")


#######################Read in data########################
library(readxl)
library(readr)
techdata <- read_excel("~/Downloads/2018rawdata.xlsx", 
                           sheet = "Submissions", na = "NA")
Broadband <- read_csv("~/Downloads/City Broadband Percentage.csv")
CityCensus <- read_csv("~/Downloads/CensusData (1).csv")
grades <- read_excel("~/Downloads/2018-state-school-grade-results.xlsx", 
                     sheet = "School Domain Scores")

#######################join data######################
library(dplyr)
tech_broadband <- techdata %>%
  left_join(Broadband, by = c("CITY" = "City"))
View(tech_broadband)

tech_broadband_grades <- tech_broadband %>%
  left_join(grades, by = c("CORP" = "Corp"))
View(tech_broadband_grades)

tbg_citycensus <- tech_broadband_grades %>%
  left_join(CityCensus, by = c("CITY" = "City"))
View(tbg_citycensus)

########Basic Clean Data Rd 1 ####################
df <- tbg_citycensus

  #Get rid of NA values in key columns
library(naniar)

df <- df %>% 
  replace_with_na(replace = list(`Overall Points` = "no data", 
                                 `Overall Grade` = "No Grade",
                                 `District 1:1 Status` = "no data"))

sum(is.na(df$`Overall Points`))

library(tidyverse)
df <- df %>% 
  drop_na(`Overall Points`)

sum(is.na(df$Broadband_Coverage))
df <- df %>% 
  drop_na(Broadband_Coverage)

sum(is.na(df$`District 1:1 Status`))
df <- df %>% 
  drop_na(`District 1:1 Status`)

sum(is.na(df$`Overall Grade`))
df <- df %>% 
  drop_na(`Overall Grade`)

sum(is.na(df$`Female persons, percent`))

#########################Export CSV##############################
write.csv(df, "NARemovedJoinedData.csv")

######################Import final CSV##########################
df <- read_csv("~/Downloads/CutDown_newnames.csv")

###################Clean Data Rd 2##############################

  #delete identifier columns
df <- df[-c(1,2, 9)]

  #Check for NAs, shouldn't be any
sum(is.na(df))

  #Figure out where the 4 NAs are
summary(df) #two in teacher enrollment, other two are in locale

  #change specific variables to factors
df$LOCALE <- as.factor(df$LOCALE)
df$One_One_Status <- as.factor(df$One_One_Status)

  #Get summary of the variables to ensure factoring worked
summary(df) #everything looks good, doesn't seem to be any whacky numbers

  #dummy code categorical variables
library(psych)
dummylocale <- dummy.code(df$LOCALE)
dummy1to1 <- dummy.code(df$One_One_Status)

  #add dummy code to dataframe
df_v2 <- data.frame(df, dummylocale, dummy1to1)

  #set factor variables
df_v2$Suburb..Large <- as.factor(df_v2$Suburb..Large)
df_v2$Suburb..Midsize <- as.factor(df_v2$Suburb..Midsize)
df_v2$Suburb..Small <- as.factor(df_v2$Suburb..Small)
df_v2$Town..Distant <- as.factor(df_v2$Town..Distant)
df_v2$Town..Fringe <- as.factor(df_v2$Town..Fringe)
df_v2$Town..Remote <- as.factor(df_v2$Town..Remote)
df_v2$Rural..Fringe <- as.factor(df_v2$Rural..Fringe)
df_v2$Rural..Distant <- as.factor(df_v2$Rural..Distant)
df_v2$City..Large <- as.factor(df_v2$City..Large)
df_v2$City..Midsize <- as.factor(df_v2$City..Midsize)
df_v2$City..Small <- as.factor(df_v2$City..Small)
df_v2$X1.1.at.all.grade.levels <- as.factor(df_v2$X1.1.at.all.grade.levels)
df_v2$X1.1.at.most.grade.levels <- as.factor(df_v2$X1.1.at.most.grade.levels)
df_v2$X1.1.in.some.grade.levels <- as.factor(df_v2$X1.1.in.some.grade.levels)
df_v2$No.current.plans.to.go.1.1 <- as.factor(df_v2$No.current.plans.to.go.1.1)
df_v2$Planning.to.launch.1.1.next.year <- as.factor(df_v2$Planning.to.launch.1.1.next.year)
df_v2$Studying.Considering.a.1.1 <- as.factor(df_v2$Studying.Considering.a.1.1)
###################Exploratory Data Analysis###################
library(Hmisc)
hist.data.frame(df_v2)

##########################Text Analysis#######################
#load data
library(readxl)
text <- read_excel("~/Documents/MastersProgram/MGMT 590-WDA/FinalProjectText.xlsx")

#sentiment scores by dictionary
library(dplyr)
library(tidytext)
library(tidyr)
library(tm)
library(stringr)

text <- text %>%
  mutate(id = row_number())

afinn <- text %>%
  unnest_tokens(word, `Future Plans to Add New Devices`) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarise(afinnsentiment = sum(value)) %>%
  mutate(method = "AFINN")

text_bing <- text %>% 
  unnest_tokens(word, `Future Plans to Add New Devices`) %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method = "Bing et al.")

text_nrc <- text %>%
  unnest_tokens(word, `Future Plans to Add New Devices`) %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive", "negative"))) %>%
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#join scores from all of the different tables
text_scores <- text %>%
  dplyr::left_join(afinn)

text_scores <- text_scores %>%
  left_join(text_bing, by = "id")

text_scores <- text_scores %>%
  left_join(text_nrc, by = "id")

#delete extra columns and rename for easy analysis
text_scores <- text_scores %>%
  dplyr::select(-c(id, method.x, negative.x, positive.x, method.y,
                   negative.y, positive.y)) %>%
  rename(AFINN = afinnsentiment, Bing = sentiment.x,NRC = sentiment.y)

text_scores$final_score <- rowMeans(text_scores[,c("AFINN", "Bing", "NRC")], na.rm=TRUE)

  #join text_scores$final_score to full data
df_v2 <- data.frame(df_v2, text_scores$final_score)
summary(df_v2$text_scores.final_score)
####################Model Attempt 1:Base############################

  #delete locale and district 1:1 individual variables
df_v2 <- df_v2 %>%
  dplyr::select(-c(LOCALE, One_One_Status))

df_v2$y <- df_v2$Points
df_v2 <- df_v2 %>%
  dplyr::select(-c(Points))
mod1 <- lm(y ~ ., data = df_v2)
summary(mod1)

####################Tranform variables#########################
  #transform variables that are skewed
df_v2$Tot_Net_Cap_sqrt <- sqrt(df_v2$Tot_Net_Cap+1)
df_v2$Num_Student_Devices_sqrt <- sqrt(df_v2$Num_Student_Devices+1)

df_v2 <- df_v2[-c(6,7)]
  #check for highly correlated variables, anything above .7
    #hh_income, Pop_percent_change is .715
    #Pop_percent_change, own_house_value is .738
    #blacks, white_alone is -.90
    #whites, white_alone is .97
    #whites, blacks is -.95
  #combine minority columns into one to hopefully decrease multicolinearity between white and black
df_v2$minority <- df_v2$blacks + df_v2$natives + df_v2$asian + df_v2$hawaii + df_v2$mixed_race
cor(df_v2$minority, df_v2$whites)
  #since it is still highly correlated but we don't want to delete people's identities, 
  #we won't delete white or black
  #delete white_alone, minority, hh_income, own_house_value
df_v2 <- df_v2 %>%
  dplyr::select(-c(white_alone, minority, hh_income, own_house_value))

  #delete other unnecessary variables
df_v2 <- df_v2 %>%
  dplyr::select(-c(under_5, college_grad, other_language, disabled))

  #check if dependent variable is normally distributed, looks a little skewed
qqnorm(df_v2$y)
qqline(df_v2$y, col = "steelblue", lwd = 2)

#see if transformation of y variable is needed
library(MASS)
boxcox(mod1)

  #square y and see if that helps normalize, update: it did help
df_v2$y_squared <- df_v2$y^2
qqnorm(df_v2$y_squared)
qqline(df_v2$y_squared, col = "steelblue", lwd = 2)

  #delete non-transformed y variable
df_v2 <- df_v2 %>%
  dplyr::select(-c(y))

  #standardize numeric variables
y_squared <- df_v2$y_squared

df_v2_y_removed <- df_v2 %>%
  dplyr::select(-c(y_squared))

df_v2_y_removed_stan <- df_v2_y_removed %>% 
  mutate_if(is.numeric, scale)

df_v2_stan <- data.frame(df_v2_y_removed_stan, y_squared)

#df_v2_stan <- df_v2 %>% 
  #mutate_if(is.numeric, scale)
summary(df_v2_stan)

  #look at histograms again
hist.data.frame(df_v2_stan) #nice
####################Model 2###########################
mod2 <- lm(y_squared ~ ., data = df_v2_stan)
summary(mod2)

  #remove town..remote and study.considering.a.1.1 because too small of data
df_v2_stan <- df_v2_stan %>%
  dplyr::select(-c(Town..Remote, Studying.Considering.a.1.1))

  #check model assumptions
    #load reviewDiag function
reviewDiag <- function(lmfit) {
  # Diagnostic plots
  par(mfcol=c(2,3), fg="black", bg="white",col.lab="black")
  # cooks distance - check for influential points
  cook<-cooks.distance(lmfit) 
  library(faraway) # library needed for half-normalplot
  halfnorm(cook,3,ylab="Cooks distance", main="Influences", col="blue") 
  boxplot(cook, col="blue", ylab="Cooks distance"
          , main="Boxplot Cooks Distances")
  # constant variance
  plot(fitted(lmfit),residuals(lmfit),xlab="Fitted",ylab="Residuals", col="blue"
       , pch=19, type='p', main="Resid vs. Fitted") 
  abline(h=0) 
  plot(fitted(lmfit),abs(residuals(lmfit)),xlab="Fitted",ylab="Abs(Residuals)"
       , main="Abs(Resid) vs. Fitted", col="blue", pch=19)
  # normality
  qqnorm(residuals(lmfit),ylab="Residuals", pch=19, col="blue") 
  qqline(residuals(lmfit)) 
  hist(residuals(lmfit), col="blue", main="Historgram of Residuals")
}

reviewDiag(mod2)

  #remove influential variables
cooksd <- cooks.distance(mod2)
sample_size <- nrow(df_v2_stan)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])

df_v2_stan_removed <- df_v2_stan[-influential, ]

  #rerun model and diagnostics
mod2_outliersremoved <- lm(y_squared ~ ., data = df_v2_stan_removed)
summary(mod2_outliersremoved)
anova(mod2_outliersremoved)
reviewDiag(mod2_outliersremoved)

#############################Model 3###############################
  #need to include interaction effects and variable selection methods
mod3_interaction <- lm(y_squared ~ . + .*., data = df_v2_stan)
summary(mod3_interaction)

  #too much computing power needed to run variable selection on interaction model
  #will go forward without interaction terms

#########################Model 4###########################
  #run variable selection for model 2
mod4 <- lm(y_squared~., df_v2_stan)
summary(mod4)


  #forward variable selection (tried backwards, but it removed a lot of important controls from model)
library(olsrr)
ols_step_forward_p(mod4, details = FALSE) #.2663
ols_step_forward_aic(mod4, details = FALSE) #.2654

######################Model 5 with reduced variables################
mod5 <- lm(y_squared ~ poverty+ hs_grad + Teacher_Pop + asian + Suburb..Midsize +
             whites + Pop_percent_change + Corp_Pop + Num_Student_Devices_sqrt+ X1.1.at.all.grade.levels +
             Suburb..Small + Tot_Net_Cap_sqrt + women + natives + City..Small + made_digi_books + 
             Town..Distant + Planning.to.launch.1.1.next.year + bought_digi_books + 
             text_scores.final_score + City..Large + X1.1.at.most.grade.levels + Broadband_Coverage +
             hawaii + broadband + mixed_race + hispanic + blacks + Providers + No.current.plans.to.go.1.1 + 
             live_own_house, data = df_v2_stan)
summary(mod5)

  #make sub-model with just variables significant at .1 level (except race)
mod6 <- lm(y_squared ~ poverty+ Teacher_Pop + asian + Suburb..Midsize +
             whites + Corp_Pop + Tot_Net_Cap_sqrt + natives + City..Small + made_digi_books + 
             Town..Distant + Planning.to.launch.1.1.next.year +
             text_scores.final_score + City..Large + X1.1.at.most.grade.levels + Broadband_Coverage +
             hawaii + broadband + mixed_race + hispanic + blacks, data = df_v2_stan)
summary(mod6)
  #compare to reduced model 
anova(mod5, mod6)

  #gonna go with mod 6 because there isn't a sig difference so go with simpler model
reviewDiag(mod6)

#remove influential variables
cooksd2 <- cooks.distance(mod6)
sample_size2 <- nrow(df_v2_stan)
influential2 <- as.numeric(names(cooksd2)[(cooksd2 > (4/sample_size2))])

df_v2_stan_removed2 <- df_v2_stan[-influential2, ]

#rerun model and diagnostics
mod6_outliersremoved <- lm(y_squared ~ poverty+ Teacher_Pop + asian + Suburb..Midsize +
                             whites + Corp_Pop + Tot_Net_Cap_sqrt + natives + City..Small + made_digi_books + 
                             Town..Distant + Planning.to.launch.1.1.next.year +
                             text_scores.final_score + City..Large + X1.1.at.most.grade.levels + Broadband_Coverage +
                             hawaii + broadband + mixed_race + hispanic + blacks, data = df_v2_stan_removed2)
summary(mod6_outliersremoved)
reviewDiag(mod6_outliersremoved)


