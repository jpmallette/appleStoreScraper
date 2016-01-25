## Purpose : Demo R Scraper meetup 
## Date : January 2016-01-28

############## INITIALISATION ################################

setwd("/Users/jpmallette/Downloads/appleStoreScraper-master/")
source("mainFonctions.R")

############## SCRAPING SECTION ###############################

top_apps_charts_books_data <- extract_data_category("Books")
unique_apple_url <- unique(top_apps_charts_books_data$apple_store_url)
apple_data <- appleStoreScraper(unique_apple_url,30,100)
apple_top_app <-merge(top_apps_charts_books_data,apple_data,all.x=T,by="AppID")

############## INSIGHTS SECTION : EXTRA MATERIALS ON HOW TO USE THE DATA 

## Start Section : Survival analysis
################################################################################
apple_top_app <- readRDS("apple_top_app.rds")

# computed variables days_since_last_observation
day_observation <- as.Date("2015-09-01") 
last_day_observation <- as.Date("2015-10-31") 
days_since_last_observation <- as.numeric(last_day_observation - day_observation)

# only keep data with published date after first day of observation
survival <- subset(apple_top_app,days_since_launch <= days_since_last_observation)

# reshape data
survival_reshape <- dcast(survival,app_id+days_since_launch+category+device+pricing_model~date
                          ,value.var="ranking",sum)
survival_reshape

#### Manage missing value

# input last previous day ranking for missing day ranking
survival_reshape$`2015-09-19` <- survival_reshape$`2015-09-20`
survival_reshape$`2015-10-10` <- survival_reshape$`2015-10-11`
survival_reshape$`2015-10-14` <- survival_reshape$`2015-10-15`
survival_reshape$`2015-10-17` <- survival_reshape$`2015-10-18`

# reorder dt
survival_reshape <- survival_reshape[,.SD,.SDcols=c(1:22,62,23:42,63,43:45,64,46,47,65,48:61)]
                                           
#### Prepare variables 
survival_reshape$category       <- as.factor(survival_reshape$category)
survival_reshape$device         <- as.factor(survival_reshape$device)
survival_reshape$pricing_model  <- as.factor(survival_reshape$pricing_model)
survival_reshape$category       <- relevel(x = survival_reshape$category, ref = "RPG")

# calculate time and censure 
survival_reshape  <- data.frame(survival_reshape)

survival_reshape_tc_200 <- time_censure(data = survival_reshape,censure_quit_rank = 200,censure_days_interval = 3,
                                        last_day_observation = last_day_observation,day_observation = day_observation)

survival_reshape_tc_100 <- time_censure(data = survival_reshape,censure_quit_rank = 100,censure_days_interval = 3,
                                        last_day_observation = last_day_observation,day_observation = day_observation)
survival_reshape_tc_100 <- subset(survival_reshape_tc_100,first_ranked_day > 0)

# descriptive statistics

table(survival_reshape_tc_200$censure_200)
summary(survival_reshape_tc_200$time_200)

table(survival_reshape_tc_100$censure_100)
summary(survival_reshape_tc_100$time_100)

par(mfrow=c(1,2))
hist(survival_reshape_tc_200$time_200,ylab = "Number of Mobile Applications", xlab = "Number of Days in Ranking",main = "Rank 1 to 200")
hist(survival_reshape_tc_100$time_100,ylab = "Number of Mobile Applications", xlab = "Number of Days in Ranking",main = "Rank 1 to 100")

### Survival Analysis

# Gobal survival function
sortie <- survfit(Surv(time_200,censure_200)~1,data=survival_reshape_tc_200)
plot_1 <- ggsurv(sortie,xlab = "Number of Days in Ranking",
                 ylab= "Survival Probability", main = "Rank 1 to 200") + 
  theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_100,censure_100)~1,data=survival_reshape_tc_100)
plot_2 <- ggsurv(sortie,xlab = "Number of Days in Ranking",
                 ylab= "Survival Probability", main = "Rank 1 to 100")  + 
  theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

grid.arrange(plot_1, plot_2, ncol=1,nrow = 2)

# Survival function with pricing_model as factor

sortie <- survfit(Surv(time_200,censure_200)~1 + pricing_model,data=survival_reshape_tc_200)
plot_1 <- ggsurv(sortie,xlab = "Number of Days in Ranking",
                 ylab= "Survival Probability", main = "Rank 1 to 200") + 
  theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_100,censure_100)~1 + pricing_model,data=survival_reshape_tc_100)
plot_2 <- ggsurv(sortie,xlab = "Number of Days in Ranking",
                 ylab= "Survival Probability", main = "Rank 1 to 100") + 
  theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

grid.arrange(plot_1, plot_2, ncol=1,nrow = 2)

# Cox model

modele.cox<-coxph(Surv(time_200,censure_200)~1 + pricing_model + device + category,data=survival_reshape_tc_200)
summary(modele.cox)

modele.cox<-coxph(Surv(time_100,censure_100)~1 + pricing_model + device + category,data=survival_reshape_tc_100)
summary(modele.cox)

### interpretation 
#  exp(-0.07434) = 0.92836 # pricing_modelPaid
#  exp(0.09272) = 1.09716 # deviceiPhone

# EX : At every moment in time, the risk that a mobile application
# quit the ranking when it's a paid apps is 7 % less than when it's free apps

# EX : At every moment in time, the risk that a mobile applications
# quit the ranking on iPhone is 9.71 % more than on iPad

#### End section : Cox Survival Analysis
################################################################################
################################################################################


