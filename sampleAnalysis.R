##### Purpose : Analyse business questions 
##### Author : Jean-Philippe 

### SECTION Object use across all questions
##########################################################

################ TOP APP DATA ####################################
## Merge top_app_data 
top_app_data <- merge_csv("C:\\Users\\Terry\\Desktop\\Project\\Applestore\\data\\")
top_app_data <- subset(top_app_data,category != "Top+Apps" )

# Add AppID field
top_app_data$AppID<-substr(top_app_data$topapp_url_product,regexpr('www.topappcharts.com'
                             ,top_app_data$topapp_url_product)+21
                           ,regexpr('app-details',top_app_data$topapp_url_product)-2)

# Shorten category name and rename
category <- gsub("\\+Games","",top_app_data$category)
category <- gsub("\\+Apps","",category)
category <- gsub("\\+Playing","",category)
category <- gsub("Role","RPG",category)
top_app_data$category <- category

################### ITUNES DATA ##################################################

# extract uniqe url of top app data 
apple_app_url <- data.frame(unique(top_app_data$apple_store_url),stringsAsFactors = F)
colnames(apple_app_url) <- "url"

# extract id of apple data
for (i in 1:nrow(apple_app_url)) {
  
  apple_app_url$AppID[i] <-substr(apple_app_url$url[i],regexpr('/id',apple_app_url$url[i])
                                  +3,regexpr('?mt',apple_app_url$url[i])-2)
  
}

apple_app_url <- apple_app_url[!duplicated(apple_app_url$AppID), ]

# read apple data
# depending on your wi-fi connection. I recommend not more than 100 url at a time and 30 seconds wait between each batch
sequence <- seq(from = 1,to = length(apple_app_url),950)
sequence <- c(sequence,nrow(apple_app_url))

# Load all data
apple_data <- appleStoreScraperFacilitator(apple_app_url,sequence)

# extract id of apple data
apple_data$AppID<-substr(apple_data$apple_store_url,regexpr('/id',apple_data$apple_store_url)
                         +3,regexpr('?mt',apple_data$apple_store_url)-2)

# compute  days_since_launch

apple_data$days_since_launch <- as.numeric(last_day_observation - as.Date(apple_data$publish_date))

########################## Merge ########################################

apple_topapp_base <-merge(top_app_data,apple_data,all.x=T,by="AppID")
#saveRDS(apple_topapp_base,"apple_topapp_base.rds")
apple_topapp_base <- readRDS("C:\\Users\\Terry\\Desktop\\Project\\Applestore\\Final_data\\apple_topapp_base.rds")

##########END section object use across all questions

##############################################################################
##### Start Section : How does the variation of paid app price affect ranking ?

# Only consider paid App
top_app_data_paid <- subset(top_app_data,price > 0)

# I consider the average of rank for individual app
dt_reshape <- dcast(top_app_data_paid,AppID+name+category+device~price
                    ,value.var="ranking",mean)

### Identify the most occuring price change. Analyse top #3
# We can see that the most occuring number of price change is 1 and after 2   (5% of data)
# top prices change are : 0.99-1.99 0.99-2.99 . 

dt_reshape$number_price_change <- apply(X =dt_reshape[,c(5:ncol(dt_reshape))],
                                 MARGIN = 1, function(x) {sum(!is.na(x))})
table(dt_reshape$number_price_change)
dt_price <- subset(dt_reshape,number_price_change==2)
price_change <- apply(X =dt_price[,c(5:(ncol(dt_price)-1))], 1, function(x) names(which(!is.na(x))) )
first_element <- price_change[1,]
second_element <- price_change[2,]
price_combination <- paste0(first_element,"-",second_element)
table(price_combination)

# calculate ranking_change_avg_difference
dt_price$ranking_change_199_099 <- dt_price$`0.99` - dt_price$`1.99`
dt_price$ranking_change_299_099 <- dt_price$`0.99` - dt_price$`2.99`
dt_price$ranking_change_399_099 <- dt_price$`0.99` - dt_price$`3.99`
dt_price$ranking_change_499_099 <- dt_price$`0.99` - dt_price$`4.99`

# remove na value 
dt_price_199_099<-dt_price[!is.na(dt_price$ranking_change_199_099),]
dt_price_299_099<-dt_price[!is.na(dt_price$ranking_change_299_099),]
dt_price_399_099<-dt_price[!is.na(dt_price$ranking_change_399_099),]
dt_price_499_099<-dt_price[!is.na(dt_price$ranking_change_499_099),]

# boxplot category global 
par(mfrow=c(2,2))
hist(dt_price_199_099$ranking_change_199_099,main="",ylab = "Nb changement de rangs",xlab ="Différence de rangs lorsque le prix change de 0.99$ à 1.99$")
hist(dt_price_299_099$ranking_change_299_099,main="",ylab = "Nb changement de rangs",xlab ="Différence de rangs lorsque le prix change de 0.99$ à 2.99$")
hist(dt_price_399_099$ranking_change_399_099,main="",ylab = "Nb changement de rangs",xlab ="Différence de rangs lorsque le prix change de 0.99$ à 3.99$",
xlim = c(-100,100))
hist(dt_price_499_099$ranking_change_499_099,main="",ylab = "Nb changement de rangs",xlab ="Différence de rangs lorsque le prix change de 0.99$ à 4.99$",
     xlim = c(-150,150))

#### Section with ranking_change_099_199 

# only consider top category for plot illustration
category <- sqldf("select category, count(ranking_change_199_099) from dt_price_199_099 
       group by category order by 2 desc")
top5_category <- category[1:5,1]
dt_category <- subset(dt_price_199_099,category %in% top5_category)



# plot boxplot 0.99 à 1.99$
  
p <- ggplot(dt_category, aes(factor(category), ranking_change_199_099, fill=factor(category)))
p1 <- p + geom_boxplot()
p2 <- p1 + geom_point(position = position_jitter(width = 0.1)) + theme(text = element_text(size=18))
p3 <- p2 + labs(x = "Catégories",y = "Difference de rang")
p4 <- p3 + coord_cartesian(ylim = c(-100, 100))   # this delete the extreme value display
p4

  #### Section with ranking_change_099_299 
# remove na value 
dt_price_299_099<-dt_price[!is.na(dt_price$ranking_change_299_099),]

# only consider top category for plot illustration
category <- sqldf("select category, count(ranking_change_299_099) from dt_price_299_099 
                  group by category order by 2 desc")
top5_category <- category[1:5,1]
dt_category <- subset(dt_price_299_099,category %in% top5_category)


# plot boxplot 0.99 à 2.99$
p <- ggplot(dt_category, aes(factor(category), ranking_change_299_099, fill=factor(category)))
p1 <- p + geom_boxplot()
p2 <- p1 + geom_point(position = position_jitter(width = 0.1)) + theme(text = element_text(size=18))
p2 + labs(x = "Categories",y = "Difference de rang")

# NOTE : il serait intéressant d'ajouter deux plots global sans catégorie ( la première
# serait la boxplot de 0.99 à 2.99 et la deuxième de 0.99 à 1.99)  


#### End section : How does the variation of paid app price affect ranking ?
################################################################################
##############################################################################


##############################################################################
##############################################################################
##### Start Section : Intensity of competition in order to reach the top  #200


### Data preparation

data_intensity <- apple_topapp_base

# consider only required column 
data_intensity <- subset(data_intensity,select = c(AppID,publish_date,device, pricing_model,category,ranking,
                                                  days_since_launch))

# investigate missing value.
# 1% publish_date  missing at random. It's ok to remove them
sapply(data_intensity, function(x) sum(is.na(x)))
data_intensity <-  data_intensity[complete.cases(data_intensity),]

# explore data plot size by group rank
data_intensity$bining <- ceiling(as.numeric(data_intensity$ranking)/10)
ranking <- rankingGroup(data= data_intensity ,bin_number = 20) 
data_intensity <- merge(data_intensity,ranking,by = "bining", all.x=T)
  
#### iPhone Free  # TOP 30 ranking

# I query average days since launch by category to identify the top / bottom
# category to plot 

df_monopoly_1_10_Free_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Free",
                                                  device ="iPhone" ,cat_ranking = "1-10")
df_monopoly_11_20_Free_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Free",
                                                         device ="iPhone" ,cat_ranking = "11-20")
df_monopoly_21_30_Free_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Free",
                                                         device ="iPhone" ,cat_ranking = "21-30")

df_competition_1_10_Free_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Free",
                                                         device ="iPhone" ,cat_ranking = "1-10")
df_competition_11_20_Free_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Free",
                                                          device ="iPhone" ,cat_ranking = "11-20")
df_competition_21_30_Free_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Free",
                                                          device ="iPhone" ,cat_ranking = "21-30")

par(mfrow=c(2,3))
boxplot(days_since_launch ~ category,data = df_monopoly_1_10_Free_iPhone, 
        main="Rangs 1 à 10",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_11_20_Free_iPhone, 
        main="Rangs 11 à 20",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_21_30_Free_iPhone, 
        main="Rangs 21 à 30",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_1_10_Free_iPhone, 
        main="Rangs 1 à 10",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_11_20_Free_iPhone, 
        main="Rangs 11 à 20",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_21_30_Free_iPhone, 
        main="Rangs 21 à 30",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")


### iPhone Paid 


df_monopoly_1_10_Paid_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Paid",
                                                         device ="iPhone" ,cat_ranking = "1-10")
df_monopoly_11_20_Paid_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Paid",
                                                          device ="iPhone" ,cat_ranking = "11-20")
df_monopoly_21_30_Paid_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Paid",
                                                          device ="iPhone" ,cat_ranking = "21-30")

df_competition_1_10_Paid_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Paid",
                                                               device ="iPhone" ,cat_ranking = "1-10")
df_competition_11_20_Paid_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Paid",
                                                                device ="iPhone" ,cat_ranking = "11-20")
df_competition_21_30_Paid_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Paid",
                                                                device ="iPhone" ,cat_ranking = "21-30")

par(mfrow=c(2,3))
boxplot(days_since_launch ~ category,data = df_monopoly_1_10_Paid_iPhone, 
        main="Rangs 1 à 10",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_11_20_Paid_iPhone, 
        main="Rangs 11 à 20",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_21_30_Paid_iPhone, 
        main="Rangs 21 à 30",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_1_10_Paid_iPhone, 
        main="Rangs 1 à 10",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_11_20_Paid_iPhone, 
        main="Rangs 11 à 20",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_21_30_Paid_iPhone, 
        main="Rangs 21 à 30",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

#### iPhone Free  # Bottom 171 - 200 ranking

# I query average days since launch by category to identify the top / bottom
# category to plot 

df_monopoly_171_180_Free_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Free",
                                                         device ="iPhone" ,cat_ranking = "171-180")
df_monopoly_181_190_Free_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Free",
                                                          device ="iPhone" ,cat_ranking = "181-190")
df_monopoly_191_200_Free_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Free",
                                                          device ="iPhone" ,cat_ranking = "191-200")

df_competition_171_180_Free_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Free",
                                                               device ="iPhone" ,cat_ranking = "171-180")
df_competition_181_190_Free_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Free",
                                                                device ="iPhone" ,cat_ranking = "181-190")
df_competition_191_200_Free_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Free",
                                                                device ="iPhone" ,cat_ranking = "191-200")

par(mfrow=c(2,3))
boxplot(days_since_launch ~ category,data = df_monopoly_171_180_Free_iPhone, 
        main="Rangs 171 à 180",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_181_190_Free_iPhone, 
        main="Rangs 181 à 190",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_191_200_Free_iPhone, 
        main="Rangs 191 à 200",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_171_180_Free_iPhone, 
        main="Rangs 171 à 180",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_181_190_Free_iPhone, 
        main="Rangs 181 à 190",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_191_200_Free_iPhone, 
        main="Rangs 191 à 200",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")


### iPhone Paid 

df_monopoly_171_180_Paid_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Paid",
                                                         device ="iPhone" ,cat_ranking = "171-180")
df_monopoly_181_190_Paid_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Paid",
                                                          device ="iPhone" ,cat_ranking = "181-190")
df_monopoly_191_200_Paid_iPhone <- top5CategoryMonopolyData(data_intensity,pricing_model = "Paid",
                                                          device ="iPhone" ,cat_ranking = "191-200")

df_competition_171_180_Paid_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Paid",
                                                               device ="iPhone" ,cat_ranking = "171-180")
df_competition_181_190_Paid_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Paid",
                                                                device ="iPhone" ,cat_ranking = "181-190")
df_competition_191_200_Paid_iPhone <- top5CategoryCompetitionData(data_intensity,pricing_model = "Paid",
                                                                device ="iPhone" ,cat_ranking = "191-200")

par(mfrow=c(2,3))
boxplot(days_since_launch ~ category,data = df_monopoly_171_180_Paid_iPhone, 
        main="Rangs 171 à 180",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_181_190_Paid_iPhone, 
        main="Rangs 181 à 190",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_monopoly_191_200_Paid_iPhone, 
        main="Rangs 191 à 200",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_171_180_Paid_iPhone, 
        main="Rangs 171 à 180",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_181_190_Paid_iPhone, 
        main="Rangs 181 à 190",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

boxplot(days_since_launch ~ category,data = df_competition_191_200_Paid_iPhone, 
        main="Rangs 191 à 200",ylim = c(0, 2500),
        xlab="Catégories", ylab="Jours depuis la publication")

#### End section : Intensity of competition in order to reach the top  #200
################################################################################
################################################################################


##############################################################################
##### Start Section : Is there a correlation between meg and ranking ?

### Data Preparation
# Transform variables ranking and meg into numeric to do the correlation
apple_topapp_data <- transform(apple_topapp_base,
                               size = as.numeric(size), 
                               ranking = as.numeric(ranking))

# subset to keep only required column. Also facilitate correlation computation 
apple_topapp_data <- subset(apple_topapp_data,select = c(ranking,size,category,device,pricing_model))

# investigate missing value.
# # calculate global correlation.
# 1% size observations missing at random. It's ok to ignore them
sapply(apple_topapp_data, function(x) sum(is.na(x)))
apple_topapp_data <-  apple_topapp_data[complete.cases(apple_topapp_data),]

# Explore data by ploting size and ranking
plot(x = apple_topapp_data$ranking,y=apple_topapp_data$size,
     type="p",
     xlab = "Rang",ylab="Poids en Meg")

# explore data plot size by group rank
dfgroup <- apple_topapp_data 
dfgroup$bining <- ceiling(as.numeric(dfgroup$ranking)/50)
ranking <- rankingGroup(data= dfgroup ,bin_number = 4) 
dfgroup <- merge(dfgroup,ranking,by = "bining", all.x=T)

# plot exploration
p <- ggplot(dfgroup, aes(factor(cat_ranking,levels = c("1-50","51-100","101-150","151-200")),size))
p1 <- p + geom_boxplot()
p2 <- p1 + labs(x = "Groupe de Rang",y = "Poids en meg")
p2 + theme(axis.text=element_text(size=12,colour="black"))

# plot with limit on y axes
p <- ggplot(dfgroup, aes(factor(cat_ranking,levels = c("1-50","51-100","101-150","151-200")),size))
p1 <- p + geom_boxplot() 
p2 <- p1 + labs(x = "Groupe de Rang",y = "Poids en meg") + ylim(0,110)
p2 + theme(axis.text=element_text(size=12,colour="black"))


# add variables int_device_pricemodel to calculate correlation group by factor
apple_topapp_data$int_device_pricemodel <-paste0(apple_topapp_data$device,"_",apple_topapp_data$pricing_model)

cor(apple_topapp_data$ranking,apple_topapp_data$size,use="complete.obs", method="spearman")

# calculate correlation per category and variable int_device_pricemodel
dt_apple_top <- data.table(apple_topapp_data)
dt_apple_cor <- dt_apple_top[, .(correlation = cor(ranking,size,use="complete.obs", method="spearman")), by=c("category","int_device_pricemodel")]

# only plot weak to moderate correlation result  -20 < x > .20  
dt_apple_imp_cor <- subset(dt_apple_cor,subset = correlation >= 0.20 | correlation <= -0.20)

# plot geom_points : x : category , y = correlation, fill = variable int
p <- ggplot(dt_apple_cor , aes(factor(category), correlation))
p1 <- p +  geom_point(aes(color = factor(int_device_pricemodel)),size=10)
p2 <- p1 + labs(x="Catégories", y="Corrélation Spearman")
p3 <- p2 + theme(axis.text=element_text(size=12,colour="black"))
#p3 +  coord_cartesian(ylim = c(-0.50, 0.50))


#### End section :  Is there a correlation between meg and ranking ?
################################################################################





##############################################################################
##############################################################################
##### Start Section : Survival analysis

# Global Variables
day_observation <- as.Date("2015-09-01") 
last_day_observation <- as.Date("2015-10-31") 

# computed variables days_since_last_observation
# to keep only observations that where publish after first date of data collection
days_since_last_observation <- as.numeric(last_day_observation - day_observation)

# data  
# only keep data with publish data after day_0_observations
survival <- subset(apple_topapp_base,days_since_launch <= days_since_last_observation)

###---------- First phase investigate data and clean data

# validate publish date subset on survival data

barplot(table(survival$publish_date),xlab = "Date de publication",
                                    ylab = "Numbre d'applications")

# number of unique apps 
length(unique(x = survival$AppID))

# count the number of apps group by ranking
sqldf("select count(distinct AppID) as nb_apps, ranking
       from survival
       group by ranking")

# count the number of apps group by category
sqldf("select count(distinct AppID) as nombre_apps, category
       from survival
      group by category")

# reshape data with length to investigate Quality of data

survival_quality <- dcast(survival,AppID+days_since_launch+category+device+pricing_model~date
                                  ,value.var="ranking",length)

# take a look at observations with more that length > 2. Delete them only 4 apps less than 1%
appid_duplicate <- survival_quality[apply(survival_quality[,-1:-6], MARGIN = 1, function(x) any(x > 1)), ][,1]
survival <- subset(survival,!AppID %in% appid_duplicate)

# reshape data
survival_reshape <- dcast(survival,AppID+days_since_launch+category+device+pricing_model~date
                          ,value.var="ranking",sum)

# investigate reshape 
summary(survival_reshape)

# Manage missing value ##### 
  # input last day missing rank. It's ok it's not going to be critical for the report
  # rearange column
survival_reshape$`2015-09-19` <- survival_reshape$`2015-09-20`
survival_reshape$`2015-10-10` <- survival_reshape$`2015-10-11`
survival_reshape$`2015-10-14` <- survival_reshape$`2015-10-15`
survival_reshape$`2015-10-17` <- survival_reshape$`2015-10-18`

survival_reshape <- cbind(survival_reshape[,c(1:22)],survival_reshape[,62],survival_reshape[,c(23:42)],
                          survival_reshape[,63],survival_reshape[,c(43:45)], 
                          survival_reshape[,64], survival_reshape[,c(46:47)], survival_reshape[,65],
                          survival_reshape[,c(48:61)])

names(survival_reshape)[names(survival_reshape)=="survival_reshape[, 62]"] <- "2015-09-19"
names(survival_reshape)[names(survival_reshape)=="survival_reshape[, 63]"] <- "2015-10-10"
names(survival_reshape)[names(survival_reshape)=="survival_reshape[, 64]"] <- "2015-10-14"
names(survival_reshape)[names(survival_reshape)=="survival_reshape[, 65]"] <- "2015-10-17"

# delete observations with NA. 0 missing.
sapply(survival_reshape, function(x) sum(is.na(x)))  
survival_reshape <-  survival_reshape[complete.cases(survival_reshape),]

# transform into factor variables
survival_reshape$category <- as.factor(survival_reshape$category)
survival_reshape$device   <- as.factor(survival_reshape$device)
survival_reshape$pricing_model <-   as.factor(survival_reshape$pricing_model)
survival_reshape$category <- relevel(x =survival_reshape$category, ref = "RPG")

#create a categorical variables that will group category
survival_reshape$group_ <- revalue(survival_reshape$category, c("Action"="games",
                                                "Adventure" = "games",
                                                "Arcade" = "games",
                                                "Puzzle" = "games",
                                                "Racing" = "games",
                                                "RPG" = "games",
                                                "Simulation" = "games",
                                                "Strategy" = "games",
                                                "Trivia" = "games",
                                                "Books" = "kids_family",
                                                "Education" = "kids_family",
                                                "Educational" = "kids_family",
                                                "Family" = "kids_family"))

# calculate time and censure 
survival_reshape_tc_200 <- time_censure(data = survival_reshape,censure_quit_rank = 200,censure_days_interval = 3,
                                      last_day_observation = last_day_observation,day_observation = day_observation)

survival_reshape_tc_100 <- time_censure(data = survival_reshape,censure_quit_rank = 100,censure_days_interval = 3,
                                    last_day_observation = last_day_observation,day_observation = day_observation)
survival_reshape_tc_100 <- subset(survival_reshape_tc_100,first_ranked_day > 0)

survival_reshape_tc_50 <- time_censure(data = survival_reshape,censure_quit_rank = 50,censure_days_interval = 3,
                                        last_day_observation = last_day_observation,day_observation = day_observation)
survival_reshape_tc_50 <- subset(survival_reshape_tc_50,first_ranked_day > 0)

survival_reshape_tc_10 <- time_censure(data = survival_reshape,censure_quit_rank = 10,censure_days_interval = 3,
                                       last_day_observation = last_day_observation,day_observation = day_observation)
survival_reshape_tc_10 <- subset(survival_reshape_tc_10,first_ranked_day > 0)

# verification du nombre d'apps par catégorie 
sqldf("select count(distinct AppID) as nombre_apps, category
        from survival_reshape_tc_10
             group by category")

# the Books categorie observation is small be carefull with data interpretation

sqldf("select count(distinct AppID) as nombre_apps, category
      from survival_reshape_tc_50
      group by category")


# descriptive statistics

hist(survival_reshape_tc_200$censure_200)
summary(survival_reshape_tc_200$time_200)

hist(survival_reshape_tc_100$censure_100)
summary(survival_reshape_tc_100$time_100)

hist(survival_reshape_tc_50$censure_50)
summary(survival_reshape_tc_50$time_50)

par(mfrow=c(2,2))
hist(survival_reshape_tc_200$time_200,ylab = "Nombre d'applications", xlab = "Temps",main = "Rang 1 à 200")
hist(survival_reshape_tc_100$time_100,ylab = "Nombre d'applications", xlab = "Temps",main = "Rang 1 à 100")
hist(survival_reshape_tc_50$time_50,ylab = "Nombre d'applications", xlab = "Temps",main = "Rang 1 à 50")


### Survival Analysis
## WARNING MAKE SURE CENSURE IS GOOD and survfit is good by looking at the TP
## And Y axis column



# Gobal survival function 
sortie <- survfit(Surv(time_200,censure_200)~1,data=survival_reshape_tc_200)
plot_1 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 200") + 
       theme(text = element_text(size=18), 
             axis.text.x = element_text(size=18,colour="black"),
             axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_100,censure_100)~1,data=survival_reshape_tc_100)
plot_2 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 100")  + 
        theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 


sortie <- survfit(Surv(time_50,censure_50)~1,data=survival_reshape_tc_50)
plot_3 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 50")  + 
        theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black"))

grid.arrange(plot_1, plot_2,plot_3, ncol=2,nrow = 2)


# Survival function with pricing_model as factor
sortie <- survfit(Surv(time_200,censure_200)~1 + pricing_model,data=survival_reshape_tc_200)
plot_1 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 200") + 
       theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_100,censure_100)~1 + pricing_model,data=survival_reshape_tc_100)
plot_2 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 100") + 
       theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_50,censure_50)~1 + pricing_model,data=survival_reshape_tc_50)
plot_3 <-ggsurv(sortie,xlab = "NNombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 50") + 
       theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

grid.arrange(plot_1, plot_2,plot_3, ncol=2,nrow = 2)

# Survival function with device as factor
sortie <- survfit(Surv(time_200,censure_200)~1 + device ,data=survival_reshape_tc_200)
plot_1 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 200") + 
       theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_100,censure_100)~1  + device ,data=survival_reshape_tc_100)
plot_2 <- ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 100") + 
        theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

sortie <- survfit(Surv(time_50,censure_50)~1  + device ,data=survival_reshape_tc_50)
plot_3 <-ggsurv(sortie,xlab = "Nombre de jours classés",
       ylab= "Probabilité de Survie", main = "Rang 1 à 50") + 
        theme(text = element_text(size=18), 
        axis.text.x = element_text(size=18,colour="black"),
        axis.text.y = element_text(size=18,colour="black")) 

grid.arrange(plot_1, plot_2,plot_3, ncol=2,nrow = 2)

# Survival function with category as factor
sortie <- survfit(Surv(time_200,censure_200)~1 + category,data=survival_reshape_tc)
summary(sortie)
# cox model

modele.cox<-coxph(Surv(time_200,censure_200)~1 + pricing_model + device + category,data=survival_reshape_tc_200)
summary(modele.cox)
modele.cox<-coxph(Surv(time_100,censure_100)~1 + pricing_model + device + category + grou,data=survival_reshape_tc_100)
summary(modele.cox)
modele.cox<-coxph(Surv(time_50,censure_50)~1 + pricing_model + device + category,data=survival_reshape_tc_50)
summary(modele.cox)


modele.cox<-coxph(Surv(time_200,censure_200)~1 + pricing_model + device + group_,data=survival_reshape_tc_200)
summary(modele.cox)
modele.cox<-coxph(Surv(time_100,censure_100)~1 + pricing_model + device + group_,data=survival_reshape_tc_100)
summary(modele.cox)
modele.cox<-coxph(Surv(time_50,censure_50)~1 + pricing_model + device + group_,data=survival_reshape_tc_50)
summary(modele.cox)

# I don't need to do proportionality test since I don't have any independant 
# variable that change in time.

### interpretation 
# EX : At every moment in time, the risk that a player 
# quit the app when using an iPad is 0.80 times an iPhone






#### End section : Cox Survival Analysis
################################################################################
################################################################################
