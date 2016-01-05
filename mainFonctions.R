####### Purpose : Main files where we can find function and library required to run
#######           the project

## Library section

libs <- c('reshape2', 'data.table', 'stringr','stringi',
          'sqldf','ggplot2','survival','plyr','gridExtra')
lapply(libs, function(lib) { 
  suppressPackageStartupMessages( 
    library(lib, character.only = TRUE)
  );
})

inc._installDeps <- function(lib) {
  lapply(libs, function(lib){
    install.packages(lib);
  })
}

## source other r files

source("C:\\Users\\Terry\\Desktop\\Project\\Applestore\\appleStoreScraper.r")
source("C:\\Users\\Terry\\Desktop\\Project\\Applestore\\topAppChartsFonction.r")


###################################################################################################
########## Functions sections

# merge daily topappchats csv collected in the project
# data are store in a folder name data

merge_csv <- function(string_directory) {

csv_files<-dir(string_directory)

for (i in 1:length(csv_files)) {
  
  if (i==1) {
    base_file<-fread(paste0(string_directory,csv_files[i]))
    print(sprintf("number of rows : %s in file %s",nrow(base_file),csv_files[i])) # to see function execution
    
  } else {
    append_file<-fread(paste0(string_directory,csv_files[i]))
    base_file <-rbindlist(l = list(base_file,append_file),use.names = T)
    print(sprintf("number of rows : %s in file %s",nrow(append_file),csv_files[i]))  # to see function execution
  }
}

return(base_file)

}

### Function to facilitate apple store scraper 

# example function arguments 

# function
appleStoreScraperFacilitator <- function(apple_app_url,sequence) {
  
  n_url <- length(apple_app_url)
 
  for (i in 1:(length(sequence)-1)) {
         
    if (i==1) {
      start_url_position <- sequence[i]
      end_url_position <-sequence[i+1]
      subset_apple_app_url <- apple_app_url[start_url_position:end_url_position]  
      base_data<-appleStoreScraper(subset_apple_app_url,30,50)
      print("first iteration") # to see function execution
      print(start_url_position)
      print(end_url_position)
      saveRDS(base_data,sprintf("apple_data_1_%s.rds",
                                end_url_position))
      
    } else {
      start_url_position <- sequence[i] + 1
      end_url_position <-sequence[i+1]
      subset_apple_app_url <- apple_app_url[start_url_position:end_url_position] 
      append_data<- appleStoreScraper(subset_apple_app_url,45,50)
      base_data <-rbindlist(l = list(base_data,append_data),use.names = T)
      print(sprintf("iteration number %s",i))  # to see function execution
      print(start_url_position)
      print(end_url_position)
      saveRDS(base_data,sprintf("apple_data_1_%s.rds",
                                end_url_position))
    }
  }
}


# the function allow to transform integer into associated ranking
# category . ex : 1 = 1-10 if bin_number = 20

rankingGroup <- function(data=df,bin_number = 4) {
  
  # complete integer_value
  integer_interval <- 200 / bin_number 
  array_ranking <- seq(1,200,1)
  possible_array_value <- unique(ceiling(array_ranking/integer_interval))
  
  # lower and upper bound possible value 
  first_lower_bound <- 1
  second_lower_bound <- integer_interval
  last_upper_bound <- 200
  
  lower_bound <- seq(from = first_lower_bound,to = last_upper_bound, by = integer_interval)
  upper_bound <- seq(second_lower_bound,last_upper_bound,integer_interval)
  
  # associate character to sequence 
  character_sequence <- paste0(lower_bound,"-",upper_bound)
  character_associated_integer <- data.frame(character_sequence,as.numeric(possible_array_value))
  colnames(character_associated_integer) <- c("cat_ranking","bining")
  
  # construct data frame
  return(character_associated_integer)
  
}

# Function to accelerate Data exploration in Competition intensity 


top5CategoryCompetitionData <- function(data_intensity,pricing_model,device,cat_ranking) {
  
query <-    sprintf("select pricing_model, device, cat_ranking, category,
                  avg(days_since_launch) as avg_days_since_launch  
                 from data_intensity
                 where pricing_model = '%s' and 
                 device = '%s' and 
                 cat_ranking = '%s'
                 group by pricing_model, device, cat_ranking,category
                 order by avg_days_since_launch",
                 pricing_model,device,cat_ranking)

data <-  sqldf(query)

top_5_category <- data[1:5,4]

data_intensity_sub <- subset(data_intensity, category %in% top_5_category)

query <- sprintf("select pricing_model, device, cat_ranking, category,
                 days_since_launch
                 from data_intensity_sub
                 where pricing_model = '%s' and 
                 device = '%s' and 
                 cat_ranking = '%s'"
                  ,pricing_model,device,cat_ranking)
data_intensity_sub_final <- sqldf(query)
  
return(data_intensity_sub_final)

}

top5CategoryMonopolyData <- function(data_intensity,pricing_model,device,cat_ranking) {
  
  query <-    sprintf("select pricing_model, device, cat_ranking, category,
                  avg(days_since_launch) as avg_days_since_launch  
                 from data_intensity
                 where pricing_model = '%s' and 
                 device = '%s' and 
                 cat_ranking = '%s'
                 group by pricing_model, device, cat_ranking,category
                 order by avg_days_since_launch",
                      pricing_model,device,cat_ranking)
  
  data <-  sqldf(query)
  
  last_category <- nrow(data)
  first_category <- last_category - 4  
  bottom_5_category <- data[first_category:last_category,4]
  
  data_intensity_sub <- subset(data_intensity, category %in% bottom_5_category)
  
  query <- sprintf("select pricing_model, device, cat_ranking, category,
                 days_since_launch
                 from data_intensity_sub
                 where pricing_model = '%s' and 
                 device = '%s' and 
                 cat_ranking = '%s'"
                   ,pricing_model,device,cat_ranking)
  data_intensity_sub_final <- sqldf(query)
  return(data_intensity_sub_final)
  
}


### Function to calculate time and censure 



# function parameter test


time_censure <- function(data,censure_quit_rank,censure_days_interval,last_day_observation,day_observation) {
  
  #//
  #   "The function take reshape data has data argument"
  #//
  
  # global variables
  
  ## create a temp data with only data rank
  nums <- sapply(data, is.integer)   # find numeric variables
  df_temp<- data[,nums]  # keep only numeric variables
  
  ## assign binary value to rank value to compute rank movement variables
  df_temp[(df_temp != 0 & df_temp <= censure_quit_rank)] <- 1
  df_temp[df_temp > censure_quit_rank] <- 0
  
  # compute rank movement variables for each row
  for (i in 1:ncol(df_temp)) {
    if (i == 1 ){
      rank_quit_movement <- df_temp[,i]  
    }else {
      paste_rank <- df_temp[,i]
      rank_quit_movement <- paste0(rank_quit_movement,paste_rank)
    }
  }
  
  df_temp$rank_quit_movement <- rank_quit_movement
  
  
  # extract first_ranked_day and last_ranked_day 
  for (i in 1:nrow(df_temp)) {
    tmp <- data.frame(str_locate_all(df_temp$rank_quit_movement[i],"1"))
    
    if(nrow(tmp) == 0) {
      df_temp$first_ranked_day[i] <- 0
      
    } else {
      df_temp$first_ranked_day[i] <- tmp[1,1]
    }
  }
  
  # find the last 1 and associated is day 
  for (i in 1:nrow(df_temp)) {
    tmp <- data.frame(str_locate_all(df_temp$rank_quit_movement[i],"1"))
    
    if(nrow(tmp) == 0) {
      df_temp$last_ranked_day[i] <- 0
      
    } else {
    nb_row <- nrow(tmp)
    df_temp$last_ranked_day[i] <- tmp[nb_row,1]
    }
  }
  
  # compute field

  
  df_temp$time <-df_temp$last_ranked_day - df_temp$first_ranked_day + 1
  df_temp$days_not_ranked <- last_day_observation - as.Date(day_observation + df_temp$last_ranked_day) - 1
  df_temp$censure <- ifelse(df_temp$days_not_ranked >= censure_days_interval,1,0)
  
  # return 
  data_merge<-data.frame(data,df_temp$time,df_temp$censure,df_temp$first_ranked_day)
  name_data <- names(data)
  colnames(data_merge) <- c(name_data,paste0(c("time","censure"),"_",censure_quit_rank),"first_ranked_day")
  return(data_merge)
  
}


# Function found to plot better survival plot
# http://www.r-statistics.com/2013/07/creating-good-looking-survival-curves-the-ggsurv-function/

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}
