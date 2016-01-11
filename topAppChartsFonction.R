# purpose : Top app chart scraper 
# author : Jean-Philippe 

#------------------- Extraction of detail of the app and ranking ------------------

#manualy write the global url you want to extract the product.
# would be a good idea to create a function with the category as argument or all.
# I only consider the chart view because easier to extract.
# don't forget that there is 8 pages.start=0, start=25, start=50 and etc

    # one store per category 
    # 200 app per category. in each category (ipad or iphone,free or paid) == 200*4=800
    # I will adjust the colonne later on.

extract_data_category <- function(category) { 

## variables 
today<-Sys.Date()
main_url_site<-"www.topappcharts.com"

data_storage <- data.frame(matrix(data=NA,
                                  nrow = 800,ncol=9)) 
colnames(data_storage)<-c("name","price","topapp_url_product","apple_store_url","device",
                          "pricing_model","ranking","category","date")

for(i in seq(0,175,25)) {
  x<- i/25
  start_storage <- 1 + (x * 100)
  end_storage <- (x+1)  * 100
  print(start_storage)
  print(end_storage)
  
  category_url<-sprintf("http://www.topappcharts.com/chart.php?show=category&category=%s&start=%s",category,i)
  # Will add a loop there for the pages 
  
  html_category_url<-readLines(category_url,warn = F)

  # Extract App name.
  # Step 1 extract the appropriate line of code of the html page 
  # Step 2 extract the string from the html line of code with stringr package  
  
  app_name_html_line <- grep('itemprop="name"',html_category_url,value=T)
  app_name_dirty_v1<- substr(app_name_html_line,regexpr('itemprop',
                      app_name_html_line)+16,regexpr("</span>",
                       app_name_html_line)-1)
  
  app_name_dirty_v2<- str_replace_all(app_name_dirty_v1,"â€"," ' ") # not working will need to correct encoding issue
  app_name_clean<- str_replace_all(app_name_dirty_v2,"&amp;","&") # correct encoding issue
  data_storage$name[start_storage:end_storage]<-app_name_clean

  # We can see that the first are free. 
  pricing_html_line <- grep('chartprice',html_category_url,value=T)
  pricing_dirty_v1<- regmatches(pricing_html_line,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",pricing_html_line)) ## special case where two prices for an app
  pricing_dirty_v1<-sapply(pricing_dirty_v1, "[", 1)  # because of special case with 2 pricing for one app
  pricing_dirty_v1[is.na(pricing_dirty_v1)] <- 0
  pricing_clean<-pricing_dirty_v1
  data_storage$price[start_storage:end_storage]<-pricing_clean
 
  # extract url of the app product_category_url necessary to extract the ranking
  
  app_url_html_line <- grep('location.href',html_category_url,value=T)
  app_url_dirty_v1<- substr(app_url_html_line,regexpr('location.href',
                                                      app_url_html_line)+15,
                            regexpr("return",app_url_html_line)-3)
  app_url_clean<-paste0(main_url_site,app_url_dirty_v1)
  data_storage$topapp_url_product[start_storage:end_storage]<-app_url_clean
  
  # extract apple_store_url
  
  app_apple_url_html_line <- grep('itunes.apple',html_category_url,value=T)
  app_apple_url_clean<- substr(app_apple_url_html_line,regexpr('itunes.apple',
                                    app_apple_url_html_line)-8,
                            regexpr("onmouseover",app_apple_url_html_line)-3)
  data_storage$apple_store_url[start_storage:end_storage]<-app_apple_url_clean
  
}
  
  # Associate iPhone or iPAD.
for (i in seq(0,750,50)){
  x<-i/50
  start_storage <- i+1
  end_storage <- i+50

  if (x%%2==0) { # if even 
  data_storage$device[start_storage:end_storage] <-"iPhone"
  } else {  # if odd
   data_storage$device[start_storage:end_storage] <-"iPad"  
  }
}
  # Create a new variable Free or Paid

for (i in 1:nrow(data_storage)) {
  
  if (data_storage$price[i]==0) {
    data_storage$pricing_model[i]<-"Free"
  } else {
    data_storage$pricing_model[i]<-"Paid"
  }
}

    # Associate ranking
  
for (i in 0:7){
  start_storage<-1+i*100
  end_storage<-(i+1)*100
  start_ranking<-1+i*25
  end_ranking<-(1+i)*25
  data_storage$ranking[start_storage:end_storage] <- start_ranking:end_ranking
}

 # Associate category and date and appID
data_storage$category<-category
data_storage$date<-today
data_storage$AppID<-substr(data_storage$topapp_url_product,regexpr('www.topappcharts.com'
                             ,data_storage$topapp_url_product)+21
                           ,regexpr('app-details',data_storag$topapp_url_product)-2)
return(data_storage)

}

