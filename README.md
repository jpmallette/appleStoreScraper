# appleStoreScraper

## Goal of the repo

This repository will allow developer, mobile app researcher and anyone interested in getting their hand on mobile app data to scrape free information from the http://www.apple.com/ca/itunes/ and the http://www.topappcharts.com/ website. The combination of the two websites allow to extract information regarding the characteristic of an app and their ranking. If the scraper run for multiple days, it will allow to have data on the evolution of any app ranking thorought time. This repository goal is also to demonstrate how to write web scraper in R from scratch. I encourage those interested in scraping in R to consult Hadley Wickham library rvest.

* A sample analysis report (written in French in MS Word format) can be found by following the url : http://bit.ly/1kGSokb.
* The file apple_topapp_base.rds contains data that were collected from the start of September to the end of October 2015. The file is available in the repository

## Fun part: How to extract the data

STEP 1 : SOURCE FILES AND CHANGE DIRECTORY

Before extracting, I strongly recommand to first change the setwd in mainFonction.R with the path where you have downloaded the repo. Afterward, execute the mainFonction.R files

* setwd("/Users/jpmallette/Downloads/appleStoreScraper-master/")
* source("mainFonctions.R") or execute the mainFonctions.R script 

STEP 2 : SCRAPE THE TOPAPPCHARTS WEBSITE with the topAppChartsScraper.R file.

With this function, you can scrape any category found in the http://www.topappcharts.com/ website.
To give an example, suppose you want to extract the data found in the Books category. Simply execute the following 
line of code. 

* top_apps_charts_books_data <- extract_data_category("Books")

This will scrape all the information of all pages in the Books category found at this 
url: http://www.topappcharts.com/chart.php?show=category&category=Books&start=0.

STEP 3 : SCRAP ITUNES WEBSITE 

Following with the example above, run the folowing line of code to get data from the ITunes website.

* apple_data <- appleStoreScraper(top_apps_charts_books_data$apple_store_url,30,60)

The appleStoreScraper(apple_store_url,wait_time,batch_size) function include wait_time and a batch_size arguments that allow to 
read many url without being block by Apple. If you have massive amount of url to read (more than 10 000), I recommand looking at my appleStoreScraperFacilitator() function to improve scalability. I also recommand to have Ethernet cables for better connection stability.

STEP 4 : MERGE THE TWO FILES

* apple_topapp_base <-merge(top_apps_charts_books_data,apple_data,all.x=T,by="AppID")

As a further note, sample analysis can be found in the file "sampleAnalysis.R". Feel free to improve the file (aka name convention and other issues that can arise)

YOU ARE GOOD TO GO AND HAVE FUN ! 
