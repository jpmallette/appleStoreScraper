# appleStoreScraper

This repository will allow developer in the mobile app space to scrape free information from the itunes.com and the http://www.topappcharts.com/ website. The combination of the two websites will allow anyone interested in mobile apps to extract information regarding the characteristic of an app and is ranking. If the scraper run for multiple days, it will allow to see the evolution of any app ranking in time.

* A detail Report (written in French in MS Word format) can be found by following the link : http://bit.ly/1kGSokb.
* The file apple_topapp_base.rds contains data that were collected From start of September to the end of October 2015.

## How to extract the data

STEP 1 : SOURCE FILES AND CHANGE DIRECTORY
Before extracting, I strongly recommand to source the main.File. Don't forget to change the directory ! 
* source("mainFonctions.R")

STEP 2 : SCRAPE THE TOPAPPCHARTS WEBSITE
You can scrape any category found in the http://www.topappcharts.com/ website.
To give an example, suppose you want to extract the data found in the Books category. Simply execute the following 
line of code. 

* today<-Sys.Date()
* main_url_site<-"www.topappcharts.com"
* books <- extract_data_category("Books")

This will scrape all the information of all pages in the Books category found on this 
url http://www.topappcharts.com/chart.php?show=category&category=Books&start=0.

STEP 3 : SCRAP ITUNES WEBSITE 

Following with the example above, run the folowing line of code 
* apple_data <- appleStoreScraper(books,wait_time,batch_size)
You simply need to extract the Itunes url column in the object resulting from the extract_data_category() function

STEP 4 : MERGE THE TWO FILES


Sample analysis can be found in the file "sampleAnalysis.R"

YOU ARE GOOD TO GO ! 
