# appleStoreScraper

This repository will allow developer in the mobile app space to scrape free information from the itunes.com and the http://www.topappcharts.com/ website. The combination of the two websites will allow anyone interested in mobile apps to extract information regarding the characteristic of an app and is ranking. If the scraper run for multiple days, it will allow to see the evolution of any app ranking in time.

* A detail Report (written in French in MS Word format) can be found at this adress : 
* Sample of Data that were collected from september to october is .... . The format of the data is in RDS and it can be found in the repository.

Before extracting, I strongly recommand to source the main.File. Don't forget to change the directory ! 
* source("mainFonctions.R")

Afterward, you can scrape any category of app found in the http://www.topappcharts.com/ website.
Suppose you want to extract the data found in the Books category. Simply execute the following 
line of code. This will scrape all the information of all pages in the Books category found on this 
url http://www.topappcharts.com/chart.php?show=category&category=Books&start=0.

* books <- extract_data_category("Books")

Extract url of itunes
scrap apple store 
merge the two file 
You are good to go 

Sample of analysis that can be done are found the report and in the file "sampleAnalysis.R"



For the english speaker, I encourage to look at the image. 
