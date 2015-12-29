## Author : Jean-Philippe Mallette

##-------- initialisation library and global variable


appleStoreScraper <- function(apple_app_url,wait_time,batch_size) {

require(stringr)
store <- data.frame(matrix(data=NA,
                    nrow = length(apple_app_url),ncol=9))
col_names<-c("apple_store_url","seller_name","version","size",
                   "rating_current_version","rating_all_version","age_rated","updated_date",
                   "publish_date")
colnames(store) <- col_names
row<-length(apple_app_url)


## --------- Read  product url and extract information on product
##           at a time  

nb_loop <- ceiling(row/batch_size)
iterator_value <- suppressWarnings(matrix(data= seq(1:row),nrow=batch_size ,ncol = nb_loop))

for (x in 1:nb_loop) { 
  
  if (x != nb_loop) {
    start_store <- batch_size * (x - 1) + 1
    end_store <- batch_size * x
    print(paste0("Now executing start row : ",start_store)) # for debug
    print(paste0("Now executing end row : ",end_store)) # for debug
    end_loop_row <- batch_size
   } else {
    start_store <- batch_size * (x - 1) + 1
    end_store <- row 
    print(paste0("Now executing start row : ",start_store)) # for debug
    print(paste0("Now executing end row : ",end_store)) # for debug
    end_loop_row <- row%%batch_size
   }

temp_store <- data.frame(matrix(data=NA,
              nrow = end_loop_row,ncol=9))

colnames(temp_store) <- col_names
  
  for (i in 1:end_loop_row) # i in 1:end_loop_row
  {  2
    
  # 1- read product url. Defensive code if url exist do all code below 
  # other wise skip everything and start the loop over
    
    iterator <- iterator_value[i,x]
    prod_url <- readLines(apple_app_url[iterator]) 
    print(paste0("row line:",iterator))
    
    # it will no longer be possible to scrap the page
    if(length(grep('Connecting to the iTunes store',
      prod_url,value=TRUE)) == F) 
    {
    
  
    temp_store[i,1] <- apple_app_url[iterator] 
    
    
    # seller name. As a side note, the seller can be different from 
    # the publisher if the publisher is on a selling plateform.
    
    author_raw <- grep('itemprop="author"',prod_url,value=TRUE)
    
    if (is.character(author_raw) && length(author_raw) == 0 ||
        length(author_raw) > 1 ) {

    temp_store[i,2] <- NA
    } else {
    temp_store[i,2] <- substr(author_raw,regexpr('itemprop="author"',author_raw)+40,
                         regexpr('class="copyright"',author_raw)-24)
    }
    # version 
    version_raw <- grep('softwareVersion',prod_url,value=TRUE)
    
    if (is.character(version_raw) && length(version_raw) == 0 ||
      length(version_raw) > 1 ){
      temp_store[i,3] <- NA
    } else {
    temp_store[i,3] <- substr(version_raw,regexpr('softwareVersion',version_raw)+17,
                         regexpr('Size:',version_raw)-37)
    }
    # Size 
    size_raw <- grep('Size:',prod_url,value=TRUE)
  
    if (is.character(size_raw) && length(size_raw) == 0 ||
        length(size_raw) > 1) {
      temp_store[i,4] <- NA
    } else {
    temp_store[i,4] <- substr(size_raw,regexpr('Size:',size_raw)+13,
                         regexpr('MB<',size_raw)-2)
    }
    
    rating_raw <- grep("ratingValue",prod_url,value=TRUE)
    
    # can have no rating in the apple temp_store      
    if (is.character(rating_raw) && length(rating_raw) == 0 ||
        length(rating_raw) > 1) {
    temp_store[i,5] <- NA
    } else {
    temp_store[i,5] <- substr(rating_raw, regexpr('ratingValue',rating_raw)+13
                           ,regexpr('</span>',rating_raw)-1) 
    }
    # All version rating  
    raw_rating_position  <- grep("All Versions:",prod_url,value=F)
    if (is.integer(raw_rating_position) && length(raw_rating_position) == 0) {
      temp_store[i,6] <- NA
    } else {
      true_position <- raw_rating_position+1
      html_contents <- prod_url[true_position]
      temp_store[i,6] <- substr(html_contents, regexpr('aria-label=',html_contents)+12
                           ,regexpr("rating-star",html_contents)-20)
    }  
    # age rated of application.
    raw_position  <- grep("Rated",prod_url,value=F)
    
    if (is.integer(raw_position) && length(raw_position) == 0) {
      temp_store[i,7] <- NA
    } else {
      true_position <- raw_position[2]
      html_contents<- prod_url[true_position]
      temp_store[i,7] <- substr(html_contents, regexpr('Rated',html_contents)+6,
                           regexpr("\\+",html_contents))
    }
    
    # udpated date
    
    updated_date <- grep("Updated:",prod_url,value=TRUE)
    
    # can have no rating in the apple temp_store      
    if (is.character(updated_date) && length(updated_date) == 0 ||
        length(updated_date) > 1)  {
      temp_store[i,8] <- NA
    } else {
      temp_store[i,8] <- substr(updated_date, regexpr('GMT',updated_date)+5
                           ,regexpr('Version',updated_date)-37) 
    }
    
    # date publish
    
    published_date <- grep("datePublished",prod_url,value=TRUE)
    
    # can have no rating in the apple store      
    if (is.character(published_date) && length(published_date) == 0 ||
        length(published_date) > 1) {
      temp_store[i,9] <- NA
    } else {
      temp_store[i,9] <- substr(published_date, regexpr('datePublished',published_date)+24
                            ,regexpr('Etc',published_date)-11) 
    }
    
    } # skip this i iteration if url does not exist
    
  } # end the loop that scrapt the apple url

store[start_store:end_store,] <- temp_store
print(paste0("Now Waiting:",wait_time," seconds"))
Sys.sleep(wait_time) # make R sleep to allow this system to be extracted
              # without being block by apple.

} # end second loop that change the parameter of the first loop 
return(store)

} # end function   


