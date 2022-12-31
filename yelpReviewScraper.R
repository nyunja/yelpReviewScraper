#load libraries
library(tidyverse)
library(rvest)
library(data.table)
library(rlist)
library(dplyr)

#will hold all reviews in one plane
df_final <- list()

#find number of pages
pageNums <- page %>%
  html_elements(xpath = "//div[@aria-label='Pagination navigation']") %>%
  html_text() %>%
  str_extract('of \\d+') %>%
  str_remove('of ') %>%
  as.numeric()

#create a sequence based on the number of pages
#to be used in the URL for moving from one page to the other
pageSequence <- seq(from = 0, to = (pageNums * 10)-10, by=10)

#function to extract extra review information
extra_info_extract <- function(ei, txt) {
  str_extract(ei, paste0(txt, ".*")) %>%
    .[!is.na(.)] %>%
    str_extract("\\d+") %>%
    str_replace_na("0") %>%
    as.numeric()
}

for (i in pageSequence) {
  url <- sprintf("https://www.yelp.com/biz/sob-dallas?start=%d&sort_by=date_asc", i)
  
  page <- read_html(url)
  
  #collect all usernames from reivews
  usernames <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") %>%
    html_elements(xpath = ".//a[starts-with(@href, '/user_details')]") %>%
    html_text()
  
  #collect all locations from reivews
  locations <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") %>%
    html_elements(xpath = ".//span[@class=' css-qgunke']") %>%
    html_text() %>%
    .[.!="Location"] #remove location as list item
  
  #collect all comments from reivews
  comments <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = "(.//p[starts-with(@class, 'comment')])[1]") %>%
    html_text()
  
  #collect all ratings
  ratings <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = "(.//div[contains(@aria-label, 'star rating')])[1]") %>%
    html_attr("aria-label") %>%
    str_remove_all(" star rating") %>%
    as.numeric()
  
  #collect all dates
  the_dates <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = "(.//span[@class = ' css-chan6m'])[1]") %>%
    html_text()
  
  #collect the emotion information about the reviews
  extra_info <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = ".//button[@type='submit']") %>%
    html_text() %>%
    .[.!=""] %>%
    .[.!="Read more"]
  
  
  #assign the extra information by category
  useful <- extra_info_extract(extra_info, "Useful")
  funny <- extra_info_extract(extra_info, "Funny")
  cool <- extra_info_extract(extra_info, "Cool")
  
  #combine the objects into a list
  df_new <- list(username = usernames,
                 dates = the_dates,
                 location = locations,
                 rating = ratings,
                 comment = comments,
                 useful = useful,
                 funny = funny,
                 cool = cool)
  
  #convert list into a data frame
  df_new_table <- as.data.frame(df_new)
  
  #append the data frome to the df_final object
  df_final <- rbindlist(list(df_final, df_new_table))
  
  #random sleep time between pages so IP address isnt banned
  Sys.sleep(sample(c(15,25), 1))  
}

#write the extracted data into csv
write.csv(df_final, "Yelp Review Sample.csv", na = "")


