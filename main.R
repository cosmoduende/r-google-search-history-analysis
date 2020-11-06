# REQUIRED LIBRARIES
library(wordcloud)
library(lubridate)
library(rvest)
library(tm)
library(tidyverse)
library(plotly)

# READ DATA
fileHTML <- "Takeout/My Activity/Search/MyActivity.html"
mySearchFile <- read_html(fileHTML, encoding = "UTF-8")

# SCRAPPING SEARCH DATE AND TIME
dateSearch <- mySearchFile %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=<br>)(.*)(?<=PM|AM)") %>%
  mdy_hms()
dateSearch[1:5]

# SCRAPING SEARCH TEXT
textSearch <- mySearchFile %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
  str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
  str_extract(pattern = '(?<=\">)(.*)')
textSearch[1:5]

# SCRAPING SEARCH TYPE
searchType <- mySearchFile %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=mdl-typography--body-1\">)(.*)(?=<a)") %>% 
  str_extract(pattern = "(\\w+)(?=\\s)")
searchType[1:5]

# CREATE DATA FRAME USING SCRAPED DATA
searchedData <- tibble(timestamp = dateSearch,
                      date = as_date(dateSearch),
                      year = year(dateSearch),
                      month = month(dateSearch, label = TRUE),
                      day = weekdays(dateSearch),
                      hour = hour(dateSearch),
                      type = searchType,
                      search = textSearch)

searchedData$day <- factor(searchedData$day, levels = c("Sunday", "Monday", "Tuesday",
                                                      "Wednesday","Thursday", "Friday",
                                                      "Saturday"))
searchedData <- na.omit(searchedData)
head(searchedData)

# PLOT SEARCH VOLUME BY YEAR
searchByYear <- ggplot(searchedData, aes(year, fill=..count..)) +
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_bar(width=0.7)+
  labs(x= "Year", y= "Count") + 
  ggtitle("How much your search frequency has changed over time", "Search activity by year")
searchByYear
ggplotly()  

# PLOT SEARCH VOLUME BY MONTH
searchByMonth <- searchedData[(searchedData$year > 2007 & searchedData$year< 2021), ]
ggplot(searchByMonth, aes(year, fill=..count..)) + 
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_bar(aes(x = month, group = year)) +
  theme(axis.text.x = element_text(angle=90)) +
  facet_grid(.~year, scales="free") + 
  labs(x= "Year / Month", y= "Count") + 
  ggtitle("How much your search frequency has changed over time", "Month activity on detail")
ggplotly() 


# PLOT SEARCH VOLUME BY HOUR
seearchByHour <- ggplot(searchedData, aes(hour, fill=..count..)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_bar() + 
  labs(x= "Hour", y= "Count") + 
  ggtitle("What time of day do you have the highest frequency of searches?", "Hour activity on detail")
seearchByHour
ggplotly() 


# PLOT SEARCH VOLUME BY WEEKDAY
seearchByWeekD <- ggplot(searchedData, aes(day, fill=..count..)) + 
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_bar() +
  labs(x= "Day", y= "Count") + 
  ggtitle("What day of the week do you have the highest frequency of searches?", "Weekday activity on detail")
seearchByWeekD
ggplotly() 


# PLOT SEARCH VOLUME BY WEEKDAY AND TIME 
searchWdayTime <- ggplot(searchedData) + 
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_bar(aes(x = hour, group = day, fill=..count..) ) +
  labs(x= "Hour / Day", y= "Count") + 
  ggtitle("Relationship between day / time you have a higher frequency of searches", "Weekday/Time activity on detail") +
  facet_grid(.~day, scales = "free")
searchWdayTime
ggplotly() 


# CLEAN AND EXTRACT TEXT TO CREATE A TEXT CORPUS
lastTwoYears <- searchedData[(searchedData$year > 2007 & searchedData$year< 2010), ]

search <- tolower(lastTwoYears$search)
search <- gsub('(http|https)\\S+\\s*|(#|@)\\S+\\s*|\\n|\\"', " ", search)
search <- gsub("(.*.)\\.com(.*.)\\S+\\s|[^[:alnum:]]", " ", search)
search <- trimws(search)

textCorpus <-  Corpus(VectorSource(search))
textCorpus <- tm_map(textCorpus, content_transformer(removePunctuation))
textCorpus <- tm_map(textCorpus, content_transformer(removeNumbers))
stopwords <- c(stopwords("english"), "que", "com", "cómo", "como", "para", "con", "qué", "las", "los", "del", "can")
textCorpus <- tm_map(textCorpus, removeWords, stopwords)

searchTDM <- TermDocumentMatrix(textCorpus)
searchMatrix <- as.matrix(searchTDM)

# CREATE DATA FRAME WITH WORDS
arrange <- sort(rowSums(searchMatrix), decreasing = TRUE)
twNames <- names(arrange)
dataCloud <- data.frame(word = twNames, freq = arrange)

wordcloud(dataCloud$word, dataCloud$freq, min.freq = 40, scale = c(2 , 0.5), max.words = 100, colors=brewer.pal(9, "Paired"))
