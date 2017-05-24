consumer <- read.csv("CC copy.csv", stringsAsFactors = FALSE)

## checking out the structure of data frame
str(Consumer.C)

##number of rows and columns
nrow(Consumer.C)
ncol(Consumer.C)

## libraries to load in this enviornment

library(plyr)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(stringr)
library(syuzhet)
library(psych)
library(lubridate)
library(jsonlite)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(Amelia)
library(proxy)
library(sentimentr)
library(SnowballC)
library(lubridate)

install.packages("Rcpp",dependencies = T)
install.packages("RcppArmadillo",dependencies = T)
install.packages("BH",dependencies = T)
install.packages("tcltk", dependencies = T)
## since there is no value in sub-product for consumer protection bureau will delete
consumer$Sub.product <- NULL
consumer$Sub.issue <- NULL
## let check the missing values
missmap(Consumer.C)

## Code for Text Analysis

glimpse(Consumer.C)

library(DT)
## overview of the data 
consumer %>%
  select(Date.received, Product, Company, Issue, Sub.issue, Company.response.to.consumer)  %>%
  datatable(., options = list(pageLength = 10))


## now lets move to companies that recieved the highest number of complaints by issue 
 consumer %>% 
    group_by(Company) %>% 
    dplyr::summarize(comments = n()) %>%
    mutate(percent=round((comments/sum(comments)*100))) %>%
    arrange(desc(comments)) 
  
  
 ggplot(consumer, aes(x=Issue)) +
   geom_bar() +
   coord_flip() +
   ggtitle("Credit Cards - Complaint Issue") + 
   xlab("") + 
   ylab("N")
 
 ##
 by_company <- consumer %>% 
   group_by(Company) %>% select(Company) %>% 
   summarise(Count = n()) %>% arrange(desc(Count))
 
 head(by_company,10)
 
 ggplot(head(by_company, 10), aes(reorder(Company, -Count), Count, fill = Company)) + geom_bar(stat = "identity") + xlab("Product") + ylab("Number of Complaints") + theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 0.4, face = "bold"), plot.title = element_text(size = 20, face = "bold", vjust = 2),axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35),axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + theme(legend.position = "none")
 
comments <- Consumer.C$Consumer.complaint.narrative

## State  Lets take a look number of dispute by state


by_state <- consumer %>% group_by(State) %>% 
  select(State) %>% summarise(Count = n()) %>% arrange(desc(Count))



head(by_state, 10)
tail(by_state, 10)

ggplot(head(by_state, 10), aes(reorder(State, -Count), Count, fill = State)) + 
  geom_bar(stat = "identity") + xlab("State") + 
  ylab("Number of Complaints") + 
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 0.4, face = "bold"), plot.title = element_text(size = 20, face = "bold", vjust = 2),
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35),axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + theme(legend.position = "none" )

## Timely Response

table(consumer$Timely.response.)

prop.table(table(consumer$Timely.response.))

## consumer Dispute

head(consumer$Consumer.disputed., 10)

table(consumer$Consumer.disputed.)
prop.table(table(consumer$Consumer.disputed.))


## Submitted Via

table(consumer$Submitted.via)

prop.table(table(consumer$Submitted.via))

ggplot(consumer, aes(reorder(Submitted.via, -table(consumer$Submitted.via)[Submitted.via]), fill = Submitted.via)) + geom_bar() + xlab("Mode of Complaints Submission") + ylab("Number of Complaints") + scale_y_continuous(breaks = seq(0,350000,50000)) + theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 0.4, face = "bold"), plot.title = element_text(size = 20, face = "bold", vjust = 2),axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35),axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + theme(legend.position = "none")

## Now maybe we could look further to see how many companies responded 

writeLines(as.character(Consumer.C[[4]]))

ggplot(consumer[issue=="Charged fees or interest I didn't expect"], aes(x=company_response_to_consumer)) +
  geom_bar(fill="Identity") +
  coord_flip() +
  ggtitle("Company Response to Unexpected Fees") + xlab("") + ylab("N") +
  theme_solarized(base_size = 14)


## Time Taken to send the compalint to company

consumer$Date.received <- as.Date(consumer$Date.received, "%m/%d/%Y")

consumer$Date.sent.to.company <- as.Date(consumer$Date.sent.to.company, "%m/%d/%Y")

consumer$Date.sent.to.company <- difftime(consumer$Date.sent.to.company, consumer$Date.received , units = c("days"))

consumer$Date.sent.to.company <- as.numeric(consumer$Date.sent.to.company)

summary(consumer$Date.sent.to.company)

dim(consumer[consumer$Date.sent.to.company > 100, ])

dim(consumer[consumer$Date.sent.to.company > 500, ])

dim(consumer[consumer$Date.sent.to.company > 900, ])

## create new variable month and year

# Extract month and year from the date compalint recieved.
# distribution of complaints across different months and years

consumer$Month <- month(ymd(consumer$Date.received))
consumer$Year <- year(ymd(consumer$Date.received))
consumer$Month <- factor(consumer$Month)
consumer$Year <- factor(consumer$Year)

table(consumer$Month)
table(consumer$Year)


ggplot(consumer, aes(Month, fill = Month)) + geom_bar() + theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35),axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + theme(legend.position = "none")

ggplot(consumer, aes(Year, fill = Year)) + geom_bar() + scale_y_continuous(breaks = seq(0,350000,50000)) + theme(axis.text.x = element_text(angle = 50, size = 10, vjust = 0.4, face = "bold"), plot.title = element_text(size = 20, face = "bold", vjust = 2),axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35),axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + theme(legend.position = "none")
