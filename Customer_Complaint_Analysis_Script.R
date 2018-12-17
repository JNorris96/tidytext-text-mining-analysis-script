##### Analyze any Sub-Category in Customer Complaint Data ################################################################################
##### Taylor Axelson & Jacob Norris ######################################################################################################
##### 12/11/2018 #########################################################################################################################

#### Edit This Variable to Change Sub-Category - DONT FORGET TO CREATE A TOP 10 WORD LIST AT LINE 222 ####################################

subcategory <<- "HIGHBILL"

#### LIBRARIES  and WORKING DIRECTORY - This section should be ran every time the script runs. ###########################################
setwd("V:/Customer Support Services/zz_Customer Service Analyst/Cornell Project") #Be sure to put the data file you want to analyze in this directory. Make sure it is a CSV!

library(babynames)
library(dplyr)
library(tidytext)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(reshape2)
library(lubridate)
library(zoo)
library(ggrepel)
library(rmarkdown)


#### Create Global Word Block List - Adding names to list of words to block ##############################################################
stop_names <- babynames::babynames                      #Create data frame to block names
stop_names <- stop_names[, -c(1:2, 4:5)]                #Delete all columns except "Name" 
colnames(stop_names)[1] <- "CaseComments"               #Rename "Name" column to "CaseComments" column
stop_names$CaseComments <- tolower(stop_names$CaseComments) #Make "Name" column values lowercase

data(stop_words)                                        #Pull "stop_words" from tidytext library
stop_words$lexicon <- NULL                              #Nullify "lexicon" column in "stop_words" 
colnames(stop_words)[1] <- "CaseComments"               #Rename "Word" column to "CaseComments" column

master_stop <<- unique(rbind(stop_words, stop_names))   #Combine "stop_words" and "stop_names" on "CaseComments" column and delete duplicate words
rm(stop_names, stop_words)                              #Delete unnecessary variables 



#### LOAD GLOBAL DATASET, BREAK COMMENTS INTO WORD TOKENS, & BLOCK WITH MASTER_STOP ######################################################
#### This section needs a CSV in which the text data that needs to be analyzed is labeled "CaseComments"
feedback <- read_csv("Copy of CCB Customer Feedback.csv")   # Read in the data file as a CSV

feedback <<- feedback %>%                                   # Separate each word by a space and put them on their own line
  unnest_tokens(CaseComments, "CaseComments", token = stringr::str_split, pattern = " ") %>%
  anti_join(master_stop)                                    # Filter out unnnecessary words

# bestcall_BB <- read_csv("Best Practices Call BB.csv")
# 
# bestcall_BB <<- bestcall_BB %>%
#   unnest_tokens(CaseComments, "CaseComments", token = stringr::str_split, pattern = " ")%>%
#   anti_join(master_stop)


#### CREATE BIGRAM ####################################################################################################################### 
#### This function breaks down the feedback received into word pairs. This function creates the bigrams used for the other functions.
bigram_creator <- function(x, y)
{
  x_bigram_feedback <- y %>%		
		  filter(`Sub Topic 2` == x)                                        # Filter feedback by subcategory
  
	x_bigram_feedback <- x_bigram_feedback %>%
  		unnest_tokens(bigram, 'CaseComments', token = "ngrams", n=3)      # Break case comments into word pairs 

	x_bigram_feedback <- x_bigram_feedback %>%
		  separate(bigram, c("word1", "word2", sep=" "))                    # Make bigram into 2 word columns 

	x_bigram_feedback <- x_bigram_feedback %>%                            # Filter unnecessary words by master_stop
  		filter(!word1 %in% master_stop$CaseComments) %>%
  		filter(!word2 %in% master_stop$CaseComments)

	x_bigram_feedback <- x_bigram_feedback %>%                            # Combine both word columns into one column called Bigram
  		unite(bigram, word1, word2, sep=" ")
	
	x_bigram_feedback <- as.data.frame(x_bigram_feedback)                 # Cast this variable as a data frame
	
	return(x_bigram_feedback)                                             

}

subcategory_bigram_feedback <<- bigram_creator(subcategory, feedback)   # Assign the return of the function as a global variable


#### CREATE SENTIMENT FEEDBACK ###########################################################################################################
#### This function breaks down the feedback received into words and sentiments associated with them using the NRC sentiment lexicon. 
sentiment_creator <- function(x, y)
{
	sentiment_feedback <- y %>%
  		unnest_tokens(word, 'CaseComments')                               # Separate CaseComments into a new column called "word"

	sentiment_feedback <- sentiment_feedback %>%
		mutate(date = mdy(`Date Opened`)) %>% 
		mutate_at(vars(date), funs(year,month,day))                         # Use year month date to make `Date Opened`

	sentiment_feedback$Month_Yr <- as.yearmon(paste(sentiment_feedback$year, sentiment_feedback$month, sep = "-"))  # Use year and month 

	sentiment_feedback <- sentiment_feedback %>%
  		filter(`Sub Topic 2` == x)                                        # Filter feedback by subcategory

	return(x_NRC_sentiment_feedback <- sentiment_feedback %>%
  		inner_join(get_sentiments("nrc")))                                # Add NRC sentiment lexicon from TidyText package to correlating words

}

subcategory_NRC_sentiment_feedback <<- sentiment_creator(subcategory, feedback)   # Assign the return of the function as a global variable


#### Top 15 BIGRAM COUNTER - Creates a plot that counts how many times the top 15 bigrams appear for chosen sub-category #################
bigram_count_plot_creator <- function(x, y)
{
  x_bigram_feedback_sorted <- x %>%
    count(bigram, sort = TRUE)                                          # Count the bigrams 
  
  return(x_top_bigrams_plot <- ggplot(x_bigram_feedback_sorted[1:15, ], aes(x = reorder(bigram, +n), y = n, label = n)) + 
           geom_bar(fill = "#003366", stat="identity") +
           geom_label(size = 3) +
           coord_flip() +
           labs(title = "Frequent Word Pairs", subtitle = paste("in", y, sep = " "), x = ("Bigram"), y = ("Count")) + 
           theme_classic())       # This ggplot makes a bar graph of the bigrams with the highest count and labels the bars with the count 
  
}

subcategory_top_bigrams_plot <<- bigram_count_plot_creator(subcategory_bigram_feedback, subcategory)    # Assign the return of the function as a global variable

subcategory_top_bigrams_plot  # Display the plot


#### FREQUENT LOCATION CALLS - Creates a plot that shows the cities with most call-ins ###################################################
top_locations_creator <- function(x, y)
{
  x <- data.frame(table(x$City))  # Cast the table made out of the feedback's city columns into a data frame
  
  return(top_n(x, n = 15, Freq) %>%
           ggplot(aes(x = reorder(Var1, +Freq), y = Freq, label = Freq)) +
           geom_bar(stat = "identity", fill = "#003366")+
           geom_label(size = 3)+
           labs(title = paste(y, "Complaints", sep = " "), subtitle = "by City") +
           xlab("City")+
           ylab("Count")+
           theme_classic()+
           coord_flip())      # This ggplot makes a bar graph that counts the citys with the most calls and labels the bars with the count
  
}

subcategory_top_locations_plot <<- top_locations_creator(subcategory_bigram_feedback, subcategory)    # Assign the return of the function as a global variable

subcategory_top_locations_plot  # Display the plot


#### FILTER, REMOVE COMMON WORDS, & COUNT WORDS ##########################################################################################
#### This function creates the raw word count to be used by the word count plot creator. You can view the raw word count by clicking on
#### the variable name to the right in the global environment tab. You can also edit the words in quotes to filter them out.
wordcount_creator <- function(x, y)
{
  x_wordcount <- y %>%
    filter(`Sub Topic 2` == x)                          # Filter feedback by subcategory                                          
  
  x_wordcount <- x_wordcount %>%
    filter(!str_detect(CaseComments, "[0-9]"),
           CaseComments!= "",
           CaseComments!= "call",
           CaseComments!= "billed",
           CaseComments!= "bills",
           CaseComments!= "budg",
           CaseComments!= "called",
           CaseComments!= "st",
           CaseComments!= "&",
           CaseComments!= "-",
           CaseComments!= "acct",
           CaseComments!= "account",
           CaseComments!= "adv",
           CaseComments!= "ave",
           CaseComments!= "callback",
           CaseComments!= "stated")                     # This function filters additional words out of the data set                 
  
  return(x_wordcount <- x_wordcount %>%
           count(CaseComments, sort=TRUE))              # Counts the words in CaseComments column
  
}

subcategory_wordcount <<- wordcount_creator(subcategory, feedback)                # Assign the return of the function as a global variable 


##### This function to be used with text files with greater than 1000 words ##############################################################
wordcount_largeplot_creator <- function(x, y)
{
  return(x %>%
           filter(n>200) %>%
           mutate(CaseComments = reorder(CaseComments, n)) %>%
           ggplot(aes(CaseComments,n)) +
           geom_col(fill = "#003366")+
           labs(title = "Most Used Words", subtitle = paste("in", y, sep = " "), x = ("Word"), y = ("Count"))+
           theme_classic()+
           coord_flip())
}


##### This function to be used with text files with less than 1000 words #################################################################
wordcount_smallplot_creator <- function(x, y)
{
  return(x %>%
           filter(n>2) %>%
           mutate(CaseComments = reorder(CaseComments, n)) %>%
           ggplot(aes(CaseComments,n, label=n)) +
           geom_col(fill = "#003366")+
           geom_label(size=3)+
           labs(title = "Most Used Words", subtitle = paste("in", y, sep = " "), x = ("Word"), y = ("Count"))+
           theme_classic()+
           coord_flip())
}


##### Edit method call, SPECIFY IF USING "wordcount_largeplot_creator" or "wordcount_smallplot_creator" ##################################
subcategory_wordcount_plot <<- wordcount_largeplot_creator(subcategory_wordcount, subcategory)  # Assign the return of the function as a global variable

subcategory_wordcount_plot  # Display the plot


#### TOP TEN WORDS #######################################################################################################################
#### Top 10 - Create a custom word list of ten words using the template below. Only change the words in the quotations.
subcategory_wordcount_t10 <<- subcategory_wordcount %>%
  filter(!str_detect(CaseComments, "[0-9]"), 
           CaseComments == "balance" |
           CaseComments == "amount" |
           CaseComments == "iub" |
           CaseComments == "confused" |
           CaseComments == "upset" |
           CaseComments == "stop"|
           CaseComments == "budget" |
           CaseComments == "address" | 
           CaseComments == "gas" |
           CaseComments == "total")


#### COUNT WORDS, PLOT WORDCOUNT - Top 10 ################################################################################################
#### This function uses the custom word list above and counts how many times those words appear in the data file. Be sure to specify
#### which Top 10 word list you want to use 
wordcount_top10_creator <- function(x, y)
{
  return(x %>%
           mutate(CaseComments = reorder(CaseComments, n)) %>%        # Takes the CaseComments and sorts by n (Count)
           ggplot(aes(CaseComments, n, label = n)) +
           geom_col(fill = "#003366")+
           geom_label(size = 3)+
           labs(title = "Custom Top 10 Words Count", subtitle = paste("in", y, sep = " "), x = ("Word"), y = ("Count"))+ 
           theme_classic()+ 
           coord_flip())
  
}

subcategory_wordcount_plot_t10 <<- wordcount_top10_creator(subcategory_wordcount_t10, subcategory)  # Assign the return of the function as a global variable

subcategory_wordcount_plot_t10  # Display the plot


#### PLOT NRC SENTIMENT CHART ############################################################################################################
nrc_sentiment_spectrum_creator <- function(x, y)
{
  x_NRC_spectrum <- x %>%
    group_by(sentiment = factor(sentiment, levels =c("fear","anger","disgust","sadness",
                                                     "negative","anticipation","surprise",
                                                     "positive","joy","trust"))) %>%           # This custom sorts the sentiment groups, the order was recommended to script authors by an anthropology major
    count(word, sort = TRUE) %>%
    arrange(desc(n)) %>%
    slice(seq_len(5)) %>%
    ungroup()
  
  return(x_NRC_spectrum %>%
           ggplot(aes(word, 1, label = word, fill = sentiment)) +
           scale_fill_manual( values = c("positive"="#bef4d0", 
                                         "trust" = "#38c477", 
                                         "negative" = "#fdebe8", 
                                         "anticipation" = "#e9fbef",
                                         "sadness" = "#f9c4b9",
                                         "anger" = "#f28268",
                                         "joy" = "#87eba8",
                                         "fear" = "#f2543d",
                                         "surprise" = "#e9fbef",
                                         "disgust" = "#f59d8a" ))+
           geom_point(color = "transparent")+ 
           geom_label_repel(force = 1, nudge_y = .5,
                            direction = "y" ,
                            box.padding = 0.08,
                            segment.color = "transparent",
                            color = "black",
                            size= 3)+
           theme_classic() +
           theme(axis.text.y = element_blank(), 
                 axis.text.x = element_blank(),
                 axis.title.x = element_text(size = 6),
                 panel.grid = element_blank(), 
                 panel.background = element_blank(),
                 panel.border = element_rect("lightgray", 
                                             fill = NA),
                 strip.text.x = element_text(size = 9),
                 axis.ticks.x=element_blank()) +
           facet_grid(~ sentiment) +
           labs(title = "Sentiment Spectrum", subtitle = paste("in", y, sep = " "), x = NULL, y = NULL)+
           coord_flip()+
           guides(fill=FALSE))                # Sort the sentiment groups and organizes words into sentiment groups and colors their boxes in a red-green gradient
  
}

subcategory_NRC_spectrum <<- nrc_sentiment_spectrum_creator(subcategory_NRC_sentiment_feedback, subcategory)  # Assign the return of the function as a global variable

subcategory_NRC_spectrum  # Display the plot


#### PLOT NRC SENTIMENT ##################################################################################################################
sentiments_group_plot_creator <- function(x, y)
{
  return(x_sentiments_group_plot <- x %>% 
           group_by(sentiment) %>%
           summarise(word_count = n()) %>%
           ungroup() %>%
           mutate(sentiment = reorder(sentiment, word_count)) %>%
           ggplot(aes(sentiment, word_count, fill= - word_count, label = word_count)) +
           geom_col() +
           geom_label(size = 3, fill = "white")+
           theme_classic() +
           coord_flip() +
           labs(title = "Sentiment Group Distribution", subtitle = paste("in", y, sep = " "), x="Sentiment Group", y="Word Count") +
           guides(fill=FALSE))              
}

subcategory_sentiments_group_plot <<- sentiments_group_plot_creator(subcategory_NRC_sentiment_feedback, subcategory)  # Assign the return of the function as a global variable

subcategory_sentiments_group_plot  # Display the plot


#### FILTER - NEG SENTIMENT ##############################################################################################################
only_negative_sentiments <- function(x)
{
  return(x_only_negative_sentiments <- x %>%
           filter(sentiment == "negative" |
                    sentiment == "sadness" |
                    sentiment == "anticipation"|
                    sentiment == "anger"|
                    sentiment == "disgust"))       # Filters out postive sentiments for the next two plots
  
}

subcategory_only_negative_sentiments <<- only_negative_sentiments(subcategory_NRC_sentiment_feedback)  # Assign the return of the function as a global variable


#### PLOT NRC SENTIMENT - TIME SERIES ####################################################################################################
negative_sentiments_over_time_plot_creator <- function(x, y)
{
  return(subcategory_Negative_Sentiments_over_time <- x %>% 
    group_by(Month_Yr) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    ggplot(aes(Month_Yr, word_count)) +
    geom_line(size= 1, color  = "#003366") +
    geom_smooth() +
    theme_classic() +
    labs(title = "Negative Sentiment Word Count Over Time", subtitle = paste("in", y, sep = " "), x=NULL, y= "Word Count") +
    guides(fill=FALSE))
    
}

subcategory_negative_sentiments_over_time <<- negative_sentiments_over_time_plot_creator(subcategory_only_negative_sentiments, subcategory)  # Assign the return of the function as a global variable

subcategory_negative_sentiments_over_time  # Display the plot


#### PLOT NRC SENTIMENT - NEG - YEAR #####################################################################################################
negative_sentiments_per_year_plot_creator <- function(x, y)
{
  return(subcategory_Negative_Sentiments_per_year_plot <- x %>%
    group_by(year) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(year = reorder(year, word_count)) %>%
    ggplot(aes(year, word_count, fill= - word_count, label=word_count)) +
    geom_col() +
    geom_label(size=3, fill="white")+
    theme_classic() +
    coord_flip() +
    labs(title = "Negative Sentiment Words per Year", subtitle = paste("in", y, sep = " "), x="Year", y= "Count") +
    guides(fill=FALSE))
    
}

subcategory_negative_sentiments_per_year_plot <<- negative_sentiments_per_year_plot_creator(subcategory_only_negative_sentiments, subcategory)  # Assign the return of the function as a global variable

subcategory_negative_sentiments_per_year_plot  # Display the plot

