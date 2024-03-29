library(tidytext)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(ROCR)
setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Assignment 5")
#A2
        #A2.1 Set the seed to 2048 and read in the plots and titles files with readr::read_lines(). Create a
      #plot_text tibble with three columns:
       # . title: the title of a story.
      #. text: the plot text for the corresponding title.
      #. story_number: a unique index/identifier for the particular story.
      #Note: you can keep track of when a story ends by looking for "<EOS>" in the text. You may find the
      #cumsum() command helpful to count these markers when creating the story_number column.
      
      set.seed(2048)
      A2.1_plots<-readr::read_lines("data_hw5/plots")
      A2.1_titles<-readr::read_lines("data_hw5/titles")

      plots <- readr::read_lines("data_hw5/plots") %>% paste(sep=" ", collapse="")
      plots <- unlist(strsplit(plots, split = "<EOS>"))
      
      titles <- readr::read_lines("data_hw5/titles")
      plot_text <- tibble::tibble(title = titles,
                                  text = plots,
                                  story_number = cumsum(!duplicated(titles[])))

      #A2.2 Use the unnest_tokens() command from the tidytext package to split the text column into separate
      # words so that each row corresponds to a particular word in a particular plot; call the resulting tibble
      # plot_words.
      
      plot_words <-plot_text %>% unnest_tokens(word, text)
 
      #Then, add a word_position column to plot_words that, for each word in each plot,
      #divides the location of the word within the plot by the corresponding plot length (in words). For example,
      #if a plot contains 210 words, and is therefore unnested into 210 rows, the value of word_position for
      #the first word is 1/210, and value of word_position for the third word is 3/210, etc.
      
      plot_words<- plot_words %>% group_by(story_number)%>%mutate(word_position = 1:n()/n())
      
      #A2.3
      #Using plot_words, create a tibble called median_word_positions that stores, for each unique word
      #that appears in plot_words:
       # . word: the unique word;
      #. median_position: the median position of the word (across all plots); and
      #. number: the total number of occurences of the word (across all plots).
      #Subset median_word_positions to only words that occured at least 2500 times across the plots.

      median_word_positions<-plot_words %>%group_by(word) %>%summarize(number = n(),median_position = median(word_position))
      median_word_positions<-filter(median_word_positions, number>=2500)
      
      #A2.4
      #Using median_word_positions from the previous step, create a tibble called interesting_words that
      #stores the words with the 10 highest median positions, and the words with the 10 lowest median positions.
      #Your tibble should have a row for each of 20 words and the same columns (word, median_position, and
      #number). Sort this tibble in ascending order by median position, and save it to data/question_a2.csv
      #within your submission directory.
      
      interesting_words<-median_word_positions%>%arrange(median_position)%>%slice(c(1:10))
      interesting_words<-rbind(interesting_words, median_word_positions%>%arrange(median_position)%>%filter(row_number()>n()-10))
      interesting_words<-interesting_words%>%arrange(median_position)
      write_csv(interesting_words, "hw5_chase_reza_chin/data/question_a2.csv")
      
      #A2.5
      #Use the interesting_words tibble to create one figure with two bar graph subfigures. In the first bar
      #graph, words should appear on the x-axis, and median position on the y-axis, in descending order. In
      #the second bar graph, words should appear on the x-axis, and number of occurrences on the y-axis,
      #in descending order. Save this figure as figures/fig_a2_5.png within your submission directory.
      #Describe your results in a sentence or two in your writeup.

      interesting_words<-interesting_words%>%arrange(-median_position)
      A2.5_1g<-ggplot(data=interesting_words, aes(x = reorder(word, -median_position), y=median_position)) +geom_bar(fill = "violet",stat='identity')  + 
      labs(title="Median Positions of Interesting Words", x="Word", y="Median Position")  +theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      interesting_words<-interesting_words%>%arrange(number)
      A2.5_2g<-ggplot(data=interesting_words, aes(x = reorder(word, -number), y=number)) +geom_bar(fill = "orange",stat='identity')  + 
      labs(title="Frequency of Interesting Words", x="Word", y="Frequency")  +theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      figure <- ggarrange(A2.5_1g, A2.5_2g,
                          ncol = 2, nrow = 1)
      
      ggsave("hw5_chase_reza_chin/figures/fig_a2_5.png",figure)
      
#A3     
      #A3.1.
      #Returning to plot_words from Question A2.2, create a tibble called word_decile_counts. Each row
      #in this tibble should include the count of a given word-decile combination, across all plots. There should
      #be three columns, word, decile (an integer between 1 and 10), and n. For example, if word W occurs
      #five times in three plots, in the first decile of plot A, the first and second decile of plot B, and twice in
      #the third decile of plot C, then word_decile_counts would have three rows corresponding to word W.
      #The first row would have decile=1 and n=2, the second row would have decile=2 and n=1, and the
      #third row would have decile=3 and n=2.
      
      word_decile_counts<-plot_words %>% 
                          mutate(decile = ceiling(word_position*10))%>% 
                          group_by(word,decile)%>%
                          summarize(n = n())
     
      #A3.2
      #Perform an inner join of interesting_words (from Question A2.4) and word_decile_counts, keeping
      #track of whether the words in interesting_words are in the group with highest median position, or
      #the group with lowest median position. Use this tibble to create a graph that has the decile on the
      #x-axis, and the normalized frequency of the word on the y-axis (normalized by the total number of
      #occurrences of the word). This graph should have 10 lines; the lines for the 10 words with the largest
      #median positions should all be one color (e.g., blue), and the lines for the 10 words with the smallest
      #median positions should all be another color (e.g. red). In your writeup, describe the shape of these
      #distributions. Save this figure as figures/fig_a3.png within your submission directory.
      
      
      interesting_words$class<-ifelse(interesting_words$median_position<0.5, "low", "high")
      A3.2<-inner_join(interesting_words, word_decile_counts)
      A3.2$normalized_freq<-A3.2$n/A3.2$number
     
      A3.2<-A3.2%>%group_by(word, class)
      g3.2<-ggplot(data=A3.2, aes(x=decile, y=normalized_freq, group = word, color = class)) +geom_line()  + 
        labs(title="decile against normalized freq", x="decile", y="normalized freq")
      ggsave("hw5_chase_reza_chin/figures/fig_a3.png",g3.2)
      
      
#QB
      #B1.2
      #Starting with the plot_words tibble from Question A2.2, add a new column called sentiment, assigning
      #to each word the corresponding sentiment value from the bing lexicon. What proportion of words have
      #sentiment values?
      
      plot_words<-plot_words%>%left_join(get_sentiments("bing"))
      proportion<-sum(!is.na(plot_words$sentiment))/nrow(plot_words)
      #B1.3. Add a decile column to plot_words, as in Question A3.1, which records the decile (an integer between
      #1 and 10) of the plot position in which each word appears. Recode positive sentiment values as 1,
      #and negative sentiment values as 0; this will allow you to compute the average sentiment for each
      #decile of each plot. Record these average sentiments in a tibble called plots, which has four columns:
      #story_number, title, decile, and mean_sentiment, a number between 0 and 1. You should ignore
      #NA values when computing mean_sentiment for each decile of each plot.
      
      plot_words$decile<-ceiling(plot_words$word_position*10)
      plot_words <-plot_words%>%mutate(sentiment2 = case_when(sentiment =="positive"~ 1, sentiment=="negative" ~0))
      
      plots<-plot_words%>%group_by(story_number,title,decile)%>%summarize(mean_sentiment = mean(sentiment2,na.rm =T))
      
      #QB2.1
      #For each plot in plots, add a column called ending that records if the 10th decile of the plot has
      #mean_sentiment greater than or equal to 0.5 (we'll say this was a happy ending, and put the value in
      #ending as 1), or less than 0.5 (we'll say this was a sad ending, and put the value in ending as 0). Drop
      #all plots that have a missing value for ending, and that have fewer than 10 deciles. How many plots do
      #you lose?
      
      
      plots2<-plots%>% group_by(story_number)%>%filter(decile==10)%>%mutate(ending = ifelse(mean_sentiment>=0.5,1,0))
      plots<-left_join(plots,plots2)
      lose<-filter(plots, decile ==10 & is.na(ending))
      lose_plots<-sum(is.na(lose$ending))
      # 31626
      plots<-plots %>%
        group_by(story_number) %>%
        filter(!any(decile==10 & is.na(ending)))
     
      #B2.2.
      #A fair number of plot-deciles in the plots tibble still have missing mean_sentiment values (for deciles
      #1-9). We'll deal with this using imputation. For each missing mean_sentiment value in deciles 1-9,
      #replace it with the average sentiment from the other deciles in the same plot, ignoring NA values. For
      #example, if plot P has mean_sentiment 0.5 for deciles 1 and 2, mean_sentiment 0 for deciles 3-7, and a
      #missing mean_sentiment value for deciles 8 and 9, then you should impute a mean_sentiment value of
      #1/7 for deciles 8 and 9. After this imputation, drop any plots that still have missing mean_sentiment
      #values in any decile.1
     
      plots<-plots %>%group_by(story_number,title) %>% 
        mutate(mean_sentiment=ifelse(is.na(mean_sentiment),mean(mean_sentiment[1:(n()-1)],na.rm=TRUE),mean_sentiment))
      sum(is.na(plots$mean_sentiment))
      
      plots<-plots %>%
        group_by(story_number) %>%
        filter(!any(is.na(mean_sentiment)))
      
  #B2.3
      #3. At this point, plots should have five columns: story_number, title, decile, mean_sentiment, and
      #ending. We would like to reshape this into a dataset where each row represents exactly one plot, and
      #we can use the sentiment values from the first nine deciles to predict the ending. Use dplyr::spread
      #to achieve this, and call the resulting tibble modeling_data. This tibble should have 11 columns:
      #  story_number, decile_1, decile_2, . . . , decile_9, and ending, where the value in each decile_j
      #column is the mean sentiment for that decile and that story. Then, randomly split modeling_data into
      #an 80% train set and a 20% test set. All the words for a given plot should be in either the training or
      #testing set; no plot should be divided across the two sets.
      
     #ASK: ending should be for every row not every plot as said earlier
      plots<-plots%>%group_by(story_number)%>%mutate(ending = ending[n()])
      modeling_data<-spread(plots, decile, mean_sentiment)
      
      #move ending to last columns
      modeling_data<-modeling_data%>%select(-ending, ending)
      names(modeling_data)<-c("story_number", "title", "decile_1","decile_2","decile_3","decile_4","decile_5","decile_6","decile_7","decile_8","decile_9","decile_10","ending")
      #drop decile 10
      modeling_data<-select(modeling_data,-decile_10)
      
      
      #train and test sets should have complete plots not repeated in the two sets using replace = FALSE
      
      index<-sample(1:nrow(modeling_data),.8*nrow(modeling_data), replace = F)
      train_data <- modeling_data[index,]
      test_data <- modeling_data[-index,]
      
      #Question B3. Fit a logistic regression. 
      #1. Fit a logistic regression model using the training data you created in the previous step, predicting ending
      #as a function of the average sentiments of the first nine deciles (make sure not to use story_number as 
      #a predictor).
      
      
      #ASK do we have to take AVERAGE of first nine deciles??
      modelB3.1 <- glm(ending~decile_1+decile_2+decile_3+decile_4+decile_5+decile_6+decile_7+decile_8+decile_9              
                       , family = binomial, data = train_data)
      
      pred_B3.1 <- predict(modelB3.1, test_data, type = "response")
      ROCR_B3.1 <- prediction(pred_B3.1, test_data$ending)
      auc_ROCR_B3.1 <- performance(ROCR_B3.1, measure = "auc")
      auc_B3.1 <- auc_ROCR_B3.1@y.values[[1]]
     
      
      #2. Report the AUC score of your fitted model on your test set in your writeup.
      auc_B3.1
      #0.6079
      
#B4   
      #B4.1. Take the plot from the 2018 movie A Star is Born, and hardcode it into your script. Process the data
      #so that we can obtain predictions for this story (i.e. unnest the tokens, join with the bing sentiment
      #lexicon, and calculate the average sentiment for each decile). Note: this question requires you to read
      #the plot of this movie, which contains spoilers!

      plot_new<-c("Jackson Jack Maine, a famous country rock singer privately battling an alcohol and drug addiction, plays a concert. His main support is Bobby, his manager and older half-brother. After a show, Jack, searching for his next drink, visits a drag bar where he witnesses a tribute performance to Edith Piaf, by Ally, a waitress and singer-songwriter. Jack is amazed by her performance, and they spend the night talking to each other, where Ally discloses to him her unsuccessful efforts in pursuing a professional music career. Ally shares with Jack some lyrics she has been working on, and Jack tells Ally she is a talented songwriter and should perform her own material.Jack invites Ally to his next show. Despite her initial refusal she attends and, with Jack's encouragement, sings Shallow on stage with him. Jack invites Ally to go on tour with him, and they form a romantic relationship. In Arizona, Ally and Jack visit the ranch where Jack grew up and where his father is buried. That was the time when he discovered that Bobby sold the land and it was converted to a wind farm. Angered at his betrayal, Jack punches Bobby, who subsequently quits as his manager. Before doing so, Bobby reveals that he did inform Jack about the sale, but Jack was too inebriated to notice.While on tour Ally meets Rez, a record producer who offers her a contract. Although visibly bothered, Jack still supports her decision. Rez refocuses Ally away from country music and towards pop. Jack misses one of Ally's performances after he passes out drunk in public; he recovers at the home of his best friend George Noodles Stone, and later makes up with Ally. There he proposes to Ally with an impromptu ring made from a loop of guitar string, and they are married that same day at a church ministered by a relative of Noodles.
      During Ally's performance on Saturday Night Live, Bobby reconciles with Jack. Later, Jack and Ally fight after he drunkenly voices his disapproval of Ally's new image and music, which is nominated for three Grammy Awards. At the Grammys, a visibly intoxicated Jack performs in a tribute to Roy Orbison, and Ally wins the Best New Artist award. When she goes up on stage to receive her award, Jack staggers up to her, where he publicly wets himself and passes out. Ally's father, Lorenzo, berates a semi-conscious Jack, while Ally attempts to help Jack sober up. Jack joins a rehabilitation program shortly thereafter. Jack recovers in rehab for about two months, where he discloses to his counselor that he attempted suicide when he was 13 years old. He also mentions that he has tinnitus, which has been getting worse.Jack tearfully apologizes to Ally for his behavior. While returning home, he admits to Bobby that it was him he idolized and not their father. Ally asks to bring Jack to perform with her European tour; Rez refuses, prompting Ally to cancel the remainder of the tour so she can care for Jack. Later, while Ally is out of the room, Rez confronts Jack, accusing him of being washed-up and of holding Ally back. That evening, Ally lies to Jack, and tells him that her record label has cancelled her tour so she can focus on her second album. Jack promises that he will come to her concert that night, but after Ally leaves, he hangs himself in their garage. Ally, grief-stricken and inconsolable after Jack's suicide, is visited by Bobby, who explains that the suicide was Jack's own decision and it was not her fault. The closing scenes reveal a flashback of Jack working on a song about his love for Ally which he had not finished. Ally sings this song as a tribute to Jack, introducing herself for the first time as Ally Maine. The film ends with a close-up of Ally looking up to the heavens, before the screen cuts to black.")
      
      plot_new_text <- tibble::tibble(title = "A Star is born",
                                  text = plot_new,
                                  story_number = 1000000)
      
      
      plot_new_words <-plot_new_text %>% unnest_tokens(word, text)
      plot_new_words<- plot_new_words %>% mutate(word_position = 1:n()/n())
      
     
      plot_new_words<-plot_new_words%>%left_join(get_sentiments("bing"))
     
      new_word_decile_counts<-plot_new_words %>% 
        mutate(decile = ceiling(word_position*10))
      new_word_decile_counts<-new_word_decile_counts%>%group_by()
 # 2. Using the classifier from Question B3, predict whether this story has a happy ending or a sad ending
      #using the mean sentiment of the first nine deciles. Report the predicted probability of a happy ending.
      #Upon reading the end of the plot, do you agree with the classifier?
      
       # 3. Rewrite the last decile of the plot to be "happy" (i.e., to have a mean sentiment greater than 0.5). Your
      #rewritten ending should be coherent English!
       # 1Note that mean imputation has a number of significant drawbacks; don't blindly use it whenever you are dealing with
      #missing data!
      
       # 4
      #4. Does the rewritten ending need to be coherent English in order to be "happy" according to our definition?
       # That is, do the written sentences have to actually make sense?
      
      
        #5. If you had unlimited time, can you think of ways to more accurately capture whether the ending to a
      #story is happy or sad? Write a few sentences in your writeup.