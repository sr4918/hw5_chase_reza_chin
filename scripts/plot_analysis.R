library(tidytext)
library(tidyverse)
library(ggplot2)
library(ggpubr)
setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML")
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
      #test<-head(A2.1_plots,100)
      test2<-plot_text[1:50,]
     
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
      write_csv(interesting_words, "Assignment 5/data/question_a2.csv")
      
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
      
      ggsave("Assignment 5/figures/fig_a2_5.png",figure)
      
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
      ggsave("hw4_reza_chase_tello/figures/question_a3_2.png",g3.2)
      