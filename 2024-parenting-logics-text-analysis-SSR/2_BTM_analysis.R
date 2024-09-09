# Replication Code for Biterm Topic Modeling in:
# What’s a Parent to Do? Measuring Cultural Logics of Parenting with Computational Text Analysis
# Last updated: September 9, 2024

# Set directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(stringr) 
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tm)
library(topicmodels)
library(haven)
library(LDAvis)
library(BTM)
library(textplot)
library(ggraph)
library(igraph)
library(concaveman)
library(quanteda)
library(SnowballC) # For stemming

alldata <- read_dta("alldata.dta") 


# IMPORTANT!!!
# Choose the Vignette number
# For the analysis in the paper, code needs to be run for Vignette's 1, 4, and 5
# to replicate the paper results.
vignum <- 5


## ======= Data preparation =========
# 
# ## Cleaning variables from Stata
# alldata$edu1 <-  alldata$edu
# alldata$edu[alldata$edu == 0] <- "no ba"
# alldata$edu[alldata$edu == 1] <- "ba"
# 
# alldata$inc1 <-  alldata$inc
# alldata$inc[alldata$inc == 0] <- "low inc"
# alldata$inc[alldata$inc == 1] <- "high inc"
# 
# alldata$race <- alldata$ppethm
# alldata$race[alldata$race == 1] <- "white"
# alldata$race[alldata$race == 2] <- "black"
# alldata$race[alldata$race == 3] <- "other nh"
# alldata$race[alldata$race == 4] <- "hispanic"
# alldata$race[alldata$race == 5] <- "2+ race nh"

## Tidying the text and pre-processing

# data cleaning of some words that appear in two forms
alldata$q1 <- gsub("extra curricular", "extracurricular", alldata$q1 )
alldata$q1 <- gsub("\\<dr\\>", "doctor", alldata$q1 )


# select which vignette to examine (based on the number chosen above)
tidy_Q1 <- alldata %>%
  filter(vig == vignum) %>%
  select(caseid,q1) %>% # , inc, edu, race) %>%
  unnest_tokens("word", q1)

#   select(caseid,Q1,inc,edu, race) %>%

## ============== Pre-processing =======

# remove stop words
data("stop_words")
tidy_Q1<-tidy_Q1 %>%
  anti_join(stop_words)

# remove whitespace
tidy_Q1$word <- gsub("\\s+","",tidy_Q1$word)

# stemming
tidy_Q1_stemmed<-tidy_Q1 %>%
  mutate_at("word", funs(wordStem((.), language="en")))
tidy_Q1 <- tidy_Q1_stemmed


## lemmatize (to use this instead of stemming, 
## uncomment this section and comment out the stemming section above)

# library(textstem)
# test <- data.frame(lemmatize_words(tidy_Q1$word))

# test_lema<-tidy_Q1 %>%
#   mutate_at("word", funs(lemmatize_words(.)))
# tidy_Q1 <- test_lema


# remove words that are extremely common and from the prompt
names <- data.frame(word = c("advis","advise", "advice", "brandon","michael","anthoni","russel","kevin","david","angela","michell","kim","nicol","vanessa","alicia"))
tidy_Q1<-tidy_Q1 %>%
  anti_join(names)

if (vignum == 1) {
  vwords <- data.frame(word = c("bore","boredom","bored","child","kid","school","daughter","son"))
}
if (vignum == 4) {
  vwords <- data.frame(word = c("rule","bed","time","bedtim","bedtime","child","kid","daughter","son"))
}
if (vignum == 5) {
  vwords <- data.frame(word = c("medic","medicin","med","medicine","medication","take","asthma","doctor","dr","child","kid","daughter","son"))
}
if (vignum == 3) {
  vwords <- data.frame(word = c("tv","show","sea","creatur","child","kid","daughter","son"))
}
if (vignum == 2) {
  vwords <- data.frame(word = c("child","kid","daughter","son"))
}
if (vignum == 6) {
  vwords <- data.frame(word = c("bore","math","class","school","child","kid","daughter","son"))
}

# Comment out this next command if not dropping any words 
tidy_Q1<-tidy_Q1 %>%
  anti_join(vwords)


# =========== Create document term matrix  =======
tidy_Q1_DTM <-
  tidy_Q1 %>%
  count(caseid, word) %>%
  cast_dtm(caseid, word, n)


# =============== Biterm Topic Modeling =========

# data cleaning of DTM to match what BTM expects
myx <- data.frame(tidy_Q1$caseid, tidy_Q1$word)
myx <- rename(myx,doc_id = tidy_Q1.caseid )
myx <- rename(myx,lemma = tidy_Q1.word )


set.seed(321)
BTMmodel  <- BTM(myx, k = 4, beta = 0.01, iter = 1000, trace = 100, detailed = TRUE)
# Note: detailed needs to be TRUE for the LDAvis

topicterms <- terms(BTMmodel, top_n = 10)
topicterms


# Visualization of Model
# top1 <- data.frame(topicterms[1])
# top1$topic <- 1  
# top2 <- data.frame(topicterms[2])
# top2$topic <- 2
# top3 <- data.frame(topicterms[3])
# top3$topic <- 3
# top4 <- data.frame(topicterms[4])
# top4$topic <- 4
# topterms <- data.frame(rbind(top1, top2, top3, top4))
# topterms <- rename(topterms,term = token)
# 
# topterms %>%
#   mutate(topic, term = reorder_within(term, probability,topic)) %>%
#   ggplot(aes(probability, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered() +
#   labs(y = NULL) +
#   ggtitle("Top words from BTM by topic")
# 
# toptermsFileName <- paste("BTMtopterms",vignum,".pdf",sep="")
# ggsave(toptermsFileName)


## Other data visualizations
# topiclabels <- c(1:4)
# plot(BTMmodel, top_n = 10, labels = topiclabels)
# topiclabelsFileName <- paste("BTMtopiclabels",vignum,".pdf",sep="")
# ggsave(topiclabelsFileName)

# group_terms   <- terms(BTMmodel, top_n = 15)
# group_biterms <- BTMmodel$biterms$biterms
# textplot_bitermclusters(x = group_terms, biterms = group_biterms)

# LDAvis
docsize <- table(myx$doc_id)
scores  <- predict(BTMmodel, myx)
scores  <- scores[names(docsize), ]
json <- createJSON(
  phi = t(BTMmodel$phi),
  theta = scores,
  doc.length = as.integer(docsize),
  vocab = BTMmodel$vocabulary$token,
  term.frequency = BTMmodel$vocabulary$freq)
serVis(json)



## ======= BTM Exclusivity and Coherence =========
source("BTM_functions.R")

## DTM for coherence calculation ##

myx2 <- aggregate(myx$lemma, by = list(myx$doc_id), paste, collapse = " ")
names(myx2) <- c("doc_id", "lemma")
corpus2 <- corpus(myx2$lemma)

DFM <- dfm(quanteda::tokens(corpus2))
DTM <- convert(DFM, to = "topicmodels")

coherenceBTM(BTMmodel, DTM)
exclusivity(BTMmodel)


# Do it for a variety of k's
df <- data.frame(matrix(ncol = 3, nrow = 6))
colnames(df) <- c('k', 'coherence', 'exclusivity')

for(k in 2:6){
  #  print(k)
  set.seed(321)
  tempmodel  <- BTM(myx, k = k, beta = 0.01, iter = 1000, trace = 100)
  df$exclusivity[k] <- exclusivity(tempmodel)
  df$coherence[k] <- mean(coherenceBTM(tempmodel, DTM))
  df$k[k] <- k
}

# create a plot of coherence vs exclusivity
if (vignum == 1) {
  ggplot(df, aes(x= coherence, y= exclusivity, label=k))+
    geom_point() +geom_text(aes(label=k),hjust=-.7, vjust=0, size = 4.5)+
    ggtitle("Situation: Bored after School")
  
  ggsave("BTMchoosingk1.pdf")
}
if (vignum == 4) {
  ggplot(df, aes(x= coherence, y= exclusivity, label=k))+
    geom_point() +geom_text(aes(label=k),hjust=-.7, vjust=0, size = 4.5)+
    ggtitle("Situation: Bedtime Rules")
  
  ggsave("BTMchoosingk4.pdf")
  
}
if (vignum == 5) {
  ggplot(df, aes(x= coherence, y= exclusivity, label=k))+
    geom_point() +geom_text(aes(label=k),hjust=-.7, vjust=0, size = 4.5)+
    ggtitle("Situation: Take Your Medicine")
  
  ggsave("BTMchoosingk5.pdf") 
}


if (vignum == 2 | vignum == 3 | vignum == 6 ) {
  
  ggplot(df, aes(x= coherence, y= exclusivity, label=k))+
    geom_point() +geom_text(aes(label=k),hjust=-.7, vjust=0, size = 4.5)+
    ggtitle("Coherence vs exclusivity")

  choosingkFileName <- paste("BTMchoosingk",vignum,".pdf",sep="")
  ggsave(choosingkFileName)
  
}


## =========== Export the BTM results to Stata format ===========
BTMdf <- data.frame(scores)
BTMdf$caseid <- (row.names(scores))
BTMdf$caseid <- as.integer(BTMdf$caseid)

require(foreign)

dataFileName <- paste("BTMtopics_vig",vignum,".dta",sep="")
write.dta(BTMdf, dataFileName) 

# Repeat for vig == 4 and vig == 5 to before moving to the next replication file
