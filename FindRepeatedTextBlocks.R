#Task is to identify multi sentence text blocks which repeat throughout a document. And then to indentify the length of these. A sentence is repeated of it occured >100 as T times per year

#Approach
# 1) Tokenize Text on subsentence breaks (. ; and , )   [Can do this in scala without library]
####### Return a parLit of arrays of word strings
# 2) Minimal subsentence featurs Number of Characters  (remove newlines and other whitespace if needed)
# 3) Perform a moving average   with window 2,3,4,5,6,7,8,10 size    [This can't be done in R ]
# 3B)  Create a new dataset which is Array of Sentence lengths with moving window   HashMap<[int],[linked list]>
# 3C)  I could first hashtogether complete sentences and delete the ones which do not occure >100 times individually
# 4) Hash Setnence Arrays into hashmap to find exact matches (with some collisions rate). Remove all Hash Buckets which has less then 100 sentences
# 5) Perform Exact equality or MD5 hash comparison on origninal sentence. To throw out sentences which do not belong (This only needs to be done for 100 exampels per group because we do not care about how often a block is used )


#Complexity
# n for tokenizing
# n for feature extraction wiht counts
# n for moving average (but more like n*3 cause you need to store it in a bunch of places) *K
#    Where K is the max number of sentences a text block can contain
# n For hashmap assignment
# (H)*T*avg(sentence lenght)   for HashBucket exact equality check  where H is the number of hashmap buckets if we set  and T is the max number of repeats you need to be considered a repeat text block

### What if there are  systematic collisions where I have multiple types of >T repeated TextBlocks
##### Maybe i should increase the hash function so that collisions like this are very unlikely. Like A small MD5
##### OR if i have colision

# I think i am going to write the above in Scala. Because i want super performant and customizable Hashmaps

# I could write it in C++ should not be hard.  RCPP
# Maybe i even do the sentence tokenization in R. Since i have fast libraries for that
# I think RCPP is best because then i can package it and maybe publish to CRAN . Although hashmaps are a bit strange in STL https://stackoverflow.com/questions/3578083/what-is-the-best-way-to-use-a-hashmap-in-c
# I want the hashmap to do linkedList on collision
# Ok but what about parallel. I don't want to do things parallel in C++. Here Scala would be the best
# Clearly the exact comparisons can be parallized by bucket

#Using HN dataset as a sit in before i
library(data.table)
system.time(hndf<-fread("/users/wolffr/Downloads/hacker_news_sample.csv")) # 90seconds
setwd("/Users/wolffr/private_workspace/FindLongstRepeatedParagraphs")

#install.packages("tokenizers")
if(FALSE){
library(tokenizers)
system.time(sentences <- tokenize_sentences(hndf$text[1:100000])) #12 seconds
((nrow(hndf)/100000)*12.5)/360  #Tokenizing the 1.5 gb would take 1.27 hours   Seems kinda slow ...
}

### Ok this i already too slow
if(FALSE){
sentLengths <- foreach( sentsent =sentences ) %do%{
          lens<-foreach ( sent =sentsent) %do%{
            stri_length(sent)
          }
}
}

library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++14")
sourceCpp("cppSplitStr.cpp")

#system.time(sentences1 <- cpp_str_split(hndf$text[1:10000],'.'))   #0.1 second
#system.time(sentences1 <- cpp_str_split(hndf$text[1:100000],'.'))   #1 second

#2.5 gb ram rsession
system.time(sentences1 <- cpp_str_split(hndf$text[1:1000000],'.'))   #10 seconds
#3.5 gb ram rsession
system.time(sentences2 <- cpp_str_split(hndf$text[1000000:2000000],'.'))   #13 seconds
#3.96 GB ram ressions
system.time(sentences3 <- cpp_str_split(hndf$text[2000000:3000000],'.'))   #25 seconds
#4.90 GB ram rsession
system.time(sentences4 <- cpp_str_split(hndf$text[3000000:3659697],'.'))   #9 seconds
#5.22 gb ram rsession

#system.time(sentences3 <- cpp_str_splitLL(hndf$text[1:100000],'.'))   # 148 seconds  WTF LIST sucks. I guess its a List from Rcpp so something related to shitty are structs

#system.time(sentences <- cpp_str_split(hndf$text[1:3659697],'.'))   # Run out of memory and rsession dies.  Probably std::Vector has a limit where it then needs to copy stuff and hence duplicates space

# Ok now lets try to just hash a window of sentences together
#system.time(hashes <-cpp_str_split_and_hash_window_of_past_splits(hndf$text[1:100000],'.',3))   #1.401 seconds

EXMAPLETEXTS<-c("I am cool but you don't know it. Lets have a drink. My street is green. Why not take a leak. Big data is small Data.",
                "I am cool but you don't know it anyway. Lets have a drink. My street is green. Why not take a leak. Big data is small Data.",
                "I am cool but you don't know it. Lets have a drink. My street is red. Why not take a leak. Big data is small Data.",
                "I am cool but you don't know it. Lets have a drink. My street is red. Why not take a leak. Big data is small Data.")
system.time(hashes <-cpp_str_split_and_hash_window_of_past_splits(EXMAPLETEXTS,'.',3))

system.time(hashes <-cpp_str_split_and_hash_window_of_past_splits(hndf$text[1:1000000],'.',3))   #13 seconds

system.time(hashes <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits(EXMAPLETEXTS,'.',3))
system.time(hashes <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits(hndf$text[1:1000000],'.',3))   #14.7 seconds


system.time(hashes2 <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits_and_hashbucket_them(EXMAPLETEXTS,'.',3,1))

system.time(hashes <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits_and_hashbucket_them(hndf$text[1:1000000],'.',2,10))   #22 seconds
system.time(hashes <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits_and_hashbucket_them(hndf$text[1:3000000],'.',6,10,4))   #77 seconds
system.time(hashes <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits_and_hashbucket_them(EXMAPLETEXTS,'.',3,2,4))



#OK so now i need to put the sentence windows in a hash map
#To make the structure easier i would like to just glue all documents together.  first then i can reference Sentences by their index in the array. Glueing them all together will create artifacts at the begining and end of a document but i can just add a sentence with a sepcial character inbetween.

##### Got Read Data from Barnaby


datadir<-"/Users/wolffr/Dropbox/BG/txt_files"
bg_files <- dir(datadir)
#bg_files<-bg_files[1:10000]
file_cat <- rep("",length(bg_files))
system.time(
for( i in 1:length( bg_files )){
  file_cat[i]<-readr::read_file(paste(datadir,bg_files[i],sep="/"))
}
)  #76874 files takes 318 seconds

fLengths<-rep(0,length(bg_files))
for( i in 1:length( bg_files )){
  fLengths[i]<-nchar(file_cat[i])
}


library(ggplot2)

ggplot(data.frame(x=fLengths),aes(x=x))+geom_histogram()
library(data.table)

bg_df <- data.table( fname=bg_files,fLen=fLengths,fCat=file_cat)
object.size(bg_df)/1e6 # 1gb size


system.time(BG_hashes <-cpp_str_split_and_concat_docs_and_hash_window_of_past_splits_and_hashbucket_them(bg_df$fCat,'.',5,10,15,F)) #looking for alteast 5 sentences

sentCounts<-BG_hashes[[3]]
countedSents<-BG_hashes[[4]]
lengthCountedSents<-nchar(BG_hashes[[4]])

count_s<-data.table(sentCounts=sentCounts,countedSents=countedSents,lengthCountedSents=lengthCountedSents)
library(dplyr)
(View(count_s%>%arrange(desc(sentCounts))%>%filter(lengthCountedSents>350) ) )


#Looks good i can fit all data into memory. But my approach of splitting sentences on . Is not working so well since there are a lot of legal abreviations which are like 1. or Abs.
# I am going to find all of these words by counting everyword that contains a .

system.time(wordCounts<-cpp_countWordsWhichContain(bg_df$fCat,' ',".:;|}{()[]")
