#cleaning analysing and visualising canadian immigration data 
#sourced from https://github.com/rashida048/Datasets/blob/master/Canada.xlsx
#I have split the dataset into four, added empty rows, added typos and deleted some data 
#to test my data cleaning skills 
setwd("~/Desktop/Learning R/Canada-immigration")
library(tidyverse)
library(readxl)
library(reclin)
library(stringr)
library(visdat)
library(fuzzyjoin)
library(stringdist)
library(janitor)

###IMPORTING ####

# a function to import the four excel files in my wd
file_list <- list.files(pattern='*.xlsx')
df_list <- lapply(file_list, read_excel, skip = 1) #reading in the files, skipping the first row  
#now I want to delete the header name for df's 2 3 and 4 so I can bind rows 
names(df_list[c(2,3,4)]) <- NULL #I dont think this was necessary
#binding rows to create one large dataset
df1 <- bind_rows(df_list)

### CLEANING ####

names(df1) <- tolower(names(df1)) #all col names to lower 
sapply(df1[1:2], unique) #can see first two cols are uninteresting so will drop them 
df1 <- select(df1,-c(1,2))
#area, reg and dev also have no use to me 
df1 <- select(df1,-c(area, reg, dev))
#now, I want to remove the rows which are all NA.
#let me see if there are any columns in which the only NA's are those which are NA rows 
vis_miss(df1)
#second row, areaname is the winner. 
#so, filtering out rows if they have NA in col. 2
df1 <- df1 %>% filter(!is.na(areaname)) 
#vis_miss(df) shows this has worked
#also removing row of zeros
df2 <- df1 %>% filter(!areaname == 0) #new df 
#now, I want to set the areaname and regname to factors
#first, let me the area/ region names are as expected ... 
sapply(df2[c("areaname", "regname")], unique)
#okay, I can see we have some typos in both
#luckily, I have data on the correct spellings for both 
spellings <- read_excel("~/Desktop/Learning R/Canada/Correct_spellings.xlsx")
#pulling the unique areanames from spelling
areas <- as.tibble(unique(spellings$AreaName))
names(areas) <- "AreaName"
#creating special use tibble for this 
df_areas <- as.tibble(df2$areaname)
names(df_areas) <- "AreaName"

#comparing area entries in the df with correct spellings 
correct_areas <- stringdist_left_join(df_areas, areas, by = "AreaName", method ="qgram")
#introduced 2 NAs - replacing manually 
correct_areas <- replace(correct_areas, is.na(correct_areas), "Latin America and the Caribbean")
#replacing areaname column in df with the corrected names 
df2['areaname'] <- correct_areas['AreaName.y']

#now doing the same for country names 
#but this time using the fact we can match area names too to 
#use reclin record linkage functionality 

countries <- spellings[c("OdName", "AreaName")]
df_countries <- df2[c('odname', 'areaname')]
names(countries) <- tolower(names(countries))

a <- pair_blocking(countries, df_countries, 
              blocking_var = "areaname"
              ) %>% 
  compare_pairs(by ="odname", default_comparator = lcs()) %>% 
  select_greedy(weight = "odname") %>% 
  link()


#an aside to test if reclin worked or they just lined up, as 
#they were in the same order before 

#x <- df %>% arrange(desc(`1980`))
#testing <-x[c('odname', 'areaname')]

#y<- pair_blocking(testing, countries, 
#            blocking_var = "areaname"
#) %>% 
# compare_pairs(by ="odname", default_comparator = lcs()) %>% 
#select_greedy(weight = "odname") %>% 
#link()
#Yes, it did actually work. See order of rows in y is as expected 
#so, ready to put the corrected column into df

df2['odname'] <- a['odname.x'] 


#documenting previous failed attempts at correcting typos/ NA's:

#countries_na <- correct_countries %>% 
#  filter(is.na(value.y)) %>% 
 # select(1) %>% na.omit(correct_countries)

#names(countries_na) = 'value'

#pairs <- pair_blocking(countries, countries_na) 
#corrections <- compare_pairs(pairs, by = "value",
 #                            default_comparator = lcs()) %>% 
#  select_n_to_m(weight= 'value') %>% 
  #link() %>% 
  #select(value.x) %>% 
 # head(4)
#these are my four NA's corrected. Now I need to get these into corrcet_countries



#After original str_dist_left_join

#This has worked, except for 5 NA's 
#One of which is from missing country in my df
#Four are unexplained 
#Want to see which correcly spelt countries aren't represented in correct_countries
#j <- countries %>% filter(!value %in% correct_countries$value.y)
#assigning this for future reference - this is what I need 
#to fill in the NAs for 

#i <- which(is.na(correct_countries$value.y))
#for (a in 1:5){
 # correct_countries$value.y[i[a]] <- j[a,]
#}
#filled them in by isolating which row the NA is in, and then
#filling these rows in with corresponding values from j
#checking that no NA's remain in correct_countries
#correct_countries %>% filter(is.na(value.y))

#now I can replace the df column with this cleaned version 
#df['odname'] <- correct_countries['value.y'] 

#this didnt work as compare_pairs had introcued new rows without me realising
#so, there were more rows that could fit into df

#anyway... back to the cleaning.... 

#chekcing if regname is okay... 
#unique(df$regname) - can see some typos... 
#so, doing the same for this col, using the fact that we have 
#areaname to cross compare... (also have same od name but thats too easy)

regions <- spellings[c('AreaName','RegName')]
names(regions) <- tolower(names(regions))
df_regions <- df2[c('areaname','regname')]

b<-pair_blocking(regions, df_regions, 
              blocking_var = "areaname") %>% 
  compare_pairs(by ="regname", default_comparator = lcs()) %>% 
  select_greedy(weight = "regname") %>% 
  link()

#successful. Now putting correct 
df2['regname'] <- b['regname.x'] 
#okay, so first three rows are taken care of... finally. 
#checking if the fourth is okay 
#unique(df$devname)
#all good... thankgod I didnt mess these ones up too 

#now I can begin to do some more standard cleaning, 
#starting with removing the annoying backticks from the year columns 
df3<- clean_names(df2) #new df 
#using janitor package to remove backticks... worked but added x in front of 
#all my year columns 
#I dont know how to remove this... gsub just brings the ticks back 
#i think the issue originates from data import... mustve been something 
#amiss with the excel column names... will continue with the xcolumns for 
#now...  


