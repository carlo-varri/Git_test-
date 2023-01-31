#cleaning analysing and visualising canadian immigration data 
#sourced from https://github.com/rashida048/Datasets/blob/master/Canada.xlsx
#I have split the dataset into four, added empty rows, added typos and deleted some data 
#to test my data cleaning skills 

library(tidyverse)
library(readxl)
library(reclin)
library(stringr)
library(visdat)
library(fuzzyjoin)
library(stringdist)

###IMPORTING ####

# a function to list .xlsx files in my wd, and then import them
#I came back to this and added .name_repair, as reading in the columns 
#with date names was causing forcing backticks into col names 
file_list <- list.files(pattern='*.xlsx')
df_list <- lapply(file_list, read_excel, skip = 1, 
                  .name_repair = ~ make.names(., unique = TRUE))

# next I am binding rows to create one large dataset
df1 <- bind_rows(df_list)

### CLEANING ####

#all col names to lower 
names(df1) <- tolower(names(df1))

#can see first a few cols are uninteresting so will drop them.
#also I want to remove the rows which are all NA (vis_miss shows all NAs in areaname are 
#full rows of NA). FInally removing row of zeros seen 
df1 <- df1 %>% select(-c(1,2,4,6,8)) %>% filter(!is.na(areaname)) %>% 
  filter(!areaname == 0) 

#Assigning this to a new dataset 
df2 <- df1 

#now, I want to set the areaname and regname to factors
#first, let me see if the area/ region names are as expected ... 
#sapply(df2[c("areaname", "regname")], unique)

#okay, I can see we have some typos in both
#luckily, I have data on the correct spellings for both.
#importing this in 
spellings <- read_excel("~/Desktop/Learning R/Canada/Correct_spellings.xlsx")

#pulling the unique areanames from spelling
areas <- as.tibble(unique(spellings$AreaName))
names(areas) <- "AreaName"

#creating special use tibble for this 
df_areas <- as.tibble(df2$areaname)
names(df_areas) <- "AreaName"

#comparing area entries in the df with correct spellings 
correct_areas <- stringdist_left_join(df_areas, areas, by = "AreaName", method ="qgram")

#introduced 2 NAs, btoh of which relate to the same area, 
#so I will replace these manually 
correct_areas <- replace(correct_areas, is.na(correct_areas), "Latin America and the Caribbean")

#replacing areaname column in df with the corrected names 
df2['areaname'] <- correct_areas['AreaName.y']

#now, I want to fix the typose in country names. However, I want to do this 
#in a neater way than above, as creating and then manually replacing NAs
#is arduous
#so, this timeI'll use reclin with a blocking var of area names. 
#first, creating a dataset with correct countries, and pulling out the 
#current countries from df2. Notice I've also included areanames in the 
#new dataframes. This is because I will use these as a blocking var. 
countries <- spellings[c("OdName", "AreaName")]
df_countries <- df2[c('odname', 'areaname')]
names(countries) <- tolower(names(countries))

#now creating a dataset with the typo'd countries matched to the correct 
#ones. 
a <- pair_blocking(countries, df_countries, 
              blocking_var = "areaname"
              ) %>% 
  compare_pairs(by ="odname", default_comparator = lcs()) %>% 
  select_greedy(weight = "odname") %>% 
  link()


#This worked, but I was unsure if that was only because the correct 
#spelling and the typos were all in the same order to start with. 
#so, I ran it again with the typos in a random order 
#Note, this is where I first discovered the backquote issue

#x <- df %>% arrange(desc(`1980`))
#testing <-x[c('odname', 'areaname')]

#y<- pair_blocking(testing, countries, 
#            blocking_var = "areaname"
#) %>% 
# compare_pairs(by ="odname", default_comparator = lcs()) %>% 
#select_greedy(weight = "odname") %>% 
#link()
#Yes, it did actually work. See order of rows in y is as expected 
#so, now I have comfort this is a good method for future, 
#I'm ready to put the corrected column names back into df2

df2['odname'] <- a['odname.x'] 

#mathcing typos and correct spellings using reclin took me a long time 
#to get right. 
#Below I document previous failed attempts at correcting typos/ NA's,

#••••••••••••••••••••••••••••••

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

#••••••••••••••••••••••••••••••

#anyway... back to the cleaning.... 

#chekcing if regname is okay... 
#unique(df$regname) - can see some typos... 
#so, doing the same for this col, using the fact that we have 
#areaname to cross compare (note, I know I also have all the country names 
#fixed for typos, so I could just use them to match, but thought that 
#was a bit too easy)

regions <- spellings[c('AreaName','RegName')]
names(regions) <- tolower(names(regions))
df_regions <- df2[c('areaname','regname')]

#now creating a dataset with the typo'd areas matched to the correct 
#ones. 
b<-pair_blocking(regions, df_regions, 
              blocking_var = "areaname") %>% 
  compare_pairs(by ="regname", default_comparator = lcs()) %>% 
  select_greedy(weight = "regname") %>% 
  link()

#successful. Now putting into my original dataframe 
df2['regname'] <- b['regname.x'] 

#okay, so first three rows are taken care of... finally. 
#checking if the fourth is okay 
#unique(df$devname)
#yep, all good.

#now I can begin to do some more standard cleaning, 
#starting with removing the annoying backticks from the year columns 
#df3<- clean_names(df2)
#using janitor package to remove backticks... worked but added x in front of 
#all my year columns 
#I dont know how to remove this... gsub just brings the ticks back 
#i think the issue originates from data import... mustve been something 
#amiss with the excel column names... will continue with the xcolumns for 
#now...  
#I ended up asking someone at work about these backtick. They told me 
#this is because R cannot handle numerical col. names. I ended up
#editing my reads_excel function above to which introduced 'X's'
#as well. This is the best I can do. 
#I'll now edit the col headers to read CY_xxxx, denoting calendar year

colnames(df2) <- str_replace(colnames(df2),"x","CY_")

#now, I want to rename odname to country, 
#areaname to continent, regname to region and devname to development_status
#then dropping the old column names
df2<- df2 %>% mutate(country = odname, 
                  continent = areaname, 
                  region = regname,
                  dev_status_of_region = devname,
                .before= odname) %>% 
                select(-c(odname,areaname,regname,devname))

#now, I want to turn area, region and dev_status to factors,
#and assign this to a new dataframe 

cols_to_become_factors <- names(df2[1:4])
df2[cols_to_become_factors] <- lapply(df2[cols_to_become_factors], factor)
df3 <- df2

#I think I'm ready to end the cleaning now. 
#Just checking no NA's exist 
#vis_miss(df3)

#Okay, so there are rows of NA data across all years for three countries 
#I want to fill in each column with the median number from the same region,
#in the same year

df4 <- df3 %>% 
  group_by(region) %>% 
  mutate_at(vars(CY_1980:CY_2013), 
            ~replace_na(., 
                        median(., na.rm = TRUE))) 
#I want to pivot my data so we have a date column, rather than 
#a column per year, and makign it numeric 

df4 <- df4%>% pivot_longer(cols=c(CY_1980:CY_2013),
                    names_to='year',
                    values_to='no_of_immigrants')
df4$year <- gsub("CY_","",df4$year) %>% as.numeric()

#Finally, looking at dataframe, I want to remove 'regions' from dev_status_of_regions
df4$dev_status_of_region <- gsub("regions", "", df4$dev_status_of_region)

#okay, cleaning come to an end 

#### ANALYSIS AND VISUALISATION ####
#examples of questions I want to answer:
#1. What has the overall immigration trend been since 1980?
#2. What has the trend in each area been since 1980?
#3. What has the trend in each region been since 1980?
#4. Which region has experienced highest % change in immigration since 1980?
#5. Which country has experienced highest % change in immigration since 1980?
#6. Has there been more migration from developed or developing countries since 1980?

#I wont stick to these religiously. I'll start with the first, but will 
#deviate if more interesting analysis opens up 

#1. What has the overall immigration trend been since 1980?
#I'll use ggplot to visualise the total since 1980

plot_df_1 <- df4 %>% filter(country == 'Total')
plot_1<- ggplot(plot_df_1, aes(year, no_of_immigrants)) +
  geom_line()

#now, I want to see how developed vs devloping has looked in this time 
#, as well as editing the labels 
plot_df_2 <- df4 %>% group_by(dev_status_of_region,year) %>% 
  summarise(total_per_dev_status = sum(no_of_immigrants))

plot_2 <- ggplot(plot_df_2, aes(x = year, 
                      y= total_per_dev_status, 
                      group = dev_status_of_region,
                      colour = dev_status_of_region))+ 
  geom_line() +
    labs(y = "No. of immigrants",
         x = "Year") +
  labs(color='Legend') 



#Now the same, by continent, and removing the legend title 

plot_df_3 <- df4 %>% 
  filter(!continent == 'World')  %>% 
  group_by(continent,year) %>% 
  summarise(total_per_continent = sum(no_of_immigrants)) %>% 
  ungroup()

plot_3 <- ggplot(plot_df_3, aes(x = year, 
                      y= total_per_continent, 
                      group = continent,
                      colour = continent))+ 
  geom_line() + 
  labs(y = "No. of immigrants that year",
       x = "Year") +
  scale_color_discrete(name="")

#I can see that Asia has expereinced the highest growth in numbers of 
#immigrants by far. I want to see if there have been any countries with 
#particularly high immigration numbers 

#As there are so many countries in Asia, I'll first narrow down to the 
#top 10 immigration countries 

top_10_asia <- df4 %>% 
  filter(continent == 'Asia') %>% 
  group_by(country) %>% 
  summarise(country_total = sum(no_of_immigrants)) %>% 
  arrange(desc(country_total)) %>% 
  slice(1:10) %>% select(1)
  
#then, filtering the dataframe by countries on this list 
plot_df_4 <- subset(df4, country %in% top_10_asia$country)

plot_4 <- ggplot(plot_df_4, aes(year, no_of_immigrants, colour = country)) +
         geom_line() +
  labs(y = "No. of immigrants that year")

#I'll have a look at region trends now, faceting for continent

plot_df_5 <- df4 %>% group_by(continent, region, year) %>% 
  summarise(region_total = sum(no_of_immigrants))

plot_5 <- ggplot(plot_df_5, aes(year, region_total, colour = region)) +
  geom_line() +
  facet_grid(rows = vars(continent), scales = "free_y")

#now I want to plot percentage change in numbers of immigrants per continent 
plot_df_6<- df4 %>% 
  group_by(continent, year) %>% 
  summarise("continent_total" = sum(no_of_immigrants)) %>% 
  mutate(percentage_change = (continent_total/lag(continent_total) - 1) * 100) %>% 
  replace(is.na(.),0) %>% filter(!continent == "World")

ggplot(plot_df_6, aes(year, percentage_change, colour = continent)) +
  geom_line() + labs(y = "YoY percentage change")
                                          
plot_df_6 %>% group_by(continent) %>% filter(continent_total == max(continent_total))
       