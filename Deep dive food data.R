library(readxl)
library(readr)
library(tidyverse)
install.packages("writexl")
library(writexl)
setwd("~/Desktop/Learning R/Learning resouces/Data ")
data_raw <- read_excel("sampledatafoodinfo.xlsx", 
                   sheet = 2,
                   col_names =  TRUE)
data_1 <- mutate_at(data_raw, vars(Category), as.factor) #Category to factor
data_2 <- data_1 %>% select(c(-3,-9,-10)) #removing last two cols and measure
data_3 <- data_2 %>% 
  group_by(Category) %>% 
  mutate_at(vars(Protein,Fat,Carbs, Fibre), 
            ~replace_na(., 
                        mean(., na.rm = TRUE))) 
#Nan's not going so converting NaN to NA

data_3 <- data_3 %>% mutate_all(~ifelse(is.nan(.), NA, .))
#issue was whole of some cats. were zero. So manually setting to zero 
#and ungrouping 
data_4 <- replace(data_3,is.na(data_3),0) %>% ungroup()

data_5 <-data_4 %>% 
  mutate_if(is.numeric, round, digits=1)
write_xlsx(data_5,"food_cleaned.xlsx")

#finding average nutritions for the Categories
analysis_1 <- data_5 %>% 
  group_by(Category) %>% 
  summarise(across(.cols = where(is.numeric),.fns = mean))

#finding highest protein food item in each category 
analysis_2 <- data_5 %>% 
  group_by(Category) %>% 
  filter(Protein == max(Protein))
#this is pulling multiple values per group if they are equal max 
#want to take lowest fat if there are ties for max
#https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr/35607934#35607934
analysis_3 <- data_5 %>%
  group_by(Category) %>% 
  arrange(desc(Protein), Fat) %>% 
  slice(1)

#a basic plot of i) protein and ii) fat of the highest protein in each category
plot_1 <- ggplot(analysis_3, 
                 aes(x= reorder(Category, Protein), y=Protein)) + 
  geom_col() +
  coord_flip() + 
  labs(title = "Fast food has most protein",
                                  x = "Food Type",
                                  y= "Average Protein (g)")
plot_1


#points showing protein by fat, colour as type and size as calories
plot_2 <- ggplot(analysis_3, 
       aes(Fat,Protein, 
           colour = Category, size = Calories )) +
  geom_point() + labs( title = "Fast food: High Protein, Fat and Calories",
                       x = "Average Fat (g)",
                       y = "Average Protein (g)",
                       caption = "Source: Absolutely no idea")
plot_2

#plotting points within all data in each category, facetted by category 
#calories by fat
plot_3 <- ggplot(data_5, aes(Fat,Calories)) +
         geom_point(size = 0.5) +
         facet_wrap(~ Category)+
  labs( x = "Average Fat (g)",
        y = "Average Calories") +
  theme(legend.position="none",
        strip.background=element_rect(colour="black",
                                      fill="grey"))
plot_3
       