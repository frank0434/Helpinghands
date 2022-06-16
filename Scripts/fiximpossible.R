######
# Aim: Detect the anomalies in the bud score data
######
library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)

df.E1.DS.2 <- read_excel("df.E1.Long.xlsx")
# evaluating the bud_score value difference between two times 
# the value should stay the same or increase
# there are occasions in the data that the values will drop randomly 
Test <- df.E1.DS.2 %>% 
  group_by(Treatment,ID,Shoot_No,Bud) %>% 
  # use diff function with lag 1 to compare values within each group
  # diff will always one value shorter than the original row numbers 
  # use "0" to make the row number the same after diff run 
  # if the value is greater or equal to 1 comparing to the previous value, return TRUE
  # otherwise, NA. the results are saved into a new column called new
  # another new column called new2 to evaluate the missing rows
  # and copy the first non-mising values from Bud_score
  mutate(new=c(0,(ifelse(diff(Bud_Score,lag=1)>=1, TRUE, NA))), 
         new2=ifelse(is.na(new), NA, Bud_Score))%>% 
  # fill the missing value with the previous observation 
  fill(new2)


Test %>% 
  filter(Treatment == "None",
         Shoot_No == 8,
         ID == 29,
         Bud == "B1") %>% View()



Test %>% 
  group_by(Treatment,ID,Shoot_No,Bud) %>% 
  summarise(Test = all(diff(new2) >= 0)) %>% 
  filter(Test != TRUE)
Test %>% 
  filter(Treatment == "None",
         Shoot_No == 4,
         ID == 116,
         Bud == "B1") %>% View()
# still errors in it
# repeat the procedure second times with another two new columns 
Test <- Test %>% 
  group_by(Treatment,ID,Shoot_No,Bud) %>% 
  mutate(new3=c("0",(ifelse(diff(new2,lag=1)>=1, "TRUE", NA))), 
         new4=ifelse(is.na(new3), NA, Bud_Score)) %>% 
  fill(new4)


suspicious <- Test %>% 
  group_by(Treatment,ID,Shoot_No,Bud) %>% 
  summarise(Test = all(diff(new4) >= 0)) %>% 
  filter(Test != TRUE)
Test %>% 
  filter(ID %in% suspicious$ID,
         Shoot_No %in% suspicious$Shoot_No, 
         Bud %in%suspicious$Bud,
         Treatment%in% suspicious$Treatment) %>% 
  View()




library(ggplot2)
df.E1.DS.2 %>% 
  filter(ID %in% suspicious$ID,
         Shoot_No %in% suspicious$Shoot_No, 
         Bud %in%suspicious$Bud,
         Treatment%in% suspicious$Treatment) %>% 
  ggplot(aes(Time_rank, Bud_Score)) +
  geom_point()+
  geom_line() +
  facet_grid(Bud+Shoot_No ~ ID)
