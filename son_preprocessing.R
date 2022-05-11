# /Users/nina/Documents/PhD/eHealth/son.csv

# use csv2 for problem of semikolon
# son = read.csv("/Users/nina/Documents/PhD/eHealth/son.csv")
# write.csv2(son, file = "/Users/nina/Documents/PhD/eHealth/son2.csv", row.names = F)

library(dplyr)
glimpse(son)

son <- read.csv2("/Users/nina/Documents/PhD/eHealth/son.csv")
ncol(son)
nrow(son)
head(son)
summary(son)


names(son)
names(select(son, matches("slider"))) #check data for columns containing "slider"





## ----------------------------------------------------------------------------
## Was Jorge mit den Rohdaten gemacht hat (kopiert aus Github)


library(readr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(ggplot2)

# 1. import dataframe ----
mydf <- read_csv("data/answers-29.04.22.csv", 
) %>%
  mutate(answers = str_replace_all(answers, '""', '"'),
         answers = str_replace_all(answers, '\\"\\[', '['),
         answers = str_replace_all(answers, '\\]\\"', ']'),
         client = str_replace_all(client, '""', '"'),
         client = str_replace_all(client, '\\"\\{', '{'),
         client = str_replace_all(client, '\\}\\"', '}')) %>% as_tibble()


mydf <- mydf[-c(41020),]
# t(mydf[x, 5]) line to debug the code from comments of app users


json_list<- list()
for(i in 1:nrow(mydf)){
  json_list[[i]] <- pivot_wider(fromJSON(mydf$answers[i]), names_from = "label", values_from = c("value", "collected_at")) %>% as.tibble()
} # should have used purrr inside the previous mutate, but could not make it work :-(


## SON

son <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 15)

gambiarra <- map_df(son$app_usage, ~ map_chr(.x, ~ if(is.list(.x)){as.character(.x)}else{.x})) %>%
  mutate(across(contains("collected_at_"), as.numeric),
         across(contains("collected_at_"), as_datetime))

son <- cbind(son[,1:8], gambiarra)


son_raw <- read.csv2("/Users/nina/Documents/PhD/eHealth/ForNina/son_raw.csv")
head(son_raw)


son <- read.csv("/Users/nina/Documents/PhD/eHealth/ForNina/magic.csv")

summary(son)
names(son)

sum(son$listen_time <= 0)

#av rating by sound
son %>% group_by(sound) %>% 
  mean = mean(sound_rating, na.rm = TRUE)



setwd("/Users/nina/Documents/PhD/eHealth/ForNina/")
son <- read.csv("magic.csv")
head(son)

son_gr <- read.csv("groupby_magic.csv")
write.csv2("groupby_son.csv")
head(son_gr)



###########################
## Beautify the Data set ##
###########################


## ----------------------------------------------------------------------------
#add delta_sound rating to data frame
#negative delta means pre_loudness > post_loudness -> tinnitus reduction

son <- rename(son, "post_loudness" = sound_rating)
post_loudness_true <- son$pre_loudness - (son$post_loudness - 50)
delta_sound_rating <- post_loudness_true - son$pre_loudness

son$delta_sound_rating <- delta_sound_rating

range(son$delta_sound_rating, na.rm = T)


## ----------------------------------------------------------------------------
#replace NaN from python 
#correct son$headphones and son$favourite coding

son[son == "nan" | son == "NaN"] <- NA

son$headphones[son$headphones == "nan" | son$headphones == "NaN"] <- NA
son$headphones[son$headphones == "False" | son$headphones == "0.0"] <- F
son$headphones[son$headphones == "1.0" | son$headphones == "True"] <- T
son$headphones <- as.logical(son$headphones)

son$favourite[son$favourite == "nan" | son$favourite == "NaN"] <- NA
son$favourite[son$favourite == "False" | son$favourite == "0.0"] <- F
son$favourite[son$favourite == "1.0" | son$favourite == "True"] <- T


## ----------------------------------------------------------------------------
## son_gr$sound_mechanism: grouping sounds to masking and lateral inhibition

son_gr$sound
sounds_masking <- c("sparrows", "seagulls", "churchbells", "kettle", "gong", "notchedmeditation", "whales", "mice", 
                    "screamingkids", "laughingkids", "notchedmusic", "creakingdoor", "teethbrushing", "frogs", "writing", "cicada")
#bird chirp = sparrows? // scribbling = writing?

library(dplyr)
# son_filtered <- son_gr %>% filter(grepl("fm_am", sound)) 
# sounds_masking <- sounds_masking + son_filtered
# str(son)
sounds_masking <- c(sounds_masking, grep("fm_", son_gr$sound, value = T))
sounds_masking <- c(sounds_masking, grep("_bp", son_gr$sound, value = T))
sounds_masking <- c(sounds_masking, grep("_notch", son_gr$sound, value = T))
sounds_masking <- c(sounds_masking, grep("tin_", son_gr$sound, value = T))

sounds_masking

#wenn Wert aus son_gr$sound in sounds_masking, schreib son_gr$sound_category[i] = 1
## son_gr$sound_mechanism: sound masking == 1, lateral inhibition == 0

for (i in 1:length(son$sound)){
  if (son$sound[i] %in% sounds_masking) {
    son$sound_mechanism[i] <- 1
  } else {
    son$sound_mechanism[i] <- 0
  }
}
son$sound_mechanism <- as.factor(son$sound_mechanism)
str(son$sound_mechanism)
# for (i in 1:length(son_gr$sound)){
#   if (son_gr$sound[i] %in% sounds_masking) {
#     son_gr$sound_category[i] <- T
#   } else {
#     son_gr$sound_category[i] <- F
#   }
# }


## ----------------------------------------------------------------------------
#son_gr$sound_origin: grouping sounds to natural and artificial
#natural sounds == 1, synthetic sounds 0

son_gr$sound
sounds_natural <- c("sparrows", "seagulls", "churchbells", "kettle", "gong", "notchedmeditation", "whales", "mice", "screamingkids", "laughingkids", "notchedmusic", "creakingdoor", "teethbrushing", "frogs", "writing", "cicada",
                  "flappingflag", "tumblingrocks", "purringcat", "beachwaves", "didgeridoo", "desertwind", "rustlingleaves", "birdwings", "bees", "crowdbabble2", "bubbling", "bonfire", "waterfall", "applause", "rain", "shower")

for (i in 1:length(son$sound)){
  if (son$sound[i] %in% sounds_natural) {
    son$sound_origin[i] <- 1
  } else {
    son$sound_origin[i] <- 0
  }
}
son$sound_mechanism <- as.factor(son$sound_mechanism)

## ----------------------------------------------------------------------------
#create df son_gr that groups son by 64 sounds

son$favourite <- as.logical(son$favourite)

son_gr <- son %>%
  group_by(sound) %>%
  summarise(
    sum_favourite = sum(favourite, na.rm = T),
    mean_pre_loudness = mean(pre_loudness), #maybe other than mean?
    mean_pre_distress = mean(pre_distress),
    mean_pause_count = mean(pause_count, na.rm = T),
    mean_listen_time = mean(listen_time), #not correct, because there are negative values
    mean_longest_listen_time = mean(longest_listen_time),
    mean_post_loudness = mean(post_loudness, na.rm = T),
    sound_mechanism = as.factor(mean(sound_mechanism, na.rm = T)),
    sound_origin = as.factor(mean(sound_origin, na.rm = T)),
    mean_delta_sound_rating = mean(delta_sound_rating, na.rm = T)
  )



## ----------------------------------------------------------------------------
#problem: negative listen_time
#write Vishnu or Carsten?


write.csv2(son, file = "son_data.csv")
write.csv2(son_gr, file = "son_groupedby_sound.csv")

###########################
### Plotting some stuff ###
###########################


#son_sort <- son_gr %>% arrange(sound_mechanism)

# a <- ggplot(son_gr, aes(sound, mean_listen_time))
# a <- a + geom_point(aes(colour = factor(sound_mechanism)))
# a
# 
# b <- ggplot(son_gr, aes(sound, mean_listen_time))
# b <- b + geom_point(aes(colour = factor(sound_mechanism)))
# b


c <- ggplot(son_gr, aes(sound, sum_favourite)) +
  geom_point(aes(colour = factor(sound_mechanism)))
c


d <- ggplot(son, aes(headphones, delta_sound_rating)) +
  geom_boxplot()
d

?geom_boxplot
mean(son$delta_sound_rating[son$headphones == T], na.rm = T)

range(son_gr$mean_delta_sound_rating, na.rm = T)

e <- ggplot(son_gr, aes(sound, mean_delta_sound_rating)) +
  geom_point(aes(colour = factor(sound_mechanism)))
e

f <- ggplot(son_gr, aes(mean_longest_listen_time, mean_delta_sound_rating)) +
  geom_point()
f


  
?facet_wrap
## ----------------------------------------------------------------------------
##Statistik

library(car)

t.test(son$pre_loudness, son$post_loudness, paired = T)

# ANOVA tin + - natural + -
# 1)a) ANOVA pre post -> tin +-
# 1)b) ANOVA pre post -> natural + -
# 2) ANOVA pre pos  t -> beides

# gematched oder nicht? 1) wurde ueberhaupt gematched bei der person? 2) ist der abgespielte sound gematched?

cor(son$pre_loudness, son$post_loudness, use = "pair")

summary(son_gr)

??levene
t.test(son$longest_listen_time, son$delta_sound_rating, var.equal = F)
cor(son$longest_listen_time, son$delta_sound_rating, use = "pair")

head(son_gr)
range(son_gr$longest_listen_time)
