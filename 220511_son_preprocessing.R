###########################
## Beautify the Data set ##
###########################



## ----------------------------------------------------------------------------
#pre stuff

rm(list = ls())
setwd("/Users/nina/Documents/PhD/eHealth/ForNina/")
son <- read.csv("son.csv")



## ----------------------------------------------------------------------------
#libraries

library(dplyr)
library(ggplot2)




## ----------------------------------------------------------------------------
#add delta_sound_rating to data frame

son <- rename(son, "post_loudness" = sound_rating)

#post_loudness == 50 means no change from pre- to post-loudness
#post_loudness_true is the adjusted value of post_loudness to the range 0-100 from pre_loudness
son$post_loudness_true <- son$pre_loudness - (son$post_loudness - 50)

#for delta_sound_rating we calculate post - pre
son$delta_sound_rating <- son$post_loudness_true - son$pre_loudness
#negative delta means post_loudness < pre_loudness -> tinnitus reduction

range(son$delta_sound_rating, na.rm = T)
#values of delta_sound_rating can range from -50 to 50



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
son$favourite <- as.logical(son$favourite)



## ----------------------------------------------------------------------------
#create data frame son_gr that groups son by 64 sounds

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
    mean_delta_sound_rating = mean(delta_sound_rating, na.rm = T),
    sound_origin = NA,
    sound_mechanism = NA
  )



## ----------------------------------------------------------------------------
#add sound_mechanism: grouping sounds to masking and lateral inhibition
#sound masking = 1, lateral inhibition = 2

#create a vector with half the masking sounds
sounds_masking <- c("sparrows", "seagulls", "churchbells", "kettle", "gong", "notchedmeditation", "whales", "mice", 
                    "screamingkids", "laughingkids", "notchedmusic", "creakingdoor", "teethbrushing", "frogs", "writing", "cicada")
#bird chirp = sparrows? // scribbling = writing?

#add the rest of the sounds comfortably
sounds_masking <- c(sounds_masking, grep("fm_", son_gr$sound, value = T))
sounds_masking <- c(sounds_masking, grep("_bp", son_gr$sound, value = T))
sounds_masking <- c(sounds_masking, grep("_notch", son_gr$sound, value = T))
sounds_masking <- c(sounds_masking, grep("tin_", son_gr$sound, value = T))


#write sound_mechanism into son
#if str from son_gr$sound is in sounds_masking, write son$sound_mechanism[i] = 1
for (i in 1:length(son$sound)){
  if (son$sound[i] %in% sounds_masking) {
    son$sound_mechanism[i] <- 1
  } else {
    son$sound_mechanism[i] <- 2
  }
}
son$sound_mechanism <- as.factor(son$sound_mechanism)


#write sound_mechanism into son_gr
for (i in 1:length(son_gr$sound)){
  if (son_gr$sound[i] %in% sounds_masking) {
    son_gr$sound_mechanism[i] <- 1
  } else {
    son_gr$sound_mechanism[i] <- 2
  }
}
son_gr$sound_mechanism <- as.factor(son_gr$sound_mechanism)


## ----------------------------------------------------------------------------
#add sound_origin: grouping sounds to natural and artificial
#natural sounds = 1, synthetic sounds = 2

#create a vector with all the natural sounds
sounds_natural <- c("sparrows", "seagulls", "churchbells", "kettle", "gong", "notchedmeditation", "whales", 
                  "mice", "screamingkids", "laughingkids", "notchedmusic", "creakingdoor", "teethbrushing", "frogs", "writing", "cicada",
                  "flappingflag", "tumblingrocks", "purringcat", "beachwaves", "didgeridoo", "desertwind", "rustlingleaves", "birdwings", 
                  "bees", "crowdbabble2", "bubbling", "bonfire", "waterfall", "applause", "rain", "shower")

#write sound_origin into son
for (i in 1:length(son$sound)){
  if (son$sound[i] %in% sounds_natural) {
    son$sound_origin[i] <- 1
  } else {
    son$sound_origin[i] <- 2
  }
}
son$sound_origin <- as.factor(son$sound_origin)


#write sound_origin into son_gr
for (i in 1:length(son_gr$sound)){
  if (son_gr$sound[i] %in% sounds_natural) {
    son_gr$sound_origin[i] <- 1
  } else {
    son_gr$sound_origin[i] <- 2
  }
}
son_gr$sound_origin <- as.factor(son_gr$sound_origin)


## ----------------------------------------------------------------------------
#add sound_matched: group sounds into matched and unmatched




## ----------------------------------------------------------------------------
#write csvs for beautified data and beautified data grouped by sound

write.csv2(son, file = "son_data.csv")
write.csv2(son_gr, file = "son_groupedby_sound.csv")



## ----------------------------------------------------------------------------
## PROBLEMS

#1: negative listen_time
#2: were sounds matched?
