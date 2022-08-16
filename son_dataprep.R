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
    mean_pre_loudness = mean(pre_loudness),
    sd_pre_loudness = sd(pre_loudness),
    mean_pre_distress = mean(pre_distress),
    sd_pre_distress = sd(pre_distress),
    mean_pause_count = mean(pause_count, na.rm = T),
    sd_pause_count = sd(pause_count, na.rm = T),
    mean_listen_time = mean(listen_time), #not correct, because there are negative values
    sd_listen_time = sd(listen_time),
    mean_longest_listen_time = mean(longest_listen_time),
    sd_longest_listen_time = sd(longest_listen_time),
    mean_post_loudness = mean(post_loudness, na.rm = T),
    sd_post_loudness = sd(post_loudness, na.rm = T),
    mean_delta_sound_rating = mean(delta_sound_rating, na.rm = T),
    sound_origin = NA,
    sound_mechanism = NA
  )


#add sound_energy: grouping sounds to tin+ and tin-

#create a vector with half the tin_plus sounds
sounds_tin_plus <- c("sparrows", "seagulls", "churchbells", "kettle", "gong", "notchedmeditation", "whales", "mice", 
                    "screamingkids", "laughingkids", "notchedmusic", "creakingdoor", "teethbrushing", "frogs", "writing", "cicada")
#bird chirp = sparrows? // scribbling = writing?

#add the rest of the tin_plus sounds comfortably
sounds_tin_plus <- c(sounds_tin_plus, grep("fm_", son_gr$sound, value = T))
sounds_tin_plus <- c(sounds_tin_plus, grep("_bp", son_gr$sound, value = T))
sounds_tin_plus <- c(sounds_tin_plus, grep("_notch", son_gr$sound, value = T))
sounds_tin_plus <- c(sounds_tin_plus, grep("tin_", son_gr$sound, value = T))


#write sound_energy into son
#if str from son_gr$sound is in sounds_tin_plus, write son$sound_energy[i] = 1
for (i in 1:length(son$sound)){
  if (son$sound[i] %in% sounds_tin_plus) {
    son$sound_energy[i] <- "tin+"
  } else {
    son$sound_energy[i] <- "tin-"
  }
}
son$sound_energy <- as.factor(son$sound_energy)


#write sound_energy into son_gr
for (i in 1:length(son_gr$sound)){
  if (son_gr$sound[i] %in% sounds_tin_plus) {
    son_gr$sound_energy[i] <- "tin+"
  } else {
    son_gr$sound_energy[i] <- "tin-"
  }
}
son_gr$sound_energy <- as.factor(son_gr$sound_energy)



## ----------------------------------------------------------------------------
#add sound_origin: grouping sounds to natural and synthetic

#create a vector with all the natural sounds
sounds_natural <- c("sparrows", "seagulls", "churchbells", "kettle", "gong", "notchedmeditation", "whales", 
                  "mice", "screamingkids", "laughingkids", "notchedmusic", "creakingdoor", "teethbrushing", "frogs", "writing", "cicada",
                  "flappingflag", "tumblingrocks", "purringcat", "beachwaves", "didgeridoo", "desertwind", "rustlingleaves", "birdwings", 
                  "bees", "crowdbabble2", "bubbling", "bonfire", "waterfall", "applause", "rain", "shower")

#write sound_origin into son
for (i in 1:length(son$sound)){
  if (son$sound[i] %in% sounds_natural) {
    son$sound_origin[i] <- "nat"
  } else {
    son$sound_origin[i] <- "syn"
  }
}
son$sound_origin <- as.factor(son$sound_origin)


#write sound_origin into son_gr
for (i in 1:length(son_gr$sound)){
  if (son_gr$sound[i] %in% sounds_natural) {
    son_gr$sound_origin[i] <- "nat"
  } else {
    son_gr$sound_origin[i] <- "syn"
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
## PROBLEMS / OPEN TASKS

#1: negative listen_time in data set
#2: create variable in data set son that contains tinnitus frequency (=matching) values
