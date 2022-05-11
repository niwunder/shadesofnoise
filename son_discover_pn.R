###########################
## Beautify the Data set ##
###########################

library(tidyverse)


# load data ---------------------------------------------------------------

son <- read.csv("/Users/shanti/Documents/GitHub/uniti-whos-using-the-apps/data/son.csv")

## ----------------------------------------------------------------------------
#add delta_sound rating to data frame
#negative delta means pre_loudness > post_loudness -> tinnitus reduction

son <- rename(son, "post_loudness" = sound_rating)
son$post_loudness_true <- son$pre_loudness - (son$post_loudness - 50)
son$delta_sound_rating <- post_loudness_true - son$pre_loudness

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
son$favourite <- as.logical(son$favourite)


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
#son$sound_mechanism <- as.factor(son$sound_mechanism)

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
#son$sound_origin <- as.factor(son$sound_origin)


## ----------------------------------------------------------------------------
#create df son_gr that groups son by 64 sounds

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
    sound_mechanism = mean(sound_mechanism, na.rm = T), #as.factor?
    sound_origin = mean(sound_origin, na.rm = T), #as.factor?
    mean_delta_sound_rating = mean(delta_sound_rating, na.rm = T)
  )



## ----------------------------------------------------------------------------
#problem: negative listen_time
#write Vishnu or Carsten?



write.csv2(son, file = "son_data.csv")
write.csv2(son_gr, file = "son_groupedby_sound.csv")
