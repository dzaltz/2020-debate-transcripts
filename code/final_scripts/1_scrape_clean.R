# Presidential and Vice Presidential Debates
# Script 1/x 
# get and clean data


# setup ####
library(tidyverse)
library(rvest) 

# scrape data_VP1 ####
url <- read_html("https://www.rev.com/blog/transcripts/kamala-harris-mike-pence-2020-vice-presidential-debate-transcript")

# extract relevant text from website
transcript_VP1 <- url %>% 
  html_nodes("#transcription p") %>% 
  html_text() %>% 
  tibble()


# clean_VP1 ####
transcript_VP1 <- transcript_VP1 %>% 
  separate(
    1, # select first column (in this case, the only column)
    into = c("A", "transcript"), # separate into two columns (temporary names)
    sep = "\n" # separated by new lines, denoted via regex \n
  ) 


transcript_VP1 <- transcript_VP1 %>%  
  separate(
    1, # select first column (which contains both speaker and time)
    into = c("speaker", "time"), # separate into new columns
    sep = ": " # separate by the colon after the speaker's name
  )


transcript_VP1$speaker <- str_squish(transcript_VP1$speaker) # rm space from names


transcript_VP1 <- transcript_VP1 %>% # rename speakers
  mutate(
    speaker = case_when(speaker == "Kamala Harris" ~ "KH",
                        speaker == "Mike Pence" ~ "MP",
                        speaker == "Susan Page" ~ "moderator")
  )


transcript_VP1 <- transcript_VP1 %>% # convert time stamps
  mutate(
    time = case_when(
      str_count(time, pattern = ":") == 1 ~ lubridate::ms(time),
      str_count(time, pattern = ":") == 2 ~ lubridate::hms(time))
  )



transcript_VP1$time[136:nrow(transcript_VP1)] <- # adjust time stamps from part 1 vs 2
  transcript_VP1$time[136:nrow(transcript_VP1)] + lubridate::ms("36:16")


transcript_VP1$time <- lubridate::ms(transcript_VP1$time, roll = TRUE) # reformat

transcript_VP1$time[1] <- lubridate::ms("00:00") # replace row 1 NA

transcript_VP1 <- transcript_VP1 %>% 
  mutate(transcript = str_replace_all(transcript, "’", "'"))


# scrape data_P1 ####
url <- read_html("https://www.rev.com/blog/transcripts/donald-trump-joe-biden-1st-presidential-debate-transcript-2020")

transcript_P1 <- url %>% 
  html_nodes("#transcription p") %>% 
  html_text() %>% 
  tibble()

# clean data_P1 ####
transcript_P1 <- transcript_P1[-180,] # remove blank line denoting the beginning of part 2


transcript_P1 <- transcript_P1 %>% # split columns
  separate(
    1,
    into = c("A", "transcript"),
    sep = "\n"
  ) 


transcript_P1$A[180] <- "Chris Wallace: (00:00)" # replace NA in col 1 of part 2

transcript_P1 <- transcript_P1 %>%  # split again to isolate time stamps
  separate(
    1,
    into = c("speaker", "time"),
    sep = ": "
  )

# rename speakers and reclass time stamp
transcript_P1 <- transcript_P1 %>% 
  mutate(
    speaker = as.factor(case_when(speaker == "Chris Wallace" ~ "moderator",
                                  speaker == "President Donald J. Trump" ~ "DT",
                                  speaker == "Vice President Joe Biden" ~ "JB")),
    time = case_when(str_count(time, pattern = ":") == 1 ~ lubridate::ms(time),
                     str_count(time, pattern = ":") == 2 ~ lubridate::hms(time))
  )




transcript_P1$time[180:nrow(transcript_P1)] <- # adjust part two time stamps
  transcript_P1$time[180:nrow(transcript_P1)] + lubridate::ms("24:30")

# replace apostrophes from transcript with ones that will be recognized by useful functions
transcript_P1 <- transcript_P1 %>% 
  mutate(transcript = str_replace_all(transcript, "’", "'"))

# clean up space
rm(url)
