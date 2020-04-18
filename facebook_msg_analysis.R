rm(list = ls())
setwd("~/Documents/DataScience/messages") 

library(tidyverse)
library(jsonlite)
library(lubridate)
library(wesanderson)
library(zoo)
library(scales)
library(grid)
library(gridExtra)
library(emo)

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}
link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

dir <- "angie" # name of folder with messanges

df <- lapply(list.files(dir) %>% str_subset("json"), function(file) {
  json_file <- str_c(dir, "/", file)
  json_data <- fromJSON(paste(readLines(json_file), collapse = ""))
  
  messages <- json_data$messages
  
  if (is.null(messages$sticker)) {
    stickers <- FALSE
  } else {
    stickers <- messages$sticker %>%
      as_tibble() %>%
      mutate(uri = !is.na(uri)) %>%
      pull(uri)
  }
  
  if (is.null(messages$gifs)) {
    gifs <- FALSE
  } else {
    gifs <- ifelse(messages$gifs == "NULL", FALSE, TRUE)
  }
  
  if (is.null(messages$photos)) {
    photos <- FALSE
  } else {
    photos <- ifelse(messages$photos == "NULL", FALSE, TRUE)
  }
  
  
  tibble(
    sender = messages$sender_name,
    when = messages$timestamp_ms,
    msg = messages$content,
    gif = gifs,
    photo = photos,
    sticker = stickers
  ) %>%
    mutate(
      when = when %>%
        as.character() %>%
        str_sub(end = -4) %>%
        as.numeric() %>%
        as_datetime(),
      date = as_date(when)
    ) 
}) %>%
  reduce(rbind) %>%
  arrange(when) %>%
  mutate(sender = str_extract(sender, "\\w*"))


#Chronological timeline ------
p_06chrono <- df %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line(color = wes_palette(n = 2, name = "Moonrise2")[1]) +
  geom_line(aes(y = rollmean(n, 30, na.pad = TRUE))) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 10, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_date(date_breaks = "3 month",
               labels = function(date) {
                 month(date, label = TRUE) %>%
                   str_c(" ",year(date))
               }) +
  ggtitle("Chronological timeline")

#Messages per day of week ----
p_01msg_per_weekday <- df %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  mutate(weekday = weekdays(date) %>%
           factor(levels = c("Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday",
                             "Saturday", "Sunday"))) %>%
  group_by(weekday) %>%
  summarise(msg_per_day = mean(n)) %>%
  ggplot(aes(x = weekday, y = msg_per_day)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")[2]) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) + 
  ggtitle("Messages per day of week")

# Average message length -----
p_04msg_length <- df %>%
  mutate(msg_length = str_length(msg)) %>%
  filter(!is.na(msg_length)) %>%
  group_by(sender) %>%
  summarise(longest = max(msg_length),
            average = mean(msg_length) %>% round(),
            median = median(msg_length)) %>%
  ggplot(aes(x = sender, y = average, fill = sender)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Average message length")

# Longest message length -----
p_05longest_msg <- df %>%
  mutate(msg_length = str_length(msg)) %>%
  filter(!is.na(msg_length)) %>%
  group_by(sender) %>%
  summarise(longest = max(msg_length),
            average = mean(msg_length) %>% round(),
            median = median(msg_length)) %>%
  ggplot(aes(x = sender, y = longest, fill = sender)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Longest message length")

# No of mesges -----
p_03no_of_msg <- df %>%
  group_by(sender) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = sender, y = n, fill = sender)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Number of messages")

# Time of day ------
p_02time_of_day <- df %>%
  mutate(hr = hour(when),
         mn = minute(when),
         mn = ifelse(mn <= 30, 0, 30)) %>%
  mutate(hr = hr + mn/60) %>%
  group_by(hr) %>%
  summarise(n = n()) %>%
  mutate(hr = factor(hr)) %>%
  ggplot(aes(x = hr, y = n)) +
  geom_bar(fill = wes_palette(n = 2, name = "Moonrise2")[2],
           position = position_dodge(), stat = "identity") +
  coord_polar() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())  +
  scale_x_discrete(breaks = round(seq(min(0), max(23.5), by = 1),1)) +
  ggtitle("Conversation time")

# Gifs, photos, stickers ----
p_08gifs <- df %>%
  group_by(sender) %>%
  summarise(n = sum(gif))  %>%
  ggplot(aes(x = sender, y = n, fill = sender)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Number of gifs")

p_07photos <- df %>%
  group_by(sender) %>%
  summarise(n = sum(photo))  %>%
  ggplot(aes(x = sender, y = n, fill = sender)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Number of photos")

p_09stickers <- df %>%
  group_by(sender) %>%
  summarise(n = sum(sticker))  %>%
  ggplot(aes(x = sender, y = n, fill = sender)) +
  geom_col(fill = wes_palette(n = 2, name = "Moonrise2")) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Number of stickers")

# Most common words & emojis by person -----
common_words_rus <- read_csv("common_rus_words.csv") %>%
  pull()

emj <- emoji(list_emoji(), TRUE)

common_words <- df %>% 
  distinct(sender) %>% 
  pull() %>%
  lapply(function(psn) {
    df %>%
      filter(sender == psn) %>%
      pull(msg) %>%
      iconv("UTF-8", "ISO-8859-1") %>%
      str_to_lower() %>%
      str_split(" ") %>%
      unlist() %>%
      str_replace_all("[:punct:]", "") %>%
      str_subset('.') %>%
      table() %>%
      tibble(msg = names(.),
             count = unlist(.)) %>%
      mutate(sender = psn) %>%
      mutate(is_emoji = msg %in% trimws(emj)) %>%
      filter(!(msg %in% c(common_words_rus, 
                          "d", "ж", "i",
                          "<3", "😂😂", "😀😀",
                          "😂😂😂")
      )) %>%
      arrange(-count) %>%
      filter(is_emoji == FALSE) %>%
      slice(1:10)
  }) %>%
  reduce(rbind)  

common_emojis <- df %>% 
  distinct(sender) %>% 
  pull() %>%
  lapply(function(psn) {
    df %>%
      filter(sender == psn) %>%
      pull(msg) %>%
      iconv("UTF-8", "ISO-8859-1") %>%
      str_to_lower() %>%
      str_split(" ") %>%
      unlist() %>%
      str_replace_all("[:punct:]", "") %>%
      str_subset('.') %>%
      str_match_all(".") %>%
      unlist() %>%
      .[. %in% trimws(emj)] %>%
      table() %>%
      tibble(emj = names(.),
             count = unlist(.)) %>%
      mutate(sender = psn) %>%
      arrange(-count) %>%
      slice(1:10)
  }) %>%
  reduce(rbind)  %>%
  mutate(url = map_chr(emj, slowly(~emoji_to_link(.x), rate_delay(1))),
         label = link_to_img(url, 7))



p_10common_words_1 <- common_words %>%
  filter(sender == df %>% 
           distinct(sender) %>% 
           pull() %>%
           .[1]) %>%
  ggplot(aes(x = reorder(msg, count), y = -count)) +
  geom_bar(position = position_dodge(), stat = 'identity',
           fill = wes_palette(n = 2, name = "Moonrise2")[1]) + 
  coord_flip() + 
  scale_x_discrete(name = "", position = "top") +     
  scale_y_continuous(breaks = seq(0, -max(common_words$count), -200),
                     labels = seq(0, max(common_words$count), 200)) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  ggtitle(str_c("Words: ", df %>% 
                  distinct(sender) %>% 
                  pull() %>%
                  .[1]))


p_11common_words_2 <- common_words %>%
  filter(sender == df %>% 
           distinct(sender) %>% 
           pull() %>%
           .[2]) %>%
  ggplot(aes(x = reorder(msg, count), y = count)) +
  geom_bar(position = position_dodge(), stat = 'identity',
           fill = wes_palette(n = 2, name = "Moonrise2")[2]) + 
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(common_words$count), 200)) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  ggtitle(str_c("Words: ", df %>% 
                  distinct(sender) %>% 
                  pull() %>%
                  .[2]))
offset <- max(common_emojis$count) / 20

p_12common_emoji_1 <- common_emojis %>%
  filter(sender == df %>% 
           distinct(sender) %>% 
           pull() %>%
           .[1]) %>%
  ggplot(aes(x = reorder(emj, count), y = -count, label = label)) +
  geom_bar(position = position_dodge(), stat = 'identity',
           fill = wes_palette(n = 2, name = "Moonrise2")[1]) +
  geom_richtext(aes(y = - count - offset), fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt")
  ) + 
  coord_flip() + 
  scale_x_discrete(name = "", position = "top")  +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(x = NULL)  + 
  ggtitle(str_c("Top emojis: ", df %>% 
                  distinct(sender) %>% 
                  pull() %>%
                  .[1]))

p_13common_emoji_2 <- common_emojis %>%
  filter(sender == df %>% 
           distinct(sender) %>% 
           pull() %>%
           .[2]) %>%
  ggplot(aes(x = reorder(emj, count), y = count, label = label)) +
  geom_bar(position = position_dodge(), stat = 'identity',
           fill = wes_palette(n = 2, name = "Moonrise2")[2]) +
  geom_richtext(aes(y = count + offset), fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt")
  ) + 
  coord_flip() + 
  scale_x_discrete(name = "", position = "top")  +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(x = NULL)  + 
  ggtitle(str_c("Top emojis: ", df %>% 
                  distinct(sender) %>% 
                  pull() %>%
                  .[2]))

# Suur graafik ---
gs <- ls() %>%
  str_subset("p_") %>%
  lapply(function(gr) {
    gr %>%
      get() 
  })

lay <- rbind(c(rep(1, 8), rep(2, 4)),
             c(rep(3, 4), rep(4, 4), rep(5, 4)),
             c(rep(6,12)),
             c(rep(7, 4), rep(8, 4), rep(9, 4)),
             c(rep(10, 3), rep(11, 3), rep(12, 3), rep(13, 3)))



p <- grid.arrange(grobs=gs, layout_matrix = lay)

ggsave(str_c(dir, ".jpg"), p,
       width = 8, 
       height = 9)

