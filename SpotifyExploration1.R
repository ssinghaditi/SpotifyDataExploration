library(tidyverse)
library(moderndive)
library(GGally)
library(tidyverse)

load("/Users/aditi/Documents/BUAD 312/312_project_Spotify.RData")

# ------------------------------------------------------------------------------
# GRAPHING

# loudness, instrumentalness, speechiness, tempo, streams

summary(spotify)

spotify = spotify %>% 
  mutate(stream_cat=ifelse( log(streams) <13, "L", 
                            ifelse( log(streams) <14.4, "M", "H" )))

spotify_highstreams = spotify %>% 
  filter(stream_cat == "H")

# Tempo vs streams
ggplot(spotify, aes(x = tempo)) +
  geom_histogram(color = "white") +
  scale_x_continuous(breaks = seq(0, 250, by = 10))

spotify = spotify %>% 
  mutate(tempo_cat = ifelse(tempo < quantile(spotify$tempo, 0.5), "low tempo", "high tempo"))

ggplot(spotify, aes(x = tempo_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") 

# Loudness vs streams
ggplot(spotify, aes(x = loudness)) +
  geom_histogram(color = "white") +
  scale_x_continuous(breaks = seq(-50, 5, by = 5))

spotify = spotify %>% 
  mutate(loudness_cat = ifelse(loudness < quantile(spotify$loudness, 0.5), "quiet", "loud"))

ggplot(spotify, aes(x = loudness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge")

ggplot(spotify_highstreams, aes(x = loudness, fill = stream_cat)) +
  geom_bar()

ggplot(spotify_highstreams, aes(x = loudness, y = streams)) +
  geom_point()

# Instrumentalness vs streams
spotify$instrumentalness = as.numeric(spotify$instrumentalness)

ggplot(spotify, aes(x = instrumentalness)) +
  geom_histogram(color = "white") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))

spotify = spotify %>% 
  mutate(instrumentalness_cat = ifelse(instrumentalness < 0.5, "less instrumental", "more instrumental"))

ggplot(spotify, aes(x = instrumentalness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") 

# Speechiness vs streams
ggplot(spotify, aes(x = speechiness)) +
  geom_histogram(color = "white") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))

spotify = spotify %>% 
  mutate(speechiness_cat = ifelse(speechiness < 0.33, "less speech", ifelse(speechiness < 0.66, "average speech", "more speech")))

ggplot(spotify, aes(x = speechiness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") 

# Combining these new variables 
ggplot(spotify, aes(x = loudness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") +
  facet_wrap(~tempo_cat)

ggplot(spotify, aes(x = instrumentalness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") +
  facet_wrap(~speechiness_cat)

# ------------------------------------------------------------------------------
# MODELING

lm1 = lm(streams ~ loudness, spotify)
summary(lm1)

lm1_points = get_regression_points(lm1)
lm1_points = lm1_points %>% 
  mutate(stdres = rstandard(lm1))

lm1_points %>% 
  ggplot(aes(sample = stdres)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red")

spotify %>% 
  ggplot(aes(x = loudness, y = streams, color = tempo)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)








# Testing model with streams and all 4 variables - tempo isn't significant
lm1 = lm(streams ~ loudness + instrumentalness + speechiness + tempo, spotify)
summary(lm1)

# Testing model with streams and all variables excluding tempo - all variables significant
# R-squared: 0.01222
lm2 = lm(streams ~ loudness + instrumentalness + speechiness, spotify)
summary(lm2)

# Using categorical version of the variables
# R-squared: 0.01372
lm3 = lm(streams ~ loudness_cat + instrumentalness_cat + speechiness_cat, spotify)
summary(lm3)

lm3_points = get_regression_points(lm3)
lm3_points = lm3_points %>% 
  mutate(stdres = rstandard(lm3))

lm3_points %>% 
  ggplot(aes(sample = stdres)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red")

lm3_points %>% 
  ggplot(aes(y = stdres, x = streams_hat)) +
  geom_point()


lm4 = lm(streams ~ instrumentalness + speechiness, spotify)
summary(lm4)

lm4_points = get_regression_points(lm4)
lm4_points = lm4_points %>% 
  mutate(stdres = rstandard(lm4))

lm4_points %>% 
  ggplot(aes(sample = stdres)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red")
