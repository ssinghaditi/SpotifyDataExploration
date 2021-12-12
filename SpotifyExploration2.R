library(tidyverse)
library(moderndive)
library(GGally)
library(tidyverse)

load("/Users/aditi/Documents/BUAD 312/312_project_Spotify.RData")

spotify = spotify %>% 
  mutate(stream_cat=ifelse( log(streams) <13, "L", 
                            ifelse( log(streams) <14.4, "M", "H" )))

spotify_highstreams = spotify %>% 
  filter(stream_cat == "H")

# Loudness vs streams
ggplot(spotify, aes(x = loudness)) +
  geom_histogram(color = "white") +
  scale_x_continuous(breaks = seq(-50, 5, by = 5))

spotify = spotify %>% 
  mutate(loudness_cat = ifelse(loudness < quantile(spotify$loudness, 0.5), "quiet", "loud"))

ggplot(spotify, aes(x = loudness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge")

ggplot(spotify_highstreams, aes(x = loudness, y = streams)) +
  geom_point()

# Energy vs streams
ggplot(spotify, aes(x = energy)) +
  geom_histogram(color = "white")

spotify = spotify %>% 
  mutate(energy_cat = ifelse(energy < 0.5, "low energy", "high energy"))

ggplot(spotify, aes(x = energy_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") 

ggplot(spotify_highstreams, aes(x = energy, y = streams)) +
  geom_point() 

# Exploring relationships between the 2 variables
ggplot(spotify, aes(x = loudness_cat, fill = stream_cat)) +
  geom_bar(position = "dodge") +
  facet_wrap(~energy_cat)


# Modeling
lm1 = lm(streams ~ loudness*energy_cat, spotify)
summary(lm1)

lm1points = get_regression_points(lm1)
lm1points = lm1points %>% 
  mutate(stdres = rstandard(lm1))

lm1points %>% 
  ggplot(aes(y = stdres, x = streams_hat)) +
  geom_point()

lm1points %>% 
  ggplot(aes(sample = stdres)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red") +
  ggtitle("Normal QQ plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs (x = "Theoretical Quantile", y = "Standardized Residuals") 

