# Load required packages 
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(skimr)
library(ggthemes)
library(ineq)
library(scales)


## Set Seed ----
# set seed so that results are the same no matter where we run it 
set.seed(111)


# Custom theme ----
theme_project <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%
    theme(
      text = element_text(family='Pt Mono'),
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 8), family = 'K2D', face = 'bold', size = 15),
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 8), family = 'K2D', face = 'bold', size = 15, angle = 90),
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 9),
      axis.text.y = element_text(face='bold', size = 9),
      legend.text = element_text(size = 5, family = 'K2D'),
      legend.title = element_text(size = 10, family = 'Proxima Nova', face = 'bold'),
      panel.background = element_rect('grey98'),
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=15, t = 10), face='bold', size=25, hjust = 0.5, family = 'Proxima Nova'),
      plot.subtitle=element_text(size=10, hjust = 0, margin = margin(b = 10), family = 'Proxima Nova'),
      plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"),
      panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
      legend.key = element_rect(fill = 'white', color = 'red'),
      legend.background = element_rect(fill = 'whitesmoke'),
      legend.box.background = element_rect(color = 'black', fill = 'red'), 
      panel.spacing = unit(10, 'points')
    )
}


# Set directory 
setwd('/Users/davidetissino/Desktop/PoliMi/Stats/Project')

# Upload the dataset and show the first lines.
df <- read.csv("hf_dataset.csv") %>% 
  .[, -1]

head(df)



# EDA ####
colnames(df)
glimpse(df)

# Overview of DF -- create summary
skim(df, where(is.numeric))
skim(df, where(is.character))

# Missing values
missing_values <- colSums(is.na(df))
print("Missing values in each column:")
print(missing_values)


## -------------------------------------------------------------------------##
### Duplicate Problem ####
## PROBLEM: there are duplicate songs which may belong to different genres
# Additionally, we have exactly 114 genres --> each genre has a sample of exactly 1000 songs 


# APPROACH #1
# Addressing duplicates with slightly different genres (as alt-rock vs alternative vs rock)
# basically get each genre's avg popularity 
# then, for each duplicate track id we keep only song with most popular genre
genre_avg_popularity <- df %>% 
  group_by(track_genre) %>% 
  summarise(avg_pop = mean(popularity, na.rm = TRUE))

df_clean_avg <- df %>% 
  left_join(genre_avg_popularity, by = 'track_genre') %>% 
  group_by(track_id) %>% 
  arrange(desc(avg_pop)) %>% 
  slice(1) %>% 
  ungroup()




# APPROACH #2 (makes most sense)
# Create macro-genres based on classic genre 
# then keep only most popular macro-genre for each duplicate track
df_clean <- df %>%
  mutate(
    macro_genre = case_when(
      str_detect(track_genre, "(?i)ambient|chill|sleep|study|new-age|sad|romance|children|kids") ~ "Mood",
      str_detect(track_genre, "(?i)pop|club|party|disco|happy|^dance$|j-dance") ~ "Pop & Dance",
      str_detect(track_genre, "(?i)electronic|techno|house|dubstep|edm|trance|electro|drum-and-bass|hardstyle|idm|trip-hop|breakbeat|garage|industrial") ~ "Electronic",
      str_detect(track_genre, "(?i)rock|metal|punk|grunge|emo|goth|hardcore|grindcore|indie") ~ "Rock & Alternative",
      str_detect(track_genre, "(?i)rap|trap|r-n-b|soul|funk|groove") ~ "Rap & Trap",
      str_detect(track_genre, "(?i)latin|reggaeton|salsa|reggae|ska|dancehall|^dub$|brazil|forro|mpb|pagode|samba|sertanejo|tango|spanish") ~ "Latin",
      str_detect(track_genre, "(?i)country|folk|bluegrass|acoustic|honky-tonk|songwriter|guitar") ~ "Country & Folk",
      str_detect(track_genre, "(?i)classical|opera|piano") ~ "Classical",
      str_detect(track_genre, "(?i)jazz|blues|gospel") ~ "Jazz & Blues", 
      str_detect(track_genre, "(?i)hip-hop") ~ "Hip-Hop", 
      TRUE ~ "Other"
    )
  )


# Identify avg popularity for each macro genre
macro_avg_popularity <- df_clean %>% 
  group_by(macro_genre) %>% 
  summarise(avg_pop = mean(popularity, na.rm = TRUE))

# Keep only most popular instance based on highest-popularity macro genre
df_clean <- df_clean %>% 
  left_join(macro_avg_popularity, by = "macro_genre") %>% 
  group_by(track_id) %>% 
  arrange(desc(avg_pop)) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(-avg_pop)





# QUESTION 1 #### 
# How is commercial success distributed across the Spotify catalog, 
# and is popularity highly concentrated among a few hits?

### Popularity Distribution -----
ggplot(df_clean, aes(x = popularity)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_project() + 
  labs(title = "Distribution of Track Popularity", x = "Popularity Score", y = "Count")
# --> simple histogram plotting popularity score and song count --> right skewed


### Lorenz Curve & Gini Index ------
gini_pop <- ineq(df_clean$popularity, type = "Gini")
print(paste("Gini Index:", round(gini_pop, 3))) # 0.355
# --> Gini index of 0.355, more than 10k songs with popularity = 0
# --> can assess differences between 0-pop songs and the other 

# Generate the Lorenz Curve data
lc_pop <- Lc(df_clean$popularity)

# Convert to a dataframe for ggplot2
lorenz_df <- data.frame(
  Cumulative_Tracks = lc_pop$p,
  Cumulative_Popularity = lc_pop$L
)

# Plot the Lorenz Curve
ggplot(lorenz_df, aes(x = Cumulative_Tracks, y = Cumulative_Popularity)) +
  # shade the Gini area
  geom_ribbon(aes(ymin = Cumulative_Popularity, ymax = Cumulative_Tracks), fill = "steelblue", alpha = 0.5) +
  # 45-degree line of equality
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = 1) +
  annotate(
    'text', 
    x = 0.5, 
    y = 0.5, 
    label = 'Line of Equality', 
    angle = 28,
    vjust = - 0.5,
    fontface = 'italic', 
    size = 9, 
    color = 'black'
  ) +  
  # Lorenz Curve
  geom_line(color = "darkblue", linewidth = 1.2) +
  theme_project() +
  theme(
    plot.subtitle = element_text(size = 15, hjust = 0.5)
  ) + 
  labs(
    title = "The Lorenz Curve of Track Popularity",
    subtitle = paste("Gini Index =", round(gini_pop, 4)),
    x = "Cumulative Share of Tracks",
    y = "Cumulative Share of Total Popularity"
  ) +
  annotate(
    'text', 
    x = 0.6, 
    y = 0.25, 
    label = 'Lorenz Curve', 
    angle = 32,
    vjust = - 0.5,
    fontface = 'italic', 
    size = 9, 
    color = 'darkblue'
  ) + 
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format())

# A Gini index of 0.355 indicates a moderate level of market concentration. 
# Rather than an absolute monopoly where a fraction of tracks holds all the value, 
# this metric reveals a tiered ecosystem. 
# It mathematically proves that while top-performing assets command a significant market premium, 
# there is a functional "middle class" of tracks successfully retaining baseline popularity






### Flops vs Hits -----
# Concentration and characteristics
df_clean <- df_clean %>%
  mutate(commercial_status = 
           case_when(
             popularity == 0 ~ 'Flop', 
             popularity >= 75 ~ 'Hit', 
             TRUE ~ 'Mid'
           ))

# pivot  features from wide to long
df_long <- df_clean %>%
  select(commercial_status, danceability, energy, loudness, speechiness, 
         acousticness, liveness, valence) %>%
  pivot_longer(
    cols = -commercial_status, 
    names_to = "audio_feature", 
    values_to = "value"
  )

# Comparison of summary stats
flop_summary <- df_clean %>%
  group_by(commercial_status) %>%
  summarise(
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_acousticness = mean(acousticness, na.rm = TRUE),
    count = n()
  )
print(flop_summary)


# Faceted density plot
df_long %>% 
  filter(commercial_status != 'Mid') %>%
  ggplot(aes(x = value, fill = commercial_status, color = 'black')) +
  geom_density(adjust = 1.5, alpha = 0.7, color = 'black') + 
  facet_wrap(~ audio_feature, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("Hit" = "firebrick2", "Flop" = "forestgreen")) +
  theme_project() +
  labs(
    title = "The Anatomy of a Flop",
    subtitle = "Acoustic Feature Distributions",
    x = "Feature Value",
    y = "Density",
    fill = "Commercial Status"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )







### The Duration Effect ========
df_clean <- df_clean %>% mutate(
  duration_min = duration_ms / 60000,
  duration_sec = duration_ms / 1000)

summary(df_clean$duration_min)

# appropriately order songs from Flop --> Mid --> Hit
df_clean <- df_clean %>% 
  mutate(
    commercial_status = fct_relevel(commercial_status, 'Flop', 'Mid', 'Hit')
  )

# boxplots in seconds
ggplot(df_clean, aes(y = duration_sec, x = commercial_status, fill = commercial_status)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(0, 500)) + # to ignore extreme outliers
  theme_project() +
  labs(title = "Commercial Status and Duration", y = "Duration (Seconds)", x = 'Commercial Status')


# summary stats (as box plot) SECONDS
duration_summary <- df_clean %>%
  group_by(commercial_status) %>%
  summarise(
    count = n(),
    min_duration = min(duration_sec, na.rm = TRUE),
    q25_duration = quantile(duration_sec, 0.25, na.rm = TRUE),
    median_duration = median(duration_sec, na.rm = TRUE),
    mean_duration = mean(duration_sec, na.rm = TRUE),
    q75_duration = quantile(duration_sec, 0.75, na.rm = TRUE),
    max_duration = max(duration_sec, na.rm = TRUE)
  ) %>%
  # rounding to all numeric columns
  mutate(across(where(is.numeric), ~ round(., 2)))

print(as.data.frame(duration_summary))



### The Tempo Effect ======== 
ggplot(df_clean, aes(y = tempo, x = commercial_status, fill = commercial_status)) +
  geom_boxplot(outlier.alpha = 0.1) +
  #coord_cartesian(ylim = c(0, 7)) + # Zoom in to ignore extreme outliers
  theme_project() +
  labs(title = "Track Tempo: Flops vs Active", y = "Tempo (BPM)", x = 'Commercial Status')




### Explicit vs non-Explicit -----
# pivot the numeric features from wide to long
df_long_explicit <- df_clean %>%
  select(explicit, danceability, energy, loudness, speechiness, 
         acousticness, liveness, valence) %>%
  pivot_longer(
    cols = -explicit, 
    names_to = "audio_feature", 
    values_to = "value"
  )

# faceted density plot for audio features
ggplot(df_long_explicit, aes(x = value, fill = explicit)) +
  geom_density(alpha = 0.8, color = 'black') + 
  facet_wrap(~ audio_feature, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("False" = "firebrick2", "True" = "forestgreen")) +
  theme_project() +
  labs(
    title = "Explicit Content Effect",
    subtitle = "Acoustic Feature Distributions", 
    x = "Feature Value",
    y = "Density",
    fill = "Explicit Content"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )


# Popularity distribution for explicit vs non-explicit content
ggplot(df_clean, aes(x = popularity, fill = explicit)) +
  geom_histogram(bins = 30, color = "white") +
  scale_fill_manual(values = c("False" = "steelblue", "True" = "firebrick2")) +
  facet_wrap(~ explicit, labeller = as_labeller(c("False" = "Clean", "True" = "Explicit"))) +
  theme_project() +
  # Remove the legend since the facet titles already explain the split
  theme(legend.position = "none") + 
  labs(title = "Clean vs. Explicit Popularity", 
       x = "Popularity Score", 
       y = "Count")


# Boxplot distribution by explicit content 
ggplot(df_clean, aes(x = explicit, y = popularity, fill = explicit)) +
  geom_boxplot(alpha = 0.8, outlier.color = "grey50", outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set1") +
  theme_project() +
  labs(title = "Popularity by Content Type",
       x = "Explicit Content", y = "Popularity Score") +
  theme(legend.position = "none") 


