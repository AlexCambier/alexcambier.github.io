# load packages
library(ggfortify)
library(factoextra) # Great for clean PCA visuals
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


# DATA PREPARATION ####
# Set directory 
setwd('/Users/davidetissino/Desktop/PoliMi/Stats/Project')


# Upload the dataset and show the first lines.
df <- read.csv("hf_dataset.csv") %>% 
  .[, -1]

head(df)


### Clean Duplicates ----
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



# QUESTION 2 #### 
# Covariance and interactions between audio features 

### Correlation Matrix -----
numeric_vars <- df_clean %>% 
  select(popularity, danceability, energy, loudness, acousticness, instrumentalness, valence, tempo)

cor_matrix <- cor(numeric_vars, use = "complete.obs")

target_correlations <- cor_matrix[,"popularity"] %>%
  sort(decreasing = TRUE)
print(target_correlations)

### Matrix Plot ----
corrplot(cor_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45,
         title = "Correlation of Audio Features", 
         addCoef.col = 'black', number.digits = 2, diag = T, number.cex = 1.3)


# --> results: high correlation between energy and loudness (+0.76)
# --> negative correlation between energy and acousticness (-0.73)
# --> interesting to see how different macro-genres compare


# hard-code colors for specific genres
genre_colors <- c(
  "Electronic"    = "#2c3e50",
  "Hip-Hop"       = "#c0392b",
  "Pop & Dance"   = "#e056fd",
  "Mood"          = "#2980b9",
  "Rock & Alternative"  = "#8B0A50",
  "Country & Folk"      = "#f1c40f",
  "Latin"         = "#228B22",
  "Jazz & Blues"  = "#d35400",
  "Classical"     = "#1abc9c",
  "Rap & Trap"    = "#9b59b6",
  "Other"         = "#7f8c8d"
)


# RELEVANT INSIGHTS ####
### Loudness vs Energy (Positive Covariance) ----
# Calculate r for each genre
cor_labels_pos <- df_clean %>%
  group_by(macro_genre) %>%
  summarise(r = cor(loudness, energy, use = "complete.obs")) %>%
  arrange(desc(r)) %>% 
  slice(1:8) %>%
  mutate(label_text = paste0("r = ", round(r, 2)))



# Extract the names of those 8 genres in their newly sorted order
top_8_genres <- cor_labels_pos$macro_genre

# 2. Filter the main dataset and lock in the factor levels
df_filtered <- df_clean %>%
  filter(macro_genre %in% top_8_genres) %>%
  # This is the magic line: it forces ggplot to respect the r-value order
  mutate(macro_genre = factor(macro_genre, levels = top_8_genres))


# Plot
plot_pos <- ggplot(df_filtered, aes(x = loudness, y = energy, color = macro_genre)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.7, se = FALSE) +
  # Add the labels
  geom_label(data = cor_labels_pos, aes(x = -Inf, y = Inf, label = label_text), 
            hjust = -0.2, vjust = 1.5, inherit.aes = FALSE, fontface = "bold", size = 3.5) +
  facet_wrap(~ macro_genre, scales = "fixed") +
  scale_color_manual(values = genre_colors) +
  theme_project() +
  labs(
    title = "The Shifting Nature of Audio Physics",
    subtitle = "Positive Covariance: Energy vs. Loudness",
    x = "Loudness (dB)", y = "Energy"
  ) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 10))

plot_pos




### Energy vs Acousticness (Negative Covariance) ----

# Calculate r for each genre
cor_labels_neg <- df_clean %>%
  group_by(macro_genre) %>%
  summarise(r = cor(energy, acousticness, use = "complete.obs")) %>%
  arrange(r) %>% 
  slice(1:8) %>%
  mutate(label_text = paste0("r = ", round(r, 2)))


# Extract the names of those 8 genres in their newly sorted order
bottom_8_genres <- cor_labels_neg$macro_genre

# 2. Filter the main dataset and lock in the factor levels
df_filtered_neg <- df_clean %>%
  filter(macro_genre %in% bottom_8_genres) %>%
  # This is the magic line: it forces ggplot to respect the r-value order
  mutate(macro_genre = factor(macro_genre, levels = bottom_8_genres))




#  Plot
plot_neg <- ggplot(df_filtered_neg, aes(x = acousticness, y = energy, color = macro_genre)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.7, se = FALSE) +
  geom_label(data = cor_labels_neg, aes(x = Inf, y = Inf, label = label_text), 
            hjust = 1.2, vjust = 1.5, inherit.aes = FALSE, fontface = "bold", size = 3.5) +
  facet_wrap(~ macro_genre, scales = "fixed") +
  scale_color_manual(values = genre_colors) +
  theme_project() +
  labs(
    title = "The Acoustic Ceiling",
    subtitle = "Negative Covariance: Energy vs. Acousticness",
    x = "Acousticness", y = "Energy"
  ) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 10))


plot_neg


# "This proves that our A&R and production teams cannot blindly apply mainstream pop formulas to niche acquisitions. A loud jazz track doesn't equal high energy; it just equals bad jazz. By mapping exactly how these covariance structures bend per genre, we know exactly what acoustic profile to optimize for depending on the target market."

