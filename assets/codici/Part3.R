
# load packages
library(ggfortify)
library(factoextra) # Great for clean PCA visuals
library(tidyverse)
library(corrplot) 
library(RColorBrewer)
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


## Clean Duplicates ----
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



##### ---------------------------------------------------------------------------- ###
# PCA #### 
# Select only the numeric acoustic features
# We must scale the data since audio features have completely different units
numeric_vars_pca <- df_clean %>% 
  select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)

pca_res <- prcomp(numeric_vars_pca, scale. = TRUE)


## Scree Plot ----
# Visualizing how much information is compressed into the principal components
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 50),
         barfill = "steelblue", barcolor = "black",
         main = "Scree Plot: Proportion of Variance Explained",
         xlab = "Principal Components", ylab = "Percentage of Explained Variances") +
  theme_project() +
  theme(plot.title = element_text(face = "bold", size = 25))


# Feature Loadings: What do the axes represent?
# We extract the rotation matrix to see the exact weights of PC1 and PC2
print(round(pca_res$rotation[, 1:2], 3))



# Extract the raw mathematical coordinates from the PCA model
pca_data <- as.data.frame(pca_res$x)

# Attach the engineered macro-genres (Ensure row alignment is perfect)
pca_data$macro_genre <- df_clean$macro_genre



# Filtering
# We select a mix of acoustic/analog genres and digital/highly produced genres
target_macros <- c("Classical", "Country & Folk", "Pop & Dance", "Rap & Trap", "Electronic")
filtered_pca_data <- pca_data %>% filter(macro_genre %in% target_macros)



## Reality Check Plot ---- 
ggplot(filtered_pca_data, aes(x = PC1, y = PC2, color = macro_genre)) +
  geom_point(alpha = 0.3, size = 0.8) +
  # Draw the 95% confidence boundaries
  stat_ellipse(level = 0.95, linetype = "dashed", linewidth = 1) + 
  theme_project() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Reality Check: Human Labels vs. Mathematical Reality",
    subtitle = "95% Confidence Ellipses: Algorithmic overlap of Macro-Genres",
    x = "PC1", 
    y = "PC2",
    color = "Market Segment"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )



# THE REALITY CHECK ####
# Human labels vs. PCA results

# Prepare projection data
pca_df <- as.data.frame(pca_res$x[, 1:2]) %>%
  mutate(genre = df_clean$macro_genre)

# Compare some polarized genres to see if they overlap in the PCA space
target_genres <- c("Classical", "Country & Folk", "Pop & Dance", "Rap & Trap", "Electronic")

colnames(pca_df)[3] <- 'Genre'


## PCA Plot ####
pca_df %>%
  filter(Genre %in% target_genres) %>%
  ggplot(aes(x = PC1, y = PC2, color = Genre)) +
  geom_density_2d(alpha = 0.5) +
  stat_ellipse(aes(fill = Genre), geom = "polygon", alpha = 0.1) +
  theme_project() +
  theme(
    legend.title = element_text(size = 13, face = 'bold'), 
    legend.text = element_text(size = 8), 
    plot.subtitle = element_text(size = 15)
  ) + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "The Reality Check: Genre Clustering",
       subtitle = "If genres occupy unique spaces, the labels are mathematically grounded",
       x = "PC1", 
       y = "PC2")




### COMMENTS 
# (long but extensive)
# If you look at this density plot, you'll see a tangled web of contour lines on the left side of the axis. That mess is exactly what we want to highlight. Our marketing teams sell Pop & Dance, Rap & Trap, and Electronic as completely different commercial verticals. But when we strip away the human bias and look at the topological density of the raw audio math, those genres sit on top of each other. To the algorithm, they are the exact same acoustic product. If we are trying to diversify our investments, signing an Electronic artist and a Pop artist doesn't lower our risk—we are just doubling down on the exact same sonic profile.




