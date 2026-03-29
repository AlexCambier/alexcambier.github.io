
# Packages Loading
library(tidyverse)
library(caret)
library(glmnet)
library(broom)


# Custom theme ----
theme_project <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%
    theme(
      text = element_text(family='Pt Mono'),
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 8), family = 'K2D', face = 'bold', size = 15),
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 8), family = 'K2D', face = 'bold', size = 15, angle = 90),
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13),
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


# set directory 
setwd("/Users/davidetissino/Desktop/PoliMi/Stats/Project")

# set seed to reproduce results 
set.seed(123)

# Dataset ----
raw_data <- read_csv("hf_dataset.csv") %>%
  .[, -1]



# Deduplication ----
# Create macro-genres based on classic genre 
# then keep only most popular macro-genre for each duplicate track
raw_data <- raw_data %>%
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
macro_avg_popularity <- raw_data %>% 
  group_by(macro_genre) %>% 
  summarise(avg_pop = mean(popularity, na.rm = TRUE))

# Keep only most popular instance based on highest-popularity macro genre
raw_data <- raw_data %>% 
  left_join(macro_avg_popularity, by = "macro_genre") %>% 
  group_by(track_id) %>% 
  arrange(desc(avg_pop)) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(-avg_pop)



# PREPARATION ####
## Numeric Attributes ----
# keep only numerical attributes (no key or mode, mess the results) 
df <- raw_data %>% 
  # select only relevant attributes
  select(popularity, danceability, energy, loudness, speechiness, 
         acousticness, instrumentalness, liveness, valence, tempo, explicit) %>%
  mutate(explicit = as.numeric(explicit)) %>% # Convert boolean to 0/1 numeric
  drop_na()



## Standardization ----
df_scaled <- df %>%
  mutate(across(where(is.numeric) & !popularity, scale))


# MULTIPLE LINEAR REGRESSION ####
# --> quantify the isolated impact of each feature on popularity
baseline_model <- lm(popularity ~ ., data = df_scaled)

## Coefficients ----
summary(baseline_model)
coefficients <- tidy(baseline_model)


# R-squared with MLR = 0.03301 --> 3.3% 


# Interpretation Example, focus on two most positive/negative:
# --> The 'danceability' and 'explicit' coefficient (+1.7, +1) represent a "Premium"
# --> The 'instrumentalness' and 'valence' coefficients represent a "penalty"
print("Baseline Coefficients:")
print(coefficients %>% filter(term %in% c("danceability", "explicit", "instrumentalness", "valence", "speechiness")))



# Visual representation of MLR results 
# plots all the terms and their estimates (outside of intercept)
mlr_plot_data <- coefficients %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate)))


# plot preliminary MLR findings
ggplot(mlr_plot_data, aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Baseline Valuation: Unpenalized Drivers",
    subtitle = "Multiple Linear Regression Coefficients (All Features)",
    x = "Audio Feature", 
    y = "Estimated Impact on Popularity"
  ) +
  theme_project() +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "tomato"), 
    labels = c("Penalty", "Premium"), 
    name = "Valuation Impact"
  ) +
  theme(
    legend.title = element_text(size = 13, face = 'bold'), 
    legend.text = element_text(size = 8), 
    plot.subtitle = element_text(size = 15, hjust = 0.3), 
    plot.title = element_text(hjust = -0.1)
  ) + 
  geom_label(
    aes(x = reorder(term, estimate), 
        label = round(estimate, 2)), 
    fontface = 'bold', color = 'black', fill = 'white')





# COMMENT: 
# R-squared with MLR = 0.03301 --> 3.3% 
# The market pays a high premium for tracks you can dance to (+1.7) and explicit content (+1.1). 
# Conversely, it violently penalizes instrumental tracks (-2.8) and speech-heavy tracks (-1.7)



# CROSS-VALIDATION ####
## K-Fold Cross-Validation ----

# IDEA: take baseline set by MLR and add interaction between energy and loudnes/acousticness
# we found in question 2 the most + and - relationship among these
train_control <- trainControl(method = "cv", number = 10) 

# run the model
noise_model <- train(
  popularity ~ . + (loudness * energy) + (acousticness * energy),
  data = df_scaled,
  method = "lm",
  trControl = train_control
)

# Performance Analysis (R-squared and RMSE)
print(noise_model$results)

# COMMENT: 
# R-squared with CV = 0.0443 --> 4.43%, gained about 1% from MLR
# We took the exact geometrical relationships established in our exploratory data analysis and built a cross-validated predictive model around them. 
# While these specific sonic interactions did improve our predictive accuracy, forecasting a hit purely on audio physics still caps out at a 4.4% success rate. 
# The acoustic envelope is a prerequisite for a hit, but it is the unmeasured variables—marketing budgets, playlist curation, and cultural trends—that make up the other 95.6%


# Create a simple dataframe for the R-Squared breakdown
cv_variance <- tibble(
  Component = c("Audio Physics (Explained)", "Market Noise & Culture (Unexplained)"),
  Percentage = c(4.43, 95.57) # Using the 4.4% from your CV output
)



# Build the Variance Breakdown Bar
ggplot(cv_variance, aes(x = "", y = Percentage, fill = Component)) +
  geom_bar(stat = "identity", width = 0.5, color = 'black') +
  coord_flip() + # Makes it a sleek horizontal progress bar
  geom_label(aes(label = paste0(Percentage, "%"), group = Component), 
             position = position_stack(vjust = 0.5), 
             fontface = "bold", color = "black",  fill = 'white', size = 7, show.legend = FALSE) +
  labs(
    title = "The Reality of Market Noise",
    subtitle = "10-Fold CV R-Squared = 0.0443",
    x = NULL,
    y = "Percentage of Variance (%)"
  ) +
  scale_fill_manual(values = c("Audio Physics (Explained)" = "#1DB954", "Market Noise & Culture (Unexplained)" = "grey70")) +
  theme_project() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 13, face = 'bold'), 
    legend.text = element_text(size = 8), 
    plot.subtitle = element_text(size = 15, hjust = 0.5), 
    #plot.title = element_text(margin = margin(b=5))
  )





# LASSO REGRESSION ####
# We found that basically 95% of our model is noise 
# We applied a Lasso penalty to the algorithm, mathematically forcing it to drop any acoustic feature that wasn't undeniably robust.
# Prepare the feature matrix and the target vector
x <- model.matrix(popularity ~ ., df_scaled)[,-1]
y <- df_scaled$popularity

# Run Cross-Validation to find the optimal lambda (regularization strength)
cv_lasso <- cv.glmnet(x, y, alpha = 1) # alpha=1 indicates Lasso regression

# Visualize the coefficient shrinkage plot
plot(cv_lasso)


# Extract non-zero coefficients at the optimal lambda (using lambda.1se for parsimony/simplicity)
final_drivers <- coef(cv_lasso, s = "lambda.1se") %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  rename(Coefficient = 2) %>%
  # keep only the coefficient >= 0.1 in absolute value, meaningless otherwise
  filter(abs(Coefficient) >= 0.1 & Feature != "(Intercept)") %>%
  arrange(desc(abs(Coefficient)))


# A final plot to visualize the importance of surviving features
ggplot(final_drivers, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Drivers of Commercial Success",
       subtitle = "Features surviving Lasso shrinkage, with |coefficient| > 0.1",
       x = "Audio Feature", 
       y = "Impact on Popularity Score") +
  theme_project() +
  scale_fill_manual(
    values = c("TRUE" = "#1DB954", "FALSE" = "#E91E63"), 
    labels = c("Penalty", "Premium"), 
    name = "Effect Type") +
  theme(
    legend.title = element_text(size = 13, face = 'bold'), 
    legend.text = element_text(size = 8), 
    plot.subtitle = element_text(size = 15, hjust = 0.3), 
    plot.title = element_text(hjust = 0.3)
  ) + 
  geom_label(
    aes(x = reorder(Feature, Coefficient,), 
        label = round(Coefficient, 2)), 
    fontface = 'bold', color = 'black', fill = 'white')




# Extract the fraction of deviance explained (R-squared equivalent) at lambda.1se
lasso_r2 <- cv_lasso$glmnet.fit$dev.ratio[which(cv_lasso$lambda == cv_lasso$lambda.1se)]
print(lasso_r2)



