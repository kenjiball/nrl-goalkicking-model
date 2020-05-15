# build_gk_model.r

# Load required libraries
library(tidyverse)
library(ggplot2)
library(png)


# set working directory
getwd()

# load data
gk_data <- read_csv("./data/Goal_Kicking_NRL 2020.csv")

# load the image
img <- png::readPNG("data/field2.png")


### Plot the data
gk_data %>%
  filter(Type == "C") %>% 
  mutate( x_coord = if_else(Side == "L", -Width, Width),
          y_coord = Dist,
          Result = str_to_title(Result),
          Foot = str_to_title(Foot) ) %>% 
  ggplot( aes(x = x_coord, y = y_coord, col = Result)) +
  background_image(img) +
  geom_point() +
  scale_y_reverse(limits = c(50,2)) +
  facet_grid(.~Foot) + 
  labs(
    title = "NRL Goalkicking 2020",
    subtitle = "Rounds 1-2",
    caption = "Data collected by @NRLFanalytics Twitter"
  )


### Prepare data frame

model_data <- gk_data %>%
  filter(Type == "C") %>% 
  mutate( x_coord = if_else(Side == "L", -Width, Width),
          y_coord = Dist,
          Result = as.factor(str_to_title(Result)),
          Result_num = if_else(Result == "Success", 1, 0, missing = 0),
          Cond = as.factor(Cond),
          Foot = str_to_title(Foot),
          # Mirror left foot kickers to right field
          x_norm = if_else(Foot == "Left", -x_coord, x_coord),
          # Calculate Alpha:
          # Find using goal width, Hypotenuse and law of cosines
          goal_width = 5.5,
          x_1 = x_norm - goal_width/2,
          x_2 = x_norm + goal_width/2,
          h_1 = sqrt(x_1^2 + y_coord^2),
          h_2 = sqrt(x_2^2 + y_coord^2),
          Angle = acos( (h_1^2 + h_2^2 - goal_width^2)/(2*h_1*h_2) ),
          # Change Angle from radians to degrees
          Alpha = Angle * 180/pi,
          
          # Distance to goal
          Distance = sqrt(x_norm^2 + y_coord^2)
  )


# Build Model
model_gk <- glm(formula = Result ~  Alpha + Alpha:Distance,
                family = binomial,
                data =  model_data)

summary(model_gk)

# Run Predict and Plot the logistic regression outcomes (One Feature Only)
newdat <- data.frame(Field_side=seq(min(model_data$Field_side), max(model_data$Field_side),len=100))
newdat$Result_num = predict(model_gk, newdata=newdat, type="response")
plot(Result_num ~ Field_side, data=model_data, col="red4")
lines(Result_num ~ Field_side, newdat, col="green4", lwd=2)

# Run predict to build expected goals
ratings_data <- model_data %>% 
                  mutate(conversion_probability = predict(model_gk, newdata=model_data, type="response"), 
                         expected_points = 2 * conversion_probability, 
                         actual_points = if_else(Result_num == 1, 2, 0), 
                         predicted_score_value = actual_points - expected_points)

# Summarise by Player
ratings_data %>% 
  group_by(Player) %>%
  summarise(attempts = n(), 
            success_rate = sum(Result_num)/attempts,
            mean_psv = mean(predicted_score_value),
            total_psv = sum(predicted_score_value)) %>%
  arrange(desc(total_psv))



# Plot 2 - Player Attempts
model_data %>%
  filter(Type == "C" & Player == "Cameron Smith") %>% 
  ggplot( aes(x = x_coord, y = y_coord, col = Result)) +
  background_image(img) +
  geom_point() +
  scale_y_reverse(limits = c(50,2)) +
  labs(
    title = "NRL Goalkicking 2020",
    subtitle = "Rounds 1-2",
    caption = "Data collected by @NRLFanalytics Twitter"
  )



