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
          Cond = as.factor(Cond),
          Foot = str_to_title(Foot),
          # Mirror left foot kickers to right field
          x_norm = if_else(Foot == "Left", -x_coord, x_coord),
          # Calculate Angle: Find using goal width, Hypotenuse and law of cosines 
          goal_width = 5.5,
          x_1 = x_norm - goal_width/2,
          x_2 = x_norm + goal_width/2,
          h_1 = sqrt(x_1^2 + y_coord^2),
          h_2 = sqrt(x_2^2 + y_coord^2),
          
          Angle = acos( (h_1^2 + h_2^2 - goal_width^2)/(2*h_1*h_2) )
  )


# Build Model
model_gk <- glm(formula = Result ~ Angle + Cond ,
                data =  model_data,
                family = binomial )

summary(model_gk)




# Plot 2
model_data %>%
  filter(Type == "C") %>% 
  ggplot( aes(x = x_norm, y = y_coord, col = Result)) +
  background_image(img) +
  geom_point() +
  scale_y_reverse(limits = c(50,2)) +
  labs(
    title = "NRL Goalkicking 2020",
    subtitle = "Rounds 1-2",
    caption = "Data collected by @NRLFanalytics Twitter"
  )

# Generate more data
model_data_g <- model_data



