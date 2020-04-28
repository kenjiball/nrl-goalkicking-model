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
          
          Foot = str_to_title(Foot),
          # Mirror left foot kickers to right field
          x_norm = if_else(Foot == "Left", -x_coord, x_coord),
          # Calculate Angle: α = arctan(a / b)
          Angle =  atan(x_norm/y_coord) * 180 / pi
  )

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

# Build Model
model_gk <- glm(formula = Result ~ Angle + y_coord,
                data =  model_data,
                family = binomial )

summary(model_gk)





# Generate more data
model_data_g <- model_data



