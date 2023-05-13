####READ ME####
#The purpose of this script is to make fake polts for presentations

####libraries####
library(ggplot2)

####create fake data####
#expected distribution of error vs distance
x <- rnorm(20)
y <- x + rnorm(20)

# Combine data into a data frame
df <- data.frame(x = x, y = y)

#create plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 1, color = "black") +  # Add points
  geom_smooth(method = "lm", se = FALSE, col = "#1A5354") +  # Add trendline
  theme_minimal() +# Use a white background 
  ggtitle("Expected distribution") +
  labs(x = "Error", y = "Distance") + # Add axis labels
  theme(plot.title = element_text(hjust = 0.5)) # Center title

#error calculation, create plot with flipped y-axis for gage vs days dry plot
#create fake data
set.seed(123) # for reproducibility
x <- seq(-5, 5, length.out = 20)
y <- x + rnorm(20, sd = 3) #sd for error, the higher sd bigger error
df <- data.frame(x = x, y = y)
#plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 1, color = "black") +  # Add points
  geom_smooth(method = "lm", se = FALSE, col = "#1A5354") +  # Add trendline
  scale_y_reverse() +  # Flip y-axis
  theme_bw() +  # Use a white background
  labs(x = "Gage 1", y = "# of days dry") + # Add axis labels
  theme(plot.title = element_text(hjust = 0.5)) # Center title

#create fake data
set.seed(123) # for reproducibility
x <- seq(-5, 5, length.out = 20)
y <- x + rnorm(20, sd = 1) #sd for error, the higher sd bigger error
df <- data.frame(x = x, y = y)
#plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 1, color = "black") +  # Add points
  geom_smooth(method = "lm", se = FALSE, col = "#1A5354") +  # Add trendline
  scale_y_reverse() +  # Flip y-axis
  theme_bw() +  # Use a white background
  labs(x = "Gage 2", y = "# of days dry") + # Add axis labels
  theme(plot.title = element_text(hjust = 0.5)) # Center title

#create fake data
set.seed(123) # for reproducibility
x <- seq(-5, 5, length.out = 20)
y <- x + rnorm(20, sd = 0.2) #sd for error, the higher sd bigger error
df <- data.frame(x = x, y = y)
#plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 1, color = "black") +  # Add points
  geom_smooth(method = "lm", se = FALSE, col = "#1A5354") +  # Add trendline
  scale_y_reverse() +  # Flip y-axis
  theme_bw() +  # Use a white background
  labs(x = "Gage 3", y = "# of days dry") + # Add axis labels
  theme(plot.title = element_text(hjust = 0.5)) # Center title
