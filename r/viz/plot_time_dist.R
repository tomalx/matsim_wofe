
### visualise the distribution of time data

#source("r/help_fun/addTimes.R")
library(RColorBrewer)


### palettes
brewer_seq_pal <- RColorBrewer::brewer.pal.info %>% filter(category == "seq") %>% row.names()


### using the functions from addTimes.R to create a distribution of times
times_df <- replicate(1000, addTimeBiModal(time1 = "07:30", time2 = "08:00", sd1 = 20, sd2 = 30, skew1 = -3, skew2 = -1))

ggplot(data.frame(time = times_df), aes(x = time)) + geom_histogram(bins = 30, fill = brewer.pal(5, sample(brewer_seq_pal,1))[1], color = brewer.pal(5, "BuGn")[2]) + theme_minimal()


times_df <- replicate(1000, addTimeBiModal(time1 = "07:00", time2 = "08:00", sd1 = 25, sd2 = 30, skew1 = -3, skew2 = -1))
ggplot(data.frame(time = times_df), aes(x = time)) + geom_histogram(bins = 30, fill = brewer.pal(5, "PuRd")[1], color = brewer.pal(5, "PuRd")[2]) + theme_minimal()


ggplot(data.frame(time = times_df), aes(x = hms(seconds(time)))) + 
  geom_histogram(bins = 30, fill = brewer.pal(5, "PuRd")[1], color = brewer.pal(5, "PuRd")[2]) + 
  theme_minimal()

### function that takes a distribution of times (in secs) and returns a plot of the distribution
### with colour palette as an argument and random color brewer palette
times_distribution_plot <- function(times_df, palette = NULL) {
  brewer_seq_pal <- RColorBrewer::brewer.pal.info %>% filter(category == "seq") %>% row.names()
  pal_select <- sample(brewer_seq_pal, 1)
  palette <- ifelse(is.null(palette), pal_select, palette)
  ggplot(data.frame(time = times_df), aes(x = hms(seconds(time)))) + 
    geom_histogram(bins = 30, fill = brewer.pal(5, palette)[1], color = brewer.pal(5, palette)[2]) + 
    theme_minimal()
}


### function that takes a distribution of times (in secs) and returns a plot of the distribution
### with colour palette as an argument and random color brewer palette
### with axis title formatted and times binned at common sense intervals
times_distribution_plot <- function(times_df, palette = NULL, bin_width_mins = 10, title = "Distribution of times") {
  brewer_seq_pal <- RColorBrewer::brewer.pal.info %>% filter(category == "seq") %>% row.names()
  pal_select <- sample(brewer_seq_pal, 1)
  palette <- ifelse(is.null(palette), pal_select, palette)
  ggplot(data.frame(time = times_df), aes(x = hms(seconds(time)))) + 
    geom_histogram(binwidth = 60 * bin_width_mins, boundary = 0, fill = brewer.pal(5, palette)[1], color = brewer.pal(5, palette)[2]) +
    xlab("Time of day") +
    ggtitle(title) +
    theme_minimal()
}


times_distribution_plot(times_df, "PuRd")
times_distribution_plot(times_df, bin_width_mins = 10)
times_distribution_plot(times_df, bin_width_mins = 15, title = "distribution of activity start times")

