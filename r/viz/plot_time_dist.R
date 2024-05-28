
### visualise the distribution of time data


times_df <- replicate(1000, addTimeBiModal(time1 = "07:30", time2 = "08:00", sd1 = 20, sd2 = 30, skew1 = -3, skew2 = -1))

ggplot(data.frame(time = times_df), aes(x = time)) + geom_histogram(bins = 30, fill = brewer.pal(5, "BuGn")[1], color = brewer.pal(5, "BuGn")[2]) + theme_minimal()


times_df <- replicate(1000, addTimeBiModal(time1 = "07:00", time2 = "08:00", sd1 = 25, sd2 = 30, skew1 = -3, skew2 = -1))
ggplot(data.frame(time = times_df), aes(x = time)) + geom_histogram(bins = 30, fill = brewer.pal(5, "PuRd")[1], color = brewer.pal(5, "PuRd")[2]) + theme_minimal()
