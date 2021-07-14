# RYM DATA ANALYSIS

library(ggplot2)
theme_set(theme_minimal())


# READING DATA
# Read in a CSV file without headers
data <- read.csv(file="D:\\Downloads\\rymList (3).csv", header=FALSE)

# drop na
data <- na.omit(data)

# Manually assign the header names
names(data) <- c("Artist","Album","Year","Date", "Rating")


# DATA CONVERSION
# convert date column from type string to type date
data$Date <- as.Date(data$Date, format="%d/%m/%Y")

# function to convert "x.x0 stars" to int
convertRating <- function(ratingStr) {
    ratingStr <- substr(ratingStr,1,3)
    ratingNum <- as.double(ratingStr)

    return(ratingNum * 2)
}

# use the function on the rating column
data$Rating <- lapply(data$Rating, convertRating)


# RECREATING PLOTS I ALREADY MADE IN EXCEL
# create plotting matrix
par(mfrow = c(2, 1))

plot(data$Date, data$Year,
    col = "#4472C4",
    pch = 19,
    main = "Years of Rated Albums vs. Time",
    xlab = "Time",
    ylab = "Years of Rated Albums"
)

plot(data$Date, data$Rating,
    ylim = c(1,10),
    col = "#4472C4",
    pch = 19,
    main = "Rating of Rated Albums vs. Time",
    xlab = "Time",
    ylab = "Rating of Rated Albums"
)


# TIME SERIES PLOTS

# reverse list order and save in new var
df <- data[nrow(data):1,]

ggplot(data = df, aes(x = Date, y = nrow(df) - as.numeric(row.names(df))))+
  geom_line(color = "#00AFBB", size = 1)