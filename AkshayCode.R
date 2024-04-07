spotify_2023$streams <- as.numeric(spotify_2023$streams)

spotify_2023$key <- as.factor(spotify_2023$key)

spotify_2023 <- na.omit(spotify_2023)

X9 <- spotify_2023$in_spotify_playlists
X10 <- spotify_2023$in_apple_playlists
X11 <- spotify_2023$in_deezer_playlists
Y <- spotify_2023$streams

model <- lm(Y ~ X9 + X10 + X11, data = spotify_2023)
summary(model)

#Plots
plot(X9, Y, main = "Streams vs. Spotify Playlists", xlab = "Spotify Playlists", ylab = "Streams", pch = 19, col = "blue")
plot(X11, Y, main = "Streams vs. Deezer Playlists", xlab = "Deezer Playlists", ylab = "Streams", pch = 19, col = "green")

# Calculate residuals
residuals <- resid(model)

# Calculate fitted values
fitted.values <- fitted(model)

# Plot residuals
plot(fitted.values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 19)
abline(h = 0, col = "red")