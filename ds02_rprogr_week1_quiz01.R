
# notes for Course 2 on R Programming, Week 1, quiz 1

df <- read.csv(file = "hw1_data.csv")
df
df[1:2,]
str(df)
tail(df, 2)
df$Ozone[[47]]
df[47,]
nrow(df[is.na(df$Ozone),])
mean(df$Ozone, na.rm = TRUE)
mean(df[df$Ozone > 31 & df$Temp > 90, 2], na.rm = TRUE)
mean(df[df$Month == 6L, "Temp"])
max(df[df$Month == 5L, 1], na.rm = TRUE)
