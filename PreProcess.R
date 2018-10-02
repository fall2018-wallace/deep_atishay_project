
df <- satisfaction_survey


df$Satisfaction[df$Satisfaction == "4.00.5" | df$Satisfaction == "4.00.2.00"] = 4
df$Satisfaction <- as.double(df$Satisfaction)

plot <- hist(df$Satisfaction)

plot
