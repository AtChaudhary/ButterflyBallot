#Load dataset
florida <- read.csv("C:/Users/ashvi/Downloads/florida.csv")
head(florida)
#Plotting Perot by Bucanan, hypothesis would vote for Buchanan when trying to vote for Perot
library(tidyverse)
ggplot(florida, mapping=aes(x=Perot96, y=Buchanan00,
  label=county)) +
  geom_point() +
  geom_text(vjust=1.5, size=3)
#Predict number of votes in a county for Buchanan in 2000 based on votes for Perot 1996
lm(formula = Buchanan00 ~ Perot96, data = florida)
#Coefficient=0.036, additional vote for Perot in 1996 is predicted for Buchanan  to have 0.036 more votes
#add regression to ggplot
ggplot(florida, mapping=aes(x=Perot96, y=Buchanan00)) +
  geom_point() +
  xlab("Votes for Perot in 1996") +
  ylab("Votes for Buchanan in 2000") +
  geom_text(aes(label = county), hjust = 0, vjust = 0) +
  geom_smooth(method = "lm", se = FALSE, color="red",linetype="dashed")
#Predicted Buchanan = about 1108, let order county by residual to see which county has highest residual
#Estimate regression
lm.florida <- lm(Buchanan00 ~ Perot96, data=florida)
lm.florida
#Save names to find output
lm.florida <- lm(Buchanan00 ~ Perot96, data=florida)
names(lm.florida)
#Extract predicted values
florida$predBuchanan00 <- lm.florida$fitted.values
#Extract residuals
florida$residuals <- lm.florida$residuals
#Order of largest to smallest residual
arrange(florida, desc(abs(residuals)))
#Table of residuals
florida %>%
  mutate(predBuchanan00 = lm.florida$fitted.values,
         residuals=lm.florida$residuals) %>%
  select(county, predBuchanan00, Buchanan00,residuals) %>%
  arrange(desc(abs(residuals)))
#label data points
#The residual is largest in magnitude in Palm Beach County(2,300 "extra votes for Buchanan, next largest residual is 612)
#Extract residual of Palm Beach County
residuals <- resid(lm.florida)
palm_beach_residual <- residuals[florida$county == "PalmBeach"]
#Calculate the standard error of the residuals
se_residuals <- summary(lm.florida)$sigma
#Calculate the standardized residual for Palm Beach County
standardized_residual <- palm_beach_residual / se_residuals
#Define the critical value for a 5% significance level (two-tailed)
critical_value <- 1.96
#Check if the standardized residual is significant
significant <- abs(standardized_residual) > critical_value

if (significant) {
  print("The residual for Palm Beach County is significant at the 5% level.")
} else {
  print("The residual for Palm Beach County is not significant at the 5% level.")
}
#Residual is significant, the butterfly ballot could of played a role into the voting outcome
