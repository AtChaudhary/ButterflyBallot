#Load dataset
florida <- read.csv("C:\Users\ashvi\Downloads\florida.csv")
head(florida)
#Plotting Perot by Bucanan, hypothesis would vote for Buchanan when trying to vote for Perot
library(tidyverse)
ggplot(florida, mapping=aes(x=Perot96, y=Buchanan00,
  label=county)) +
  geom_point() +
  geom_text(vjust=1.5, size=3)
#Predict number of votes in a county for Buchanan in 2000 based on votes for Perot 1996
lm(formula = Buchanan00 ~ Perot96, data = florida)
#Coefficient=0.036, additional vote for Perot in 1996 is predicted to increase votes for Buchanan by 0.036
#add regression to ggplot
ggplot(florida, mapping=aes(x=Perot96, y=Buchanan00)) +
  geom_point() +
  xlab("Votes for Perot in 1996") +
  ylab("Votes for Buchanan in 2000") +
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
#
florida %>%
  mutate(predBuchanan00 = lm.florida$fitted.values,
         residuals=lm.florida$residuals) %>%
  select(county, predBuchanan00, Buchanan00,residuals) %>%
  arrange(desc(abs(residuals)))
#The residual is largets in maginitude in Palm Beach County(2,300 "extra votes for Buchanan, next largest residual is 612)
#Conduct hypothesis testing

#Bush won Florida by 537 votes, possibly butterfly ballot could of affected outcome

