# install.packages('plyr')
library(plyr)

# install.packages('ggplot2')
# library(ggplot2) 
# install.packages("ggthemes")
# library(ggthemes)


HR = read.csv('data/case.csv')
# head(HR,n = 3)
# print(HR)
# str(HR)
# print(summary(HR))
# print(summary(HR$longitude))
# case$longitude= as.factor(case$confirmed)
# print(summary(case$confirmed))


# print(summary(HR$confirmed))
# HR$confirmed = ifelse(HR$confirmed > 50, 'High', ifelse(HR$confirmed > 20, 'Mid', 'Low'))
# print(HR)
# HR$confirmed = as.factor(HR$confirmed)
# print(summary(HR$confirmed))


HR_High = subset(HR,confirmed == '1' & province == 'Jeollabuk-do')
print(HR_High)
print(summary(HR_High$confirmed))


SS=ddply(HR, 
         c("confirmed","province"),summarise,
         M_SF = mean(confirmed), 
         COUNT =length(confirmed), 
         M_WH = round(mean(confirmed),2))

print(SS)
