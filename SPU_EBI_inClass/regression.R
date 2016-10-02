summary(lm(rating~.,data=select(filter(data3,response=="Electronics"),-response)))
