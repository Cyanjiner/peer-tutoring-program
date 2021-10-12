rats_data<-data.frame(initial=c(217, 246, 256, 200, 198, 248, 180, 218,
                                264, 200, 210, 192, 181, 266, 274, 180),
                      final=c(196, 218, 216, 165, 202, 231, 187, 230, 231, 170,
                              189, 185, 193, 285, 266, 188),
                      group=rep(c("1","2","3","4"), each=4))

# has randomization worked?
# check initial weights between groups

tapply(rats_data$initial, rats_data$group, mean)

library(ggplot2)
ggplot(rats_data)+
  geom_point(aes(x=initial,y=final))

model1<-aov(final~group,data = rats_data)
summary(model1)

model4b<-aov(final~group+initial, data=rats_data)
summary(model4b)

layout(matrix(c(1,2,3,4),2,2))
plot(model1)
