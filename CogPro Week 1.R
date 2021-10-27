


library(ggplot2)
library(tidyverse)

x1 <- seq(-4,4,length = 100)
y1 <- dnorm(x1,mean = 0 , sd = 1)
x2 <-  seq(-3,5,length = 100)
y2 <-  dnorm(x2, mean  = 1, sd = 1 )

df <- tibble(x1 = x1, y1 = y1, x2 = x2, y2 = y2)

df %>% ggplot() + 
  geom_line(aes(x=x1, y=y1), color="palegreen3")  + 
  geom_line(aes(x=x2, y=y2), color="lightslateblue")+
  theme (plot.title = element_text(hjust = 0.5), text = element_text(family = "serif", size = 12), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))+
         labs(x = "Intensity", y = " ")+
         ggtitle("Noise and Signal Distributions")+
         geom_vline(xintercept = 1)

# Liberal
#false alarm rate
pnorm(0, mean = 0 , sd = 1, lower.tail = FALSE) 
#hit rate 
pnorm(0, mean = 1, sd = 1, lower.tail = FALSE )       
#Neutral
#false alarm rate
pnorm(0.5, mean = 0 , sd = 1, lower.tail = FALSE) 
#hit rate 
pnorm(0.5, mean = 1, sd = 1, lower.tail = FALSE )     

# Conservative

#false alarm rate
pnorm(1, mean = 0 , sd = 1, lower.tail = FALSE) 
#hit rate 
pnorm(1, mean = 1, sd = 1, lower.tail = FALSE )     


# Calculate D prime
qnorm(0.84, mean=0, sd=1)-qnorm(0.5, mean = 0, sd =1 )


#Plot ROC curve

# Receiver Operating Characteristic( ROC)
dat1 <- tibble(x1 = c(0.84,0.69,0.5),y1 = c(0.5,0.31,0.16))

# Plot ROC curve
ggplot(data=dat1, aes(x=y1, y= x1)) +     
geom_point() +
expand_limits(x = c(0, 1), y = c(0,1))+
labs(x = " False Alarm Rate", y = " Hit Rate")+
theme (plot.title = element_text(hjust = 0.5), text = element_text(family = "serif", size = 12), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))+
          geom_curve(data = dat1,
             x = 0,
             xend = 1,
             y = 0,
             yend = 1,
             curvature = -0.39,
             color = "red")+
           ggtitle("Receiver Operating Characteristic( ROC)")
        


  #Plot hit rate and false alarm rate when Diagnosticity ratio is same

dat2 <- tibble(x = c(0.1,0.2,0.4),y= c(0.2,0.4,0.8))

dat2 %>% ggplot()+
geom_point( aes(x= x, y= y))+
expand_limits(x = c(0, 1), y = c(0,1))+
geom_line(aes(x = x , y = y))+
labs(x = " False Alarm Rate", y = " Hit Rate")+
theme (plot.title = element_text(hjust = 0.5), text = element_text(family = "serif", size = 12), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))+
ggtitle("ROC when DR remains constant")















