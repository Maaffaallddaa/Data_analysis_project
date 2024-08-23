library(ggplot2)

my_data <- mtcars


ggplot(mtcars, aes(x=wt)) +
  geom_density()


ggplot(mtcars, aes(x=wt)) +
  geom_density(adjust = 0.1)

ggplot(mtcars, aes(x=wt)) +
  geom_density(adjust = 3)


ggplot(mtcars, aes(x=wt,fill=factor(cyl))) +
  geom_density(adjust = 3)


ggplot(mtcars, aes(x=wt,fill=factor(cyl))) +
  geom_density(adjust = 3,alpha=0.5)

ggplot(mtcars, aes(x=wt,fill=factor(cyl))) +
  geom_density(adjust = 3,alpha=0.5)+
  theme_classic()

ggplot(mtcars, aes(x=wt,fill=factor(cyl))) +
  geom_density(adjust = 3,alpha=0.5)+
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  theme_classic()


ggplot(mtcars, aes(x=wt,fill=factor(cyl))) +
  geom_density(adjust = 1,alpha=0.5)+
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
