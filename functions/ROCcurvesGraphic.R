set.seed(12)
a <- rnorm(100000, 50, 5)
b <- rnorm(100000, 60, 5)
x <- c(a,b)
g <- as.factor(c(rep(0,100000), rep(1,100000)))
xdf1 <- as.data.frame(cbind(x=x, Sınıf=g))
xdf1$Sınıf <- factor(xdf1$Sınıf)

p1 <- ggplot(xdf1, aes(x=x, fill=Sınıf)) +
  geom_density(alpha=0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
p1

set.seed(12)
a <- rnorm(100000, 50, 5)
b <- a
x <- c(a,b)
g <- as.factor(c(rep(0,100000), rep(1,100000)))
xdf2 <- as.data.frame(cbind(x=x,Sınıf=g))
xdf2$Sınıf <- factor(xdf2$Sınıf)

p2 <- ggplot(xdf2, aes(x=x, fill=Sınıf)) +
  geom_density(alpha=0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
p2

set.seed(12)
a <- rnorm(100000, 40, 5)
b <- rnorm(100000, 75, 5)
x <- c(a,b)
g <- as.factor(c(rep(0,100000), rep(1,100000)))
xdf3 <- as.data.frame(cbind(x=x,Sınıf=g))
xdf3$Sınıf <- factor(xdf3$Sınıf)

p3 <- ggplot(xdf3, aes(x=x, fill=Sınıf)) +
  geom_density(alpha=0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p3