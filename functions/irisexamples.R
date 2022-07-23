# Örnek: readxl paketinin kurulması
> if(!require(readxl)) {
+   install.packages("readxl", repo="https://cloud.r-project.org")}
   
# Örnek: readxl ile Excel dosyasını okuma
> library(readxl)}
> excel_sheets("iris.xlsx")
> iris <- read_excel("iris.xlsx", sheet="Sheet1")
> head(iris)

> fivenum(iris$Sepal.Length)
[1] 4.3 5.1 5.8 6.4 7.9

> summary(iris)

> if(!require(pastecs)){
+   install.packages("pastecs", repo="https://cloud.r-project.org");
+   library(pastecs) }

> stat.desc(iris[,1:4], basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)


> if(!require(psych)){
+   install.packages("psych", repo="https://cloud.r-project.org");
+   library(psych) }
> describeBy(iris[,1:4], group= iris[,5])


> data(iris)
> hist(iris$Sepal.Width, col="gray",
+   xlab="Sepal Width", ylab="Frekans",
+   main="Sepal Width Histogramı")

> data(iris)
> swdensity <- density(iris$Sepal.Width)
> plot(swdensity, col="blue", lwd=2, lty=1,
+   ylab="Yoğunluk", main="Olasılık Yoğunluk Grafiği")



# Örnek 2.1: Histogram ve Olasılık Yoğunluk Grafiği
> data(iris)
> hist(iris$Sepal.Width, prob=TRUE,  
+   col="gray", xlab="Sepal Width", ylab="Yoğunluk",
+   main="Sepal Width Histogramı ve Yoğunluk Grafiği")
> swdensity <- density(iris$Sepal.Width)
> lines(swdensity, col="blue", lwd=2, lty=1)



# Örnek 2.1: Kutu-bıyık grafiği
> data(iris)
> boxplot(iris$Sepal.Width, col="gray", horizontal=TRUE,
+   main="Sepal Width Kutu-bıyık Grafiği")

# Örnek 2.1: Karşılaştırma amaçlı kutu-bıyık grafikleri
> data(iris)
> boxplot(iris[,1:4], col=c(2,3,5,6), horizontal=TRUE, notch=TRUE,
+   main="iris veriseti Kutu-bıyık Grafiği")

# Örnek 2.1: Karşılaştırma amaçlı kutu-bıyık grafikleri (formülle)
> data(iris)
> boxplot(iris[,1]~iris[,5], col=c(2,3,5,6), 
+   xlab="Türler", ylab="Sepal Length", 
+  main="Sepal Length Kutu-bıyık Grafiği")



> boxplot(iris[,1]~iris[,5], col=c(2,3,5,6), 
+   xlab="Türler", ylab="Sepal Length", 
+  main="Sepal Length Kutu-bıyık Grafiği")
> stripchart(iris[,1] ~ iris[,5], vertical=TRUE, data = iris, 
+  method='jitter', add=TRUE, pch=19, col='blue', cex=0.8)


# Örnek 2.1: Çubuk grafik
> data(iris)
> irismeans <- apply(iris[,1:4], 2, mean)
> barplot(irismeans, col=c("red","green","blue","orange"),
+   xlab="Özellikler", ylab="Ortalama", 
+   main="iris verisi değişken ortalamaları")


# Örnek 2.1: Pasta grafik
> iristable <- table(iris$Species)
> iristable
> pie(iristable)

# Örnek 2.1: Serpilme grafiği
> plot(iris$Sepal.Length, iris$Petal.Length,
+   col="blue", pch=19, cex=2,
+   xlab = "Sepal.Length", ylab = "Petal.Length", 
+   main = "Serpilme Grafiği")

# Örnek 2.1: Serpilme grafiği
> plot(iris$Sepal.Length, iris$Petal.Length,
+   col=iris$Species, pch=19, cex=2,
+   xlab = "Sepal.Length", ylab = "Petal.Length", main = "Serpilme Grafiği")

> pairs(iris[,1:4], col=iris[,5], pch=19)

# Örnek: Korelasyon grafikleri 1
> if(!require(corrplot)){
+   install.packages("corrplot ", repo="https://cloud.r-project.org");
+   library(corrplot) }
> iriscormat <- cor(iris[1:4])
> corrplot(iriscormat, method="ellipse", type="upper", order="hclust",
+   tl.col="black", tl.srt=45)

# Örnek: Korelasyon grafikleri 2
> if(!require(corrplot)){
+   install.packages("corrplot ", repo="https://cloud.r-project.org");
+   library(corrplot) }
> iriscormat <- cor(iris[1:4])
> corrplot.mixed(iriscormat, lower.col="black", number.cex = 1.2)


> suppressPackageStartupMessages(installed <- require(ggstatsplot))
> if (!installed) {
+   install.packages("ggstatsplot", repos="https://cloud.r-project.org")  
+   suppressPackageStartupMessages(require(ggstatsplot))  
+ }
> suppressPackageStartupMessages(installed <- require(ggcorrplot))
> if (!installed) {
+   install.packages("ggcorrplot", repos="https://cloud.r-project.org")  
+   suppressPackageStartupMessages(require(ggcorrplot))  
+ }


> data(iris)
> ggcorrmat(
+  data     = iris,
+  colors   = c("red", "white", "dodgerblue"),
+  title    = "İris Korelogram",
+  subtitle = "standart"
+ )

> grouped_ggcorrmat(
+   data         = iris,
+   type         = "robust", ## correlation method
+   colors       = c("#cbac43", "white", "#550000"),
+   grouping.var = Species, ## grouping variable
+   matrix.type  = "lower" ## type of matrix
+ )

> # Hücreler
> par(mfrow=c(2,2))
> # Kenarlar
> par(mar=c(4.5, 4.2, 3, 1.5)) 
> hist(iris[,1], xlab = "Sepal Length", col = "gray", main="Histogram")
> hist(iris[,2], xlab = "Sepal Width", col = "gray", main="Histogram")
> plot(iris[,1:2], xlab="Length", ylab="Width", main= "Sepal", pch=19)
> boxplot(iris[,1:2], notch=TRUE, col=c("red","blue"))


