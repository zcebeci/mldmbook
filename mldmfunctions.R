# mldmfunctions.R
# 23 July 2022, Ver 1.0
#

# Kod 2.1: R paketi kurma fonksiyonu
paketkur <- function(paketler){
  yenipaket <- paketler[!(paketler %in% installed.packages()[,"Package"])]
  if (length(yenipaket)) 
    install.packages(yenipaket, dependencies=TRUE)
  sapply(yenipaket, require, character.only=TRUE)
}

# Kod 3.1: Çarpıklık katsayısı fonksiyonu
skewness <- function(x){
     skew <- sum((x-mean(x))^3) / ((length(x)-1)*sd(x)^3)
     return(skew)
  }

# Kod 3.2: Max-Min normalleştirme fonksiyonu
maxminnorm <- function(x) {
      return((x-min(x))/(max(x)-min(x)))
 }

# Kod 4.1: İki sınıflı sınıflama problemleri başarım ölçütleri 
binary_class_metrics <- function(yactual, ypred, ndigits=2) {
     confmat <- table(ypred, yactual, dnn=c("Tahmin","Gerçek"))
     N <- sum(confmat)
     TP <- confmat[1,1]
     FP <- confmat[1,2]
     TN <- confmat[2,2]
     FN <- confmat[2,1]
     TPR <- TP/(TP+FN) #Recall, sensitivity veya hit rate
     TNR <- TN/(TN+FP)
     FPR <- FP/(TN+FP)
     FNR <- FN/(TP+FN)
     FDR <- FP/(TP+FP)
     FOR <- FN/(TN+FN)
     PPV <- TP/(TP+FP) #Precision
     NPV <- TN/(TN+FN)
     TTS <- TPR-FPR
     BS <- (TP+FP)/(TP+FN)
     CSI <- TP/(TP+FP+FN)
     OR <- (TP*TN) / (FP*FN)
     MCR <- (FP+FN)/N 
     ACC <- (TP+TN)/N #Accuracy
     F1 <- 2 * PPV * TPR / (PPV + TPR)
     metrics <- list(
         confmat=confmat, ACC=ACC, TPR=TPR, TNR=TNR, FPR=FPR, 
         FNR=FNR, FDR=FDR, FOR=FOR, PPV=PPV, NPV=NPV, TTS=TTS,
         BS=BS, CSI=CSI, OR=OR, MCR=MCR,F1=F1)
     metrics[] <- lapply(metrics, round, ndigits)
     return(metrics)
  }


# Kod 4.2: Çok sınıflı problemler için başarım ölçütleri 
multi_class_metrics <- function(yactual, ypred, ndigits=2) {
     confmat <- table(ypred, yactual, dnn=c("Tahmin","Gerçek"))
     N <- sum(confmat)
     CS <- colSums(confmat)
     RS <- rowSums(confmat)
     TP <- diag(confmat)
     TN <- N - (RS + CS - TP)
     FP <- RS - TP
     FN <- CS - TP
     TPR <- TP/(TP+FN) #Recall
     PPV <- TP/(TP+FP) #Precision
     TNR <- TN/(TN+FP)
     FPR <- FP/(TN+FP)
     FNR <- FN/(TP+FN)
     FDR <- FP/(TP+FP)
     FOR <- FN/(TN+FN)
     NPV <- TN/(TN+FN)
     TTS <- TPR-FPR
     BS <- (TP+FP)/(TP+FN)
     CSI <- TP/(TP+FP+FN)
     OR <- (TP*TN) / (FP*FN)
     F1 <- 2 * PPV * TPR / (PPV + TPR)
     ACC <- (TP+TN)/N
     MCR <- (FP+FN)/N 
     GACC <- sum(TP) / N
     MacF1 <- mean(F1)
     MacPrec <- mean(PPV)
     MicPrec <- sum(TP)/(sum(TP)+sum(FP))
     MicRecall <- sum(TP)/(sum(TP)+sum(FN))
     MacRecall <- mean(TPR)
     MicF1 <- 2*(MicPrec*MicRecall)/( MicPrec+MicRecall)
     WF1 <- sum(CS*F1)/N
     metrics <- list(
         confmat=confmat, 
         criteria=data.frame(
            TP=TP, TN=TN, FP=FP, FN=FN, SUP=CS,
            TPR=TPR, TNR=TNR, FPR=FPR, FNR=FNR,
            FDR=FDR, FOR=FOR, PPV=PPV, NPV=NPV,
            TTS=TTS, BS=BS, CSI=CSI, OR=OR, 
            ACC=ACC, MCR=MCR, F1=F1),
         performances = data.frame(
            accuracy = GACC,
            macro.F1 = MacF1,
            micro.F1 = MicF1,
            weighted.F1 = WF1,
            macro.precision = MacPrec,
            micro.precision = MicPrec,
            weighted.precision = MicPrec,
            macro.recall = MacRecall,
            micro.recall = MicRecall,
            weighted.recall = MicPrec)
       )
     metrics[] <- lapply(metrics, round, ndigits)
     return(metrics)
  }



# Kod 4.3: Veri benzetimi ve yanılgı matrisi 
sim_confmat <- function(clabels, n=100, mcr=0.25, probs, seed=0){
     if(seed>0) set.seed(seed)
     nf <- round(n*mcr) # Yanlış sınıflanacak gözlem sayısı
     nc <- length(clabels) # Sınıf sayısı
     if(missing(probs)) probs <- rep(1/nc, nc)
     if(mcr<0 | mcr>1) stop("MCR [0,1] aralığında olmalı")
  # Gerçek veri üret
         yactual <- clabels[sample(1:nc, n, prob=probs, replace=TRUE)] 
      # Tahmin verisi üret
           ypred <- yactual 
        # Gerçek sınıf etiketlerini değiştirerek tahmin hatası oluştur
             idx <- sample(1:n, nf, replace=FALSE)
             for(i in idx)
                 ypred[i] <- sample(clabels[-which(clabels==yactual[i])],1)
            # Benzetilen verliler çerçevesi oluştur
                 df <- cbind(yactual, ypred)
              # Yanılgı matrisi oluştur
                   cm <- as.matrix(table(Tahmin = ypred, Gerçek = yactual))
                   return(list(df=df, confmat=cm))
                }

# Kod 4.4: Yanılgı matrisini görselleştirme
plot_confmat <- function(confmat, color){
     suppressPackageStartupMessages(installed <- require(ggplot2))
     if (!installed) {
          install.packages("ggplot2", repos="https://cloud.r-project.org")  
          suppressPackageStartupMessages(require(ggplot2)) }
     if(!is.data.frame(confmat)) confmat <- as.data.frame(confmat)
     if(missing(color)) color <- c("White", "red")
     names(confmat) = c("Predicted","Actual","Freq")
     ggplot(data = confmat, mapping = aes(x = Predicted, y = Actual)) +
       geom_tile(aes(fill = Freq)) +
       labs(x = "Gerçek",y = "Tahmin") +
       geom_text(aes(label=Freq), vjust = .5, fontface="bold", alpha=1) +
       scale_fill_gradient(low = color[1], high = color[2])
  }



# Kod 4.5: Regresyon problemleri başarım ölçütleri 
regression_metrics <- function(yactual, ypred, ndigits=2) {
  errors <- yactual - ypred
  ymean <- mean(yactual)
  N <- length(yactual)
  MINE <- min(errors)
  MAXE <- max(errors)
  MAE <- mean(abs(errors))
  MEDAE <- median(abs(errors))
  MAPE <- mean(abs(errors)/(abs(yactual)))*100
  RAE <- sum(abs(errors)) / sum(abs(yactual-ymean))
  MSE <- mean(errors^2)
  RMSE <- sqrt(MSE)
  RMSLE <- log(RMSE, base=exp(1))
  sst <- (yactual-ymean)^2
  ssr <- errors[is.finite(sst)]^2
  R2 <- 1-sum(ssr) / sum(sst[is.finite(sst)])
  COR <- cor(yactual, ypred)
  metrics <- list(
    errors=errors, MINE=MINE, MAXE=MAXE, MAE=MAE, MEDAE=MEDAE, 
    MAPE=MAPE, MSE=MSE, RMSE=RMSE, RMSLE=RMSLE, 
    R2=R2, COR=COR, RAE=RAE)
  metrics[] <- lapply(metrics, round, ndigits)
  return(metrics)
}


# Kod 5.1: Başarım ölçütleri hesaplama fonksiyonu
performance <- function(original, predicted, k){
    err <- original-predicted
    MAE <- mean(abs(err))
    MAPE <- mean(abs(err/original))*100
    MSE <- mean((err)^2)
    RMSE <- sqrt(MSE)
    R2 <- 1-(sum((err)^2)/sum((original-mean(original))^2))
    PCOR <- cor(original, predicted)
    names(PCOR) <- "PCOR"
    n <- length(original)
    AIC <- n * log(mean(err^2)) + 2 * k
    BIC <- n * log(MSE) + log(n) * k
    perf <- data.frame(MAE=MAE, MAPE=MAPE, MSE=MSE, RMSE=RMSE, 
                            R2=R2, PCOR=PCOR, AIC=AIC, BIC=BIC)
    return(perf)
 }


# Kod 6.3: Görüntü işleme
imgprocess <- function(x) {
   arrays <- lapply(x, function(path) {
        img <- image_load(img_path, target_size = c(256, 256), grayscale=F) 
        x <- image_to_array(img)
        x <- array_reshape(x, c(1, dim(x)))
        x <- x / 255
       })
   do.call(abind::abind, c(arrays, list(along = 1)))
  }

