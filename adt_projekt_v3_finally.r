### ADT - projekt
library(MASS)
library(class)
library(car)
library(klaR)
library(e1071)
library(mvtnorm)

'1. Wczytujemy zebrane dane'
## Dane - Maciek
# Fizyka
m_fiz_dol_acc <- read.table('~/ADT/maciek/fiz_dol/Accelerometer.csv', header=TRUE,  sep=',')
m_fiz_dol_gyr <- read.table('~/ADT/maciek/fiz_dol/Gyroscope.csv', header=TRUE,  sep=',')
m_fiz_dol_lin <- read.table('~/ADT/maciek/fiz_dol/Linear Acceleration.csv', header=TRUE,  sep=',')

m_fiz_gor_acc <- read.table('~/ADT/maciek/fiz_go/Accelerometer.csv', header=TRUE,  sep=',')
m_fiz_gor_gyr <- read.table('~/ADT/maciek/fiz_go/Gyroscope.csv', header=TRUE,  sep=',')
m_fiz_gor_lin <- read.table('~/ADT/maciek/fiz_go/Linear Acceleration.csv', header=TRUE,  sep=',')

# GG
m_gg_dol_acc <- read.table('~/ADT/maciek/gg_d/Accelerometer.csv', header=TRUE,  sep=',')
m_gg_dol_gyr <- read.table('~/ADT/maciek/gg_d/Gyroscope.csv', header=TRUE,  sep=',')
m_gg_dol_lin <- read.table('~/ADT/maciek/gg_d/Linear Acceleration.csv', header=TRUE,  sep=',')

m_gg_gor_acc <- read.table('~/ADT/maciek/gg_g/Accelerometer.csv', header=TRUE,  sep=',')
m_gg_gor_gyr <- read.table('~/ADT/maciek/gg_g/Gyroscope.csv', header=TRUE,  sep=',')
m_gg_gor_lin <- read.table('~/ADT/maciek/gg_g/Linear Acceleration.csv', header=TRUE,  sep=',')

# MINI
m_mini_dol_acc <- read.table('~/ADT/maciek/mini_d/Accelerometer.csv', header=TRUE,  sep=',')
m_mini_dol_gyr <- read.table('~/ADT/maciek/mini_d/Gyroscope.csv', header=TRUE,  sep=',')
m_mini_dol_lin <- read.table('~/ADT/maciek/mini_d/Linear Acceleration.csv', header=TRUE,  sep=',')

m_mini_gor_acc <- read.table('~/ADT/maciek/mini_g/Accelerometer.csv', header=TRUE,  sep=',')
m_mini_gor_gyr <- read.table('~/ADT/maciek/mini_g/Gyroscope.csv', header=TRUE,  sep=',')
m_mini_gor_lin <- read.table('~/ADT/maciek/mini_g/Linear Acceleration.csv', header=TRUE,  sep=',')


## Dane - Denis
# Fizyka
d_fiz_dol_acc <- read.table('~/ADT/denis/fiz_dol/Accelerometer.csv', header=TRUE,  sep=',')
d_fiz_dol_gyr <- read.table('~/ADT/denis/fiz_dol/Gyroscope.csv', header=TRUE,  sep=',')
d_fiz_dol_lin <- read.table('~/ADT/denis/fiz_dol/Linear Accelerometer.csv', header=TRUE,  sep=',')

d_fiz_gor_acc <- read.table('~/ADT/denis/fiz_go/Accelerometer.csv', header=TRUE,  sep=',')
d_fiz_gor_gyr <- read.table('~/ADT/denis/fiz_go/Gyroscope.csv', header=TRUE,  sep=',')
d_fiz_gor_lin <- read.table('~/ADT/denis/fiz_go/Linear Accelerometer.csv', header=TRUE,  sep=',')

# GG
d_gg_gor_acc <- read.table('~/ADT/denis/gg_g/Accelerometer.csv', header=TRUE,  sep=',')
d_gg_gor_gyr <- read.table('~/ADT/denis/gg_g/Gyroscope.csv', header=TRUE,  sep=',')
d_gg_gor_lin <- read.table('~/ADT/denis/gg_g/Linear Accelerometer.csv', header=TRUE,  sep=',')

# MINI
d_mini_dol_acc <- read.table('~/ADT/denis/mini_d/Accelerometer.csv', header=TRUE,  sep=',')
d_mini_dol_gyr <- read.table('~/ADT/denis/mini_d/Gyroscope.csv', header=TRUE,  sep=',')
d_mini_dol_lin <- read.table('~/ADT/denis/mini_d/Linear Accelerometer.csv', header=TRUE,  sep=',')

d_mini_gor_acc <- read.table('~/ADT/denis/mini_g/Accelerometer.csv', header=TRUE,  sep=',')
d_mini_gor_gyr <- read.table('~/ADT/denis/mini_g/Gyroscope.csv', header=TRUE,  sep=',')
d_mini_gor_lin <- read.table('~/ADT/denis/mini_g/Linear Accelerometer.csv', header=TRUE,  sep=',')


'2. Wybieramy dane do klasyfikacji: PCA'
## MACIEK

#FIZ
FIZ_dol = data.frame(c(m_fiz_dol_gyr),
          c(m_fiz_dol_acc$Acceleration.y..m.s.2.)); colnames(FIZ_dol) <- c("time","gx", "gy","gz","y")
FIZ_gora = data.frame(c(m_fiz_gor_gyr),
                      c(m_fiz_gor_acc$Acceleration.y..m.s.2.)); colnames(FIZ_gora) <- c("time","gx", "gy","gz","y")

#MINI
MINI_dol = data.frame(c(m_mini_dol_gyr),
                     c(m_mini_dol_acc$Acceleration.y..m.s.2.)); colnames(MINI_dol) <- c("time","gx", "gy","gz","y")
MINI_gora = data.frame(c(m_mini_gor_gyr),
                      c(m_mini_gor_acc$Acceleration.y..m.s.2.)); colnames(MINI_gora) <- c("time","gx", "gy","gz","y")

#GG
GG_dol = data.frame(c(m_gg_dol_gyr),
                      c(m_gg_dol_acc$Acceleration.y..m.s.2.)); colnames(GG_dol) <- c("time","gx", "gy","gz","y")
GG_gora = data.frame(c(m_gg_gor_gyr),
                       c(m_gg_gor_acc$Acceleration.y..m.s.2.)); colnames(GG_gora) <- c("time","gx", "gy","gz","y")


FIZ <- rbind(FIZ_dol,FIZ_gora)
MINI <- rbind(MINI_dol,MINI_gora)
GG <- rbind(GG_dol,GG_gora)

# PCA
lifts_to_PCA <- rbind(FIZ[,2:5],MINI[,2:5],GG[,2:5])
test.pc <- princomp(~., cor=T, data=lifts_to_PCA)
plot(test.pc, main="PCA dla gx, gy, gz, y")


'3. Przygotowanie danych do klasyfikatorow - uczacy, testowy. Wizualizacja'
FIZ$class <- as.factor(1)
MINI$class <- as.factor(2)
GG$class <- as.factor(3)

lifts_to_knn <- rbind(FIZ[c('gx','gy','gz', 'class')], MINI[c('gx','gy','gz', 'class')], GG[c('gx','gy','gz', 'class')])


## DENIS - zbior testowy
#FIZ
FIZ_dol_d = data.frame(c(d_fiz_dol_gyr)); colnames(FIZ_dol_d) <- c("time","gx", "gy","gz")
FIZ_gora_d = data.frame(c(d_fiz_gor_gyr)); colnames(FIZ_gora_d) <- c("time","gx", "gy","gz")

#MINI
MINI_dol_d = data.frame(c(d_mini_dol_gyr)); colnames(MINI_dol_d) <- c("time","gx", "gy","gz")
MINI_gora_d = data.frame(c(d_mini_gor_gyr)); colnames(MINI_gora_d) <- c("time","gx", "gy","gz")

#GG
GG_gora_d = data.frame(c(d_gg_gor_gyr)); colnames(GG_gora_d) <- c("time","gx", "gy","gz")

FIZ_d <- rbind(FIZ_dol_d, FIZ_gora_d)
MINI_d <- rbind(MINI_dol_d, MINI_gora_d)
GG_d <- GG_dol

FIZ_d$class <- as.factor(1)
MINI_d$class <- as.factor(2)
GG_d$class <- as.factor(3)

lifts_to_knn_d <- rbind(FIZ_d[c('gx','gy','gz', 'class')], MINI_d[c('gx','gy','gz', 'class')], GG_d[c('gx','gy','gz', 'class')])

# Wizualizacja - zbior uczacy (Maciek)
scatter3d(x = lifts_to_knn$gx, y = lifts_to_knn$gy, z = lifts_to_knn$gz, groups = lifts_to_knn$class,
          surface=FALSE, grid = FALSE, ellipsoid = TRUE,
          surface.col = c("#999999", "#E69F00", "#56B4E9"))

# Wizualizacja - zbior testowy (Denis)
scatter3d(x = lifts_to_knn_d$gx, y = lifts_to_knn_d$gy, z = lifts_to_knn_d$gz, groups = lifts_to_knn$class,
          surface=FALSE, grid = FALSE, ellipsoid = TRUE,
          surface.col = c("#999999", "#E69F00", "#56B4E9"))


'4. Klasyfikacja'
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


'4.1. KNN'
# Powtorne podstawienie, k = 1...6
for (i in seq(6)) {
  print(i)
  class.knn <- knn(lifts_to_knn[1:3], lifts_to_knn[1:3], lifts_to_knn$class, i)
  tab <- table(class.knn, lifts_to_knn$class)
  print(accuracy(tab))
}

# Zbior testowy, k = 1...6
for (i in seq(6)) {
  print(i)
  class.knn <- knn(lifts_to_knn[1:3], lifts_to_knn_d[1:3], lifts_to_knn$class, i)
  tab <- table(class.knn, lifts_to_knn_d$class)
  print(accuracy(tab))
}


'4.2. QDA'
data.qda <- qda(class ~ ., lifts_to_knn)
data.qda.pred <- predict(data.qda, lifts_to_knn_d)
CM <- table(lifts_to_knn_d$class, data.qda.pred$class)
accuracy(CM)


'4.3. LDA'
data.lda <- lda(class ~ ., lifts_to_knn)
data.lda.pred <- predict(data.lda, lifts_to_knn_d)
CM <- table(lifts_to_knn_d$class, data.lda.pred$class)
accuracy(CM)


'4.4. Naive Bayes - BARDZO NISKIE ACC'
data.nb <- naiveBayes(class ~ ., lifts_to_knn)
data.nb.pred <- predict(data.nb, lifts_to_knn_d)
CM <- table(lifts_to_knn_d$class, data.nb.pred)
accuracy(CM)