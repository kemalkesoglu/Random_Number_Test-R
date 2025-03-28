#Fonksiyonu istenilen sekilde olusturalim.
lcg <- function(a, c, m, run.length, seed) {
  x <- rep(0, run.length)
  x[1] <- seed
  for (i in 1:(run.length - 1)) {
    x[i + 1] <- (a * x[i] + c + sample(1:150, 1)) %% m 
  }
  U <- x / m
  return(list(x = x, U = U))
}


#Fonksiyon ile 100 tane ve 10000 tane rastgele veri üretelim.
set.seed(Sys.time())  
z <- lcg(30, 90, 150, 100, sample(1:150, 1))  
z2 <- lcg(30, 90, 150, 10000, sample(1:150, 1))  


#NOT:Her çalıştırıldığında oluşan veriler değişir.
#başlangıç seed değeri olarak sistem zamanını kullanılmıştır.



#Olusan verilerin Histogram grafiklerini çizdirelim.
par(mfrow=c(2,2))
hist(z$x,col="blue")
hist(z$U,col="green")
hist(z2$x,col="red")
hist(z2$U,col="purple")
z
z2



#Ljung-Box test uygulayalim.
#Yüksek bir p degeri önemli bir otokorelasyon olmadigini gösterir.
#Düsük bir p degeri dizinin test edilen gecikmelerdeki degerler arasinda bagimliliklara sahip oldugunu gösterir.
Box.test (z$x, lag = 10, type = "Ljung")
Box.test (z$U, lag = 10, type = "Ljung")
#Uygulanan Ljung-Box Test sonucunda P=0.8464 oldugundan önemli bir otokorelasyon olmadigini söyleyebiliriz.

#Ljung-Box test uygulayalim.
#Yüksek bir p degeri önemli bir otokorelasyon olmadigini gösterir.
#Düsük bir p degeri dizinin test edilen gecikmelerdeki degerler arasinda bagimliliklara sahip oldugunu gösterir.
Box.test (z2$x, lag = 10, type = "Ljung")
Box.test (z2$U, lag = 10, type = "Ljung")
#Uygulanan Ljung-Box Test sonucunda P=0.6824 oldugundan önemli bir otokorelasyon olmadigini söyleyebiliriz.



#Ki-Kare test uygulayalim.
observed_z <- table(cut(z$U, breaks=8))  
expected_z <- rep(length(z$U) / 8, 8)  
chisq_test_z <- chisq.test(observed_z, p=expected_z / sum(expected_z), rescale.p=TRUE)
chisq_test_z
#P = 0.2673, anlamlilik degeri a = 0.05 den büyük oldugundan rastgele olusturdugumuz
#verilerin dagilimi ile belirtilen dagilim arasinda fark olmadigini söyleyebiliriz.

observed_z2 <- table(cut(z2$U, breaks=15))  
expected_z2 <- rep(length(z2$U) / 15, 15)  
chisq_test_z2 <- chisq.test(observed_z2, p=expected_z2 / sum(expected_z2), rescale.p=TRUE)
chisq_test_z2
#P = 0.8465, anlamlilik degeri a = 0.05 den büyük oldugundan rastgele olusturdugumuz
#verilerin dagilimi ile belirtilen dagilim arasinda fark olmadigini söyleyebiliriz.



#Serial Testi uygulayalim.
library(randtoolbox)
serial_test_z <- serial.test(z$U, d = 2, echo = TRUE)
serial_test_z
#Uygulanan serial test sonucunda P=0.19, a = 0.05 den büyük oldugu için H0 Reddedilemez.
#verilerin random oldugu söylenebilir.

serial_test_z2 <- serial.test(z2$U, d = 2, echo = TRUE)
serial_test_z2
#Uygulanan serial test sonucunda P=0.74, a = 0.05 den büyük oldugu için H0 Reddedilemez.
#verilerin random oldugu söylenebilir.



#Otokorelasyon Fonksiyonu(ACF) uygulayip gecikmeleri görsellestirelim.
acf(z$x, main="ACF for z$x")
acf(z$U, main="ACF for z$U")
acf(z2$x, main="ACF for z2$x")
acf(z2$U, main="ACF for z2$U")
#Ljung-Box test ile önemli bir otokorelasyon olmadigi söylemistik.
#Görsel olarak çizimi yapilmistir.



#Rastgelelik testi için Runs Test uygulayalim.
#Küçük bir p degeri dizinin rastgele olmayan desenlere sahip oldugunu gösterir.
#1'e yaklasitikça rastgelelik artar.
if(!require(randtests)) install.packages("randtests")

runs_test_z_x <- runs.test(z$x)
runs_test_z_x
#P degeri 0.5 oldugu için verilerin random oldugu söylenebilir.

runs_test_z_U <- runs.test(z$U)
runs_test_z_U
#P degeri 0.5 oldugu için verilerin random oldugu söylenebilir.

runs_test_z2_x <- runs.test(z2$x)
runs_test_z2_x
#P degeri 0.7 oldugu için verilerin random oldugu söylenebilir.

runs_test_z2_U <- runs.test(z2$U)
runs_test_z2_U
#P degeri 0.7 oldugu için verilerin random oldugu söylenebilir.



#Durbin-Watson testi ile otokorelasyon kontrolü yapalim.
#DW = 2 ise otokarelasyon yoktur.
#DW < 2 ise negatif bir otokarelasyon vardir.
#DW > 2 ise pozitif otokarelasyon vardir.
if(!require(lmtest)) install.packages("lmtest")

lm_model_z <- lm(z$x ~ seq_along(z$x))  
dw_test_z <- dwtest(lm_model_z)  
dw_test_z
#DW=2 oldugu için otokarelasyon yoktur diyebiliriz.

lm_model_z2 <- lm(z2$x ~ seq_along(z2$x))  
dw_test_z2 <- dwtest(lm_model_z2)  
dw_test_z2
#DW=2 oldugu için otokarelasyon yoktur diyebiliriz.



#Testlerimiz ve görsellestirmelerimiz sonucunda üretilen verilerin otokorelasyonlarinin olmadigini
#Random oldugunu söyleyebiliriz.
