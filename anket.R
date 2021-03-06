library(likert)
library(tidyverse)
setwd("~/GitHub/DogaBilimleriR")

anket <- read.csv("anket.csv")

anket1 <- anket[,4:6]
anket2 <- anket[,2:3]

for (i in 1:3) {
     anket1[,i] <- factor(anket1[,i], levels= c("1","2","3","4","5"))
     levels(anket1[,i]) <- c("Kesinlikle katılmıyorum", "Katılmıyorum", "Normal", 
                            "Katılıyorum","Kesinlikle katılıyorum")
}

for (i in 1:2) {
     anket2[,i] <- factor(anket2[,i], levels= c("1","2","3","4","5"))
     levels(anket2[,i]) <- c("Çok yavaş", "Yavaş", "Normal", 
                             "Hızlı","Çok hızlı")
}

names(anket1) <- c("Eğitmenler çalıştaya iyi hazırlanmıştı",
                   "Eğitmenler çalıştay boyunca yardımcı oldular",
                   "Bu çalıştayda öğrendiklerimi ileride kullanabileceğim")
names(anket2) <- c("Çalıştayın genel zorluk seviyesi nasıldı ?",
                   "Çalıştayın genel ilerleme hızı nasıldı ?")

anket_lik1 <- likert(anket1)
anket_lik2 <- likert(anket2)
tt <- "Doğa Bilimlerinde R Programlamaya Giriş Çalıştayı Değerlendirme Anketi"

plot(anket_lik1) + theme(
     axis.text.y = element_text(size = 13),
     strip.background = element_rect(colour = "white", fill = "white"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank())
plot(anket_lik2) + theme(
     axis.text.y = element_text(size = 13),
     strip.background = element_rect(colour = "white", fill = "white"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank())

plot(anket_lik1, type = "density") +
     theme(strip.background = element_rect(colour = "white", fill = "white"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank()) +
     ggtitle("Doğa Bilimlerinde R Programlamaya Giriş Çalıştayı Değerlendirme Anketi")
plot(anket_lik2, type = "density") + 
     theme(strip.background = element_rect(colour = "white", fill = "white"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank()) +
     ggtitle("Doğa Bilimlerinde R Programlamaya Giriş Çalıştayı Değerlendirme Anketi")



