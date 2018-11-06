library(tidyverse)

anket <- read.csv("anket.csv")

anket1 <- anket[,c(1,4:6)]
anket2 <- anket[,c(1,2:3)]

for (i in 2:4) {
     anket1[,i] <- factor(anket1[,i], levels= c("1","2","3","4","5"))
     levels(anket1[,i]) <- c("Kesinlikle katılmıyorum", "Katılmıyorum", "Normal", 
                             "Katılıyorum","Kesinlikle katılıyorum")
}

for (i in 2:3) {
     anket2[,i] <- factor(anket2[,i], levels= c("1","2","3","4","5"))
     levels(anket2[,i]) <- c("Çok yavaş", "Yavaş", "Normal", 
                             "Hızlı","Çok hızlı")
}

names(anket1) <- c("ID",
                   "Eğitmenler çalıştaya iyi hazırlanmıştı",
                   "Eğitmenler çalıştay boyunca yardımcı oldular",
                   "Bu çalıştayda öğrendiklerimi ileride kullanabileceğim")
names(anket2) <- c("ID",
                   "Çalıştayın genel zorluk seviyesi nasıldı?",
                   "Çalıştayın genel ilerleme hızı nasıldı?")



anket_lf1 <- reshape2::melt(anket1, id= "ID", factorsAsStrings= FALSE)
anket_lf2 <- reshape2::melt(anket2, id= "ID", factorsAsStrings= FALSE)
anket_lf1$value <- fct_rev(anket_lf1$value)
anket_lf2$value <- fct_rev(anket_lf2$value)

ggplot(anket_lf1, aes(variable, fill= value)) +
     geom_bar(stat = "count", position = "stack") +
     labs(x= "Anket Sorusu",
          y= "Katılımcı Sayısı",
          fill= "Cevaplar") +
     coord_flip() +
     scale_fill_brewer(palette = "Spectral", direction= -1) +
     theme_light()


ggplot(anket_lf2, aes(variable, fill= value, width=1)) +
     geom_bar(stat = "count", position = "dodge") +
     labs(x= "Anket Sorusu",
          y= "Katılımcı Sayısı",
          fill= "Cevaplar") +
     scale_fill_brewer(palette = "Spectral", direction = -1) +
     theme_light()
