
library(tidyverse)
library(RColorBrewer)
setwd("~/Desktop/DogaBilimleriR/")

file_adaylar <- as.tibble(read.csv("katilimcilar.csv"))

# remove duplicates
adaylar <- file_adaylar %>% 
     distinct(isim, .keep_all = TRUE) %>% 
     select(isim, university, area, level, PreviousWorkshop, rlevel, 
            department_, study_area)

# remove prep years and edit factors
adaylar <- adaylar %>% 
     filter(level != "Hazırlık" ) %>%
     mutate(level = factor(level, levels = c("1_2", "3_4","Mezun", "YL_Doktora", 
                                             "Doktora+"))) %>% 
     mutate(level = recode(level, "1_2" = "Lisans 1",
                           "3_4" = "Lisans 2",
                           "Mezun" = "Lisans mezunu",
                           "YL_Doktora" = "Yüksek Eğitim"))

# graph
ggplot(adaylar, aes(level, fill= rlevel)) +
     geom_histogram(stat= "count") +
     scale_fill_brewer(palette= "Set2", direction = -1)

# filters
levels(adaylar$area)

adaylar_s <- adaylar %>% 
     filter(rlevel %in% c("Giriş", "Hiç")) %>% 
     filter(level %in% c("Yüksek Eğitim", "Doktora+")) %>% 
     filter(area %in% c("Aerobiyoloji", "Atmosfer ve iklim", "Biyocoğrafya",
                        "Biyoloji", "Botanik", "Deniz Bilimleri",
                        "Doğa Koruma", "Ekoloji", "Ornitoloji", "Orman", 
                        "Zooloji"))