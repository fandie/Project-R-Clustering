### Packages


library(ggplot2)
library(dplyr)
library(scales)
library(randomForest)
library(modelr)
library(Amelia)
##---------------- Packages Cluster
library(cluster)
library(factoextra)
library(tidyverse)
library(rpart.plot)
library(dendextend)

##---------------- Package Klasifikasi
library(caTools)
library(rpart)
library(gridExtra)





###--------------------------------------------------------------------------------###





ev <- read.csv('evdataset.csv')
dim (ev) # Melihat banyakanya kolom dan baris
colnames (ev) # Melihat kolom
str(ev)

ev_new <- select(ev, -id, -link) # Menghilangkan kolom id dan kolom link
ev_new$Electric.Range<-as.numeric(ev_new$Electric.Range)
summary(ev_new)
str(ev_new) # Pengecekan stuktur data

# Pengecekan data lengkap atau tidak
any(is.na(ev_new))

# Pengecekan data lengkap atau tidak (visual)
missmap(ev_new, main = 'Data Mobil EV', col=c('yellow', 'black'), legend = FALSE)

### Dari pengecekan data, tidak ada data yang (NA/tidak lengkap/kosong) karena dari any(is.na(ev_new)) = False

###--------------------------------------------------------------------------------###

# 1. Exploratiory data Analys (Univariate data Analys)

# 1.1 Ploting = Box Plot
# 1.1.1 Untuk melihat Peforma antara kesemua Merek Mobil Listrik dengan Electric range
ggplot(data = ev_new)+
  geom_boxplot(mapping = aes (ev_new$Make, ev_new$Electric.Range))+
                 xlab('Make')+
                 ylab('Electric.Range')+
  coord_flip()


# 1.1.2 Untuk melihat peforma dari mobil listrik Audi Saja dengan Electric Range
audi_ev <- subset(ev_new, ev_new$Make == 'Audi')
ggplot(audi_ev, aes(audi_ev$Make, audi_ev$Electric.Range)) +
         geom_boxplot(aes(audi_ev = 'Black' ), varwidth = TRUE)

summary (audi_ev)
## sample Data untuk peforma mobil listrik Audi untuk Electric Range diperoleh 
## Minimum Electric Range sebesar = 280
## Quartile 1 sebesar             = 313.8 
## Median sebesar                 = 382.5
## Mean                           = 360.9 
## Quartile 3 sebesar             = 401.2
## Maximum sebesar                = 425


# 1.1.3 gabungan keseluruhan dari peforma Merek Mobil dengan Electric Range

all_ev <- (data = ev_new)
ggplot(data = ev_new, aes(x=Electric.Range)) +
  geom_boxplot()+
coord_flip()
summary (all_ev)

## Untuk Keseluruhan peforma mobil litrik untuk Electric Range diperoleh
## Minimum Electric Range sebesar = 135
## Quartile 1 sebesar             = 265
## Median sebesar                 = 352.5
## Mean                           = 345.6
## Quartile 2 sebesar             = 410
## Maximum/outlier Sebesar        = 640


# 1.2 Ploting = point

# 1.2.1 Peforma antara kesemua Merek Mobil Listrik dengan Electric range
ggplot(data = ev_new)+
  geom_point(mapping = aes (ev_new$Make, ev_new$Electric.Range))+
  labs(title = 'Peforma Electric Range Mobil Ev',
       x='Make', y= 'Electric Range')+
  coord_flip()






###--------------------------------------------------------------------------------###







# 2. Korelasi (Hubungan Data dengan Data lainnya)
## Korelasi hanya dapat berupa data numerik
ev_3 <- subset (ev_new, select = -c(Drive))

# 2.1 korelasi Kesemua data
library(ggcorrplot)

kor_ev <- cor(ev_3[ ,2:24])
view (kor_ev)
kor_ev[,1]
ggcorrplot(kor_ev, lab = FALSE)

# 2.2  Contoh Korelasi

# 2.2.1 korelasi Tidak berhubungan (0)

cor(ev_3$Length, ev_3$Acceleration.0...100.km.h)
##  Korelasi antara Length dengan Acceleration.0...100.km.h Diperoleh korelasi sebesar 
##  0.009513124 dimana korelasi antara data tersebut tidak berhubungan

# 2.2.2 korelasi berhubungan terbalik (-)

cor(ev_3$Acceleration.0...100.km.h, ev_3$Top.Speed)
##  Korelasi antara Acceleration.0...100.km.h dengan Top.Speed Diperoleh korelasi sebesar 
##  -0.8724949 dimana korelasi antara data tersebut berhubungan terbalik

# 2.2.3 Korelasi berhubungan Searah (mendekati 1)

cor(ev_3$Length, ev_3$Width)
##  Korelasi antara Length dengan Width Diperoleh korelasi sebesar 
##  0.8590183 dimana korelasi antara data tersebut berhubungan Searah atau mendekati 1






#-------------------- Kluster Hirarki --------------------------






# 3. CLUSTER HIRARKI

#Melakukan transformasi data 
#Scale hanya berupa numerik maka untuk data Make tidak di masukan

scale_ev <- scale(ev_3[9:11]) #Melakukan scaling data dan mengambil kolom 9 s/d 11 (Top.Speed, Electric.Range, Total.Power)
scale_ev

# Distance
dis_ev <- dist(scale_ev) #Mencari jarak/distance dari hasil scaling
dis_ev

### Determining Optimal Clusters


fviz_nbclust(scale_ev, FUN = hcut, method = "wss")
fviz_nbclust(scale_ev, FUN = hcut, method = "silhouette")


#Cluster menggunakan metode hirarki
hc_ev <- hclust(dis_ev, method = "complete")
plot(hc_ev, ev_3$Total.Power, labels = ev_3$Total.Power, cex = 0.6, hang = -1)
rect.hclust(hc_ev, 5) #Memvisualisasikan hasil dari method silhouette yang sudah didapatkan

## melihat anggota pada kluster

isi_hc_ev = data.frame(id=ev_3$Make, cutree(hc_ev,k=5))
view (isi_hc_ev)


