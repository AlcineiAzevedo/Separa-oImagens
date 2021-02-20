remove(list=ls())
setwd("D:/Backup Pendrive/UFMG/Disciplinas/Estatistica experimental/Videos/_Analise computacional de imagens/6 segmentação/6f")
library(EBImage)
library(imager)

nomes=list.files(pattern = ".jpg")

MAT=NULL
for(i in 1:length(nomes)){

im=readImage(nomes[i])
#plot(im)
mask=(im@.Data[,,1]>otsu(im@.Data[,,1]))
mask2=bwlabel(mask)
area=computeFeatures.shape(mask2)
id=area[,1]>400
area2=area[id,]

Coord=computeFeatures.moment(mask2)[id,]

Mat=cbind(Foto=nomes[i],Objeto=1:nrow(Mat),area2[,1:2],Coord[,3:4])
rownames(Mat)=1:nrow(Mat)
MAT=rbind(MAT,Mat)
png(paste("id_",nomes[i]),".png")
plot(im)
text(Coord[,1],Coord[,2],1:nrow(Mat),col=2)
dev.off()

writeImage(mask2,file="a.png")

im2=load.image("a.png")

for (j in 1:nrow(Coord)){
im3=px.flood(im2,Coord[j,1],Coord[j,2])

save.image(as.cimg(im3),file="a.png")

im4=readImage("a.png")
mask4=im4@.Data

mask5=fillHull(mask4)
#EBImage::display(mask5)

Data=im@.Data*0

Data[,,1][mask5==1]=im@.Data[,,1][mask5==1]
Data[,,2][mask5==1]=im@.Data[,,2][mask5==1]
Data[,,3][mask5==1]=im@.Data[,,3][mask5==1]

im5=im
im5@.Data=Data
#plot(im5)

writeImage(im5,file=paste(nomes[i],"_",j,".png"))
print(paste(i,j))
}
}

write.table(Mat,"Resultados.txt",sep="\t",row.names = F)
