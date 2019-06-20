#行を海域にし、列を年代にしたもの

data = read.csv("C:/cygwin64/home/kaito/Program/R_PCA/SSTave_label_nonyear_row.csv",header=T, row.names=1)

rain = read.csv("C:/cygwin64/home/kaito/Program/R_PCA/rain.csv",header=T)

rain

#scale(data)
#svd(data)
pca = prcomp(data, scale=T)

biplot(pca)
pca
summary(pca)


ScatterplotMatrix <- pca


#主成分得点をグラフにプロット

 library(maptools)
 par(las=1)
 plot(x=NULL,type="n",xlab="PC1",ylab="PC2",xlim=c(-7,7),ylim=c(-7,7),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="l")
 axis(side=1,at=seq(-7,7,1),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7))
 axis(side=2,at=seq(-7,7,1),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7))
 points(x=pca$x[,1],y=pca$x[,2],pch=16,col="#ff8c00")
 pointLabel(x=pca$x[,1],y=pca$x[,2],labels=rownames(data),cex=0.8)
 box(bty="l")
 
 pca$x
 

 pc <- pca$x
 
 pcs <- pc[,1:6]
 
 pcs
 
 #主成分の軸方向をプロット

 loading=sweep(pca$rotation,MARGIN=2,pca$sdev,FUN="*")
 library(maptools)
  par(las=1)
  plot(x=NULL,type="n",xlab="PC1",ylab="PC2",xlim=c(-1,1),ylim=c(-1,1),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  axis(side=1,at=seq(-1,1,0.2),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0))
  axis(side=2,at=seq(-1,1,0.2),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0))
  for(i in 1:41)
  {
    	arrows(0,0,loading[i,1],loading[i,2],col="#ff8c00",length=0.1)
    }
  pointLabel(x=loading[,1],y=loading[,2],labels=rownames(loading),cex=1)
  box(bty="l")
 
 summary(pca)

 #～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～
 
  
  
  #行を年代にし、列を海域にしたもの
  
  data = read.csv("C:/cygwin64/home/kaito/Program/R_PCA/SSTave_label_nonyear.csv",header=T, row.names=1)
  
  
  #scale(data)
  #svd(data)
  pca = prcomp(data, scale=T)
  
  biplot(pca)
  pca
  summary(pca)
  
  
  ScatterplotMatrix <- pca
  
  
  #主成分得点をグラフにプロット
  
  library(maptools)
  par(las=1)
  plot(x=NULL,type="n",xlab="PC1",ylab="PC2",xlim=c(-7,7),ylim=c(-7,7),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="l")
  axis(side=1,at=seq(-7,7,1),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7))
  axis(side=2,at=seq(-7,7,1),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7))
  points(x=pca$x[,1],y=pca$x[,2],pch=16,col="#ff8c00")
  pointLabel(x=pca$x[,1],y=pca$x[,2],labels=rownames(data),cex=0.8)
  box(bty="l")
  
  pca$x
  
  
  #主成分の軸方向をプロット
  
  loading=sweep(pca$rotation,MARGIN=2,pca$sdev,FUN="*")
  library(maptools)
  par(las=1)
  plot(x=NULL,type="n",xlab="PC1",ylab="PC2",xlim=c(-1,1),ylim=c(-1,1),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  axis(side=1,at=seq(-1,1,0.2),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0))
  axis(side=2,at=seq(-1,1,0.2),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0))
  for(i in 1:21)
  {
    arrows(0,0,loading[i,1],loading[i,2],col="#ff8c00",length=0.1)
  }
  pointLabel(x=loading[,1],y=loading[,2],labels=rownames(loading),cex=1)
  box(bty="l")
  
  
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  #データを回帰分析する
   
  pc <- pca$x
  
  pcs <- pc[,1:6]
  

  for(i in 1:6){
    nam <- paste("var_",i+1,sep="")
    assign(nam,c(pcs[,i]))
    
  }
  
  for(i in 1){
    nam <- paste("var_",i,sep="")
    assign(nam,c(rain[,i]))
    
  }
  
  
  lm1 <- lm(var_1 ~
              var_2 + var_3 + var_4 + var_5 + var_6 + var_7 
         )

  summary(lm1)
  summary(pca)
  step(lm1)

  lm2 <-lm(var_1 ~
             var_2 + var_3 + var_5  
  )
  
  summary(lm2)
  
# 主成分分析の結果をグラフに描く
# install.packages("devtools")
# devtools::install_github("vqv/ggbiplot")
#library(ggbiplot)

#ggbiplot(
#  pca, 
#  obs.scale = 1, 
#  var.scale = 1, 
#  groups = iris$Species, 
 # ellipse = TRUE, 
#  circle = TRUE,
#  alpha=0.5
#)