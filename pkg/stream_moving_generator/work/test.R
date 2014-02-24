library(scales)

#add noise see how resistant
#test multiple dimensions?

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

#seperating
c1 <- MGC_Linear()
c2 <- MGC_Linear()
add_keyframe(c1,1,10,10,c(0,0),c=1)
add_keyframe(c1,50,10,10,c(50,50),c=1)
add_keyframe(c1,100,10,10,c(50,100),c=1)
add_keyframe(c2,1,10,10,c(0,0),c=1)
add_keyframe(c2,50,10,10,c(50,50),c=2)
add_keyframe(c2,100,10,10,c(100,50),c=2)
dsd <- DSD_MG(2,c1,c2)
reset_stream(dsd)
tnn <- DSC_tNN(alpha=.5,r=10,lambda=0.1,decay_interval=1)

#not run
#tnnPurity <- animate_cluster(tnn,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)


##graph through time tnn
reset_stream(dsd)
plot(dsd,2500,alpha(1, 0.05),xlim=c(-20,120),ylim=c(-10,120))
reset_stream(dsd)
tnn <- DSC_tNN(alpha=.5,r=10,lambda=0.1,decay_interval=1)
cluster(tnn,dsd,100)
par(new=TRUE)
plot(tnn,type="macro",xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n')
cluster(tnn,dsd,1000)
par(new=TRUE)
plot(tnn,type="macro",xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n')
cluster(tnn,dsd,1000)
par(new=TRUE)
plot(tnn,type="macro",xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n')


reset_stream(dsd)
kmeans <- DSC_Kmeans(2)
clustream <- DSC_CluStream(k=10,t=5,horizon=10)
clustreamPurity <- animate_cluster(clustream,dsd,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)


#Graph through time clustream
reset_stream(dsd)
plot(dsd,2500,alpha(1, 0.03),xlim=c(-20,120),ylim=c(-10,120))
reset_stream(dsd)
clustream <- DSC_CluStream(k=10,t=5,horizon=10)
cluster(clustream,dsd,100)
recluster(kmeans,clustream)
par(new=TRUE)
plot(clustream,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)
cluster(clustream,dsd,1000)
recluster(kmeans,clustream)
par(new=TRUE)
plot(clustream,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)
cluster(clustream,dsd,1000)
recluster(kmeans,clustream)
par(new=TRUE)
plot(clustream,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)

reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
clustreePurity <- animate_cluster(clustree,dsd,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)

#graph through time clustree
reset_stream(dsd)
plot(dsd,2500,alpha(1, 0.03),xlim=c(-20,120),ylim=c(-10,120))
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
cluster(clustree,dsd,100)
recluster(kmeans,clustree)
par(new=TRUE)
plot(clustree,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)
cluster(clustree,dsd,1000)
recluster(kmeans,clustree)
par(new=TRUE)
plot(clustree,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)
cluster(clustree,dsd,1000)
recluster(kmeans,clustree)
par(new=TRUE)
plot(clustree,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)

reset_stream(dsd)
denstream <- DSC_DenStream(epsilon=5,lambda=.1)
dbscanPurity <- animate_cluster(denstream,dsd,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)

#graph through time denstream
reset_stream(dsd)
plot(dsd,2500,alpha(1, 0.03),xlim=c(-20,120),ylim=c(-10,120))
reset_stream(dsd)
denstream <- DSC_DenStream(epsilon=5,lambda=.1)
cluster(denstream,dsd,100)
recluster(kmeans,denstream)
par(new=TRUE)
plot(denstream,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)
cluster(denstream,dsd,1000)
recluster(kmeans,denstream)
par(new=TRUE)
plot(denstream,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)
cluster(denstream,dsd,1000)
recluster(kmeans,denstream)
par(new=TRUE)
plot(denstream,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',col_clusters=alpha(1, 0.25))
par(new=TRUE)
plot(kmeans,xlim=c(-20,120),ylim=c(-10,120),ann=FALSE,xaxt='n',yaxt='n',pch=10)


plot(tnnPurity,type="l", col=1, ylim=c(0,1))
points(clustreamPurity,type="l",col=2)
points(clustreePurity,type="l",col=3)

reset_stream(dsd)
tnn <- DSC_tNN(alpha=.5,r=5,lambda=0.1,decay_interval=1)
tnnRand <- animate_cluster(tnn,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)
reset_stream(dsd)
kmeans <- DSC_Kmeans(2)
clustream <- DSC_CluStream(k=25,t=5,horizon=10)
clustreamRand <- animate_cluster(clustream,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
clustreeRand <- animate_cluster(clustree,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)

plot(tnnRand,type="l", col=1, ylim=c(0,1))
points(clustreamRand,type="l",col=2)
points(clustreeRand,type="l",col=3)



#merging
dsd <- DSD_MG()
c1 <- MGC_Linear()
c2 <- MGC_Linear()
add_keyframe(c1,1,1,10,50,0,c=1)
add_keyframe(c1,50,1,10,50,50,c=1)
add_keyframe(c1,100,1,10,100,100,c=1)
add_keyframe(c2,1,1,10,0,50,c=2)
add_keyframe(c2,50,1,10,50,50,c=1)
add_keyframe(c2,100,1,10,100,100,c=1)
add_cluster(dsd,c1)
add_cluster(dsd,c2)
reset_stream(dsd)
dsc <- DSC_tNN(alpha=.5,r=5,lambda=0.1,decay_interval=1)
tnnPurity <- animate_cluster(dsc,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)
reset_stream(dsd)
kmeans <- DSC_Kmeans(2)
clustream <- DSC_CluStream(k=25,t=5)
clustreamPurity <- animate_cluster(clustream,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
clustreePurity <- animate_cluster(clustree,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="purity",horizon=100)

plot(tnnPurity,type="l", col=1, ylim=c(0,1))
points(clustreamPurity,type="l",col=2)
points(clustreePurity,type="l",col=3)

reset_stream(dsd)
dsc <- DSC_tNN(alpha=.5,r=5,lambda=0.1,decay_interval=1)
tnnRand <- animate_cluster(dsc,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)
reset_stream(dsd)
kmeans <- DSC_Kmeans(2)
clustream <- DSC_CluStream(k=25,t=5)
clustreamRand <- animate_cluster(clustream,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
clustreeRand <- animate_cluster(clustree,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)

plot(tnnRand,type="l", col=1, ylim=c(0,1))
points(clustreamRand,type="l",col=2)
points(clustreeRand,type="l",col=3)


#both
dsd <- DSD_MG()
c1 <- MGC_Linear()
c2 <- MGC_Linear()
add_keyframe(c1,1,1,10,50,0)
add_keyframe(c1,50,1,10,50,50)
add_keyframe(c1,100,1,10,50,100)
add_keyframe(c2,1,1,10,0,50)
add_keyframe(c2,50,1,10,50,50)
add_keyframe(c2,100,1,10,100,50)
add_cluster(dsd,c1)
add_cluster(dsd,c2)
reset_stream(dsd)
dsc <- DSC_tNN(alpha=.25,r=5,lambda=0.05,decay_interval=10)
animate_cluster(dsc,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100))
reset_stream(dsd)
clustream <- DSC_CluStream(k=25,t=5)
animate_cluster(clustream,dsd,n=2500,xlim=c(0,100),ylim=c(0,100))
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
animate_cluster(clustree,dsd,n=2500,xlim=c(0,100),ylim=c(0,100))

reset_stream(dsd)
dsc <- DSC_tNN(alpha=.5,r=5,lambda=0.1,decay_interval=1)
tnnRand <- animate_cluster(dsc,dsd,n=2500,type="macro",xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)
reset_stream(dsd)
kmeans <- DSC_Kmeans(2)
clustream <- DSC_CluStream(k=25,t=5)
clustreamRand <- animate_cluster(clustream,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
clustreeRand <- animate_cluster(clustree,dsd,kmeans,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)

plot(tnnRand,type="l", col=1, ylim=c(0,1))
points(clustreamRand,type="l",col=2)
points(clustreeRand,type="l",col=3)

#speed
dsd <- DSD_MG()
c1 <- MGC_Linear()
c2 <- MGC_Linear()
c3 <- MGC_Linear()
c4 <- MGC_Linear()
c5 <- MGC_Linear()
add_keyframe(c1,1,1,20,10,0)
add_keyframe(c1,1600,1,20,10,1000)
add_keyframe(c2,1,1,20,30,00)
add_keyframe(c2,800,1,20,30,1000)
add_keyframe(c3,1,1,20,50,00)
add_keyframe(c3,400,1,20,50,1000)
add_keyframe(c4,1,1,20,70,00)
add_keyframe(c4,200,1,20,70,1000)
add_keyframe(c5,1,1,20,90,00)
add_keyframe(c5,100,1,20,90,1000)
add_cluster(dsd,c1)
add_cluster(dsd,c2)
add_cluster(dsd,c3)
add_cluster(dsd,c4)
add_cluster(dsd,c5)
reset_stream(dsd)
dsc <- DSC_tNN(alpha=.25,r=5,lambda=0.01,decay_interval=10)
animate_cluster(dsc,dsd,n=1500,type="macro",xlim=c(0,100),ylim=c(0,200))
reset_stream(dsd)
clustream <- DSC_CluStream(k=25,t=5)
animate_cluster(clustream,dsd,n=1500,xlim=c(0,100),ylim=c(0,200))
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
animate_cluster(clustree,dsd,n=1500,xlim=c(0,100),ylim=c(0,200))

#variance
dsd <- DSD_MG()
c1 <- MGC_Linear()
add_keyframe(c1,1,1,10,150,150)
add_keyframe(c1,100,1500,10,150,150)
add_cluster(dsd,c1)
reset_stream(dsd)
dsc <- DSC_tNN(alpha=.25,r=20,lambda=0,decay_interval=10)
animate_cluster(dsc,dsd,n=1500,type="macro",xlim=c(0,300),ylim=c(0,300))
reset_stream(dsd)
clustream <- DSC_CluStream(k=25)
animate_cluster(clustream,dsd,n=1500,xlim=c(0,300),ylim=c(0,300))
reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=3)
animate_cluster(clustree,dsd,n=1500,xlim=c(0,300),ylim=c(0,300))









dsc <- DSC_tNN(alpha=.10,r=12,lambda=.010)
reset_stream(dsd)
cluster(dsc,dsd,100)
plot(dsc,dsd,1,type="macro",xlim=c(0,300),ylim=c(0,300))
cluster(dsc,dsd,400)
plot(dsc,dsd,1,type="macro",xlim=c(0,300),ylim=c(0,300))
cluster(dsc,dsd,500)
plot(dsc,dsd,1,type="macro",xlim=c(0,300),ylim=c(0,300))




MGC_Linear(dimension=)

dsd <- DSD_MG()
c1 <- MGC_Function(list(function(t){1},function(t){100}),function(t){25},list(function(t){t},function(t){t}))
c2 <- MGC_Function(function(t){.1*t},function(t){25},list(function(t){sin(t/10)*50+50},function(t){t}))
add_cluster(dsd,c1)
add_cluster(dsd,c2)
reset_stream(dsd)
animate_data(dsd,5000,xlim=c(0,100),ylim=c(0,100))


dsd <- DSD_MG()
c1 <- MGC_Random(c(50,50),1,20,.25)
c2 <- MGC_Random(c(50,50),.25,20,1)
add_cluster(dsd,c1)
add_cluster(dsd,c2)
reset_stream(dsd)
animate_data(dsd,5000,xlim=c(0,100),ylim=c(0,100))


plot(cl, points_dsd,
     n=nrow(points),
     col_points=col[horizon-nrow(points)+1: horizon],...)












c1 <- MGC_Linear()
c2 <- MGC_Linear()
add_keyframe(c1,1,10,10,0,0,c=1)
add_keyframe(c1,50,10,10,50,50,c=1)
add_keyframe(c1,100,10,10,50,100,c=1)
add_keyframe(c2,1,10,10,0,0,c=1)
add_keyframe(c2,50,10,10,50,50,c=2)
add_keyframe(c2,100,10,10,100,50,c=2)
dsd <- DSD_MG(2,c1,c2,noise=.5,nRange=c(-20,120,-20,120))
reset_stream(dsd)
tnn <- DSC_tNN(alpha=.5,r=10,lambda=0.1,decay_interval=1,minweight=0.15)
tnnPurity <- animate_cluster(tnn,dsd,n=2500,type="macro",xlim=c(-20,120),ylim=c(-20,120),evaluationMethod="rand",horizon=100)

reset_stream(dsd)
clustream <- DSC_CluStream(k=10,t=5,horizon=10)
clustreamPurity <- animate_cluster(clustream,dsd,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)

reset_stream(dsd)
clustree <- DSC_ClusTree(maxHeight=8,horizon=10)
clustreePurity <- animate_cluster(clustree,dsd,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)

reset_stream(dsd)
denstream <- DSC_DenStream(epsilon=5,lambda=.1,beta=.2,mu=.75)
dbscanPurity <- animate_cluster(denstream,dsd,n=2500,xlim=c(0,100),ylim=c(0,100),evaluationMethod="rand",horizon=100)

plot(tnnPurity,type="l", col=1, ylim=c(0,1))
points(clustreamPurity,type="l",col=2)
points(clustreePurity,type="l",col=3)
points(dbscanPurity,type="l",col=4)
