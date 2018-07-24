setwd('/Users/peterstafford/generalassembly/music_classification_repo/music_classification')
### line 2 - 11 loads in the file names for the rock .wav files
tN<-"music/rock.csv"
tVRock<-vector();
df <- read.csv(tN, header=F)
tLength<-dim(df)[1]
tR<-""
for(i in 1:tLength)
{
	 tVRock[i]<-paste(tR,as.vector(df[i,]),sep="");	
}
require(stats)
require(tuneR)
require(randomForest)
require(ModelMetrics)
tN<-"music/sinatra.csv"
tVSinatra<-vector();
df <- read.csv(tN, header=F)
tLength<-dim(df)[1]
### line 20 - 27 loads in the file names for the easy listening .wav files

tR<-""
for(i in 1:tLength)
{
	 tVSinatra[i]<-paste(tR,as.vector(df[i,]),sep="");	
}

#### - this code is commented out. The abridge version does not use the .wav file.
### It used the principle components derived from the wave files via melfcc 
# numOfSec<-30;
# #tSample<-20000;
# tSample<-20000;
# tFrom<-round(1323000);
# tTo<-round(2646000);
# tFilename<-tVRock[1];
# tW<-readWave(tFilename, from = tFrom, to = tTo, header = FALSE, toWaveMC = NULL)
# #tW<-readWave(tFilename, header = FALSE, toWaveMC = NULL)
# #tW<-randomizeWave(tW,numOfSec);
 # #show(tW)
 # df<-mono(tW, which = c("left"))
 # m1 <- melfcc(df)
 # tMtrxCols<-(dim(m1)[1]*dim(m1)[2]*2); 
## The code below process the .wav file. It extracts features based on the melfcc format. This section is for rock 
# tRockMtrx<-matrix(0,length(tVRock),tMtrxCols);
# tRockLength<-vector();
# for(i in 1:length(tVRock))
# {
	   # tFilename<-tVRock[i];
	   # #tW<-readWave(tFilename,header = FALSE, toWaveMC = NULL)
	   # #tW<-randomizeWave(tW,numOfSec);
	   # #tW<-downsample(tW,tSample)
	   # #tRockLength[i]<-length(tW@left)/tW@samp.rate;
	   # tW<-readWave(tFilename, from = tFrom, to = tTo, header = FALSE, toWaveMC = NULL)
	   # tW<-downsample(tW,tSample);
	   # #tW<-normalize(tW, unit = c("16"),center=F,  rescale = TRUE)
	   # tChannel='both';
	   # if(length(tW@right)==0)
	   # {
	   	   # tChannel='left';
	   # }
       # #show(tW)
       # df<-mono(tW, which = c(tChannel))
       # #show(tW)
       # m1 <- melfcc(df)
       # dta1<-deltas(m1);
       # v1 <- as.vector(matrix(m1, 1,tMtrxCols/2))
       # v2 <- as.vector(matrix(dta1,1,tMtrxCols/2));
       # v1<-c(v1,v2);
   # tNA<-which(is.na(v1))
       # v1[tNA]=0;     
   # tRockMtrx[i,]<-v1;    
# }
## The section process the easy listening audio
# tSinatraMtrx<-matrix(0,length(tVSinatra),tMtrxCols);
# tSinatraLength<-vector();
# for(i in 1:length(tVSinatra))
# {
	   # tFilename<-tVSinatra[i];
	   # #tW<-readWave(tFilename,header = FALSE, toWaveMC = NULL)
	   # #tW<-randomizeWave(tW,numOfSec);
	   # #tSinatraLength[i]<-length(tW@left)/tW@samp.rate;
	   # tW<-readWave(tFilename, from = tFrom, to = tTo, header = FALSE, toWaveMC = NULL)
	   # tW<-downsample(tW,tSample);
	   # #tW<-normalize(tW, unit = c("16"), center = F,  rescale = TRUE)
	   # tChannel='both';
	   # if(length(tW@right)==0)
	   # {
	   	   # tChannel='left';
	   # }
       # #show(tW)
       # df<-mono(tW, which = c(tChannel))
       # m1 <- melfcc(df)
       # #v1 <- as.vector(matrix(m1, 1,tMtrxCols))
       # dta1<-deltas(m1);
       # v1 <- as.vector(matrix(m1, 1,tMtrxCols/2))
       # v2 <- as.vector(matrix(dta1,1,tMtrxCols/2));
       # v1<-c(v1,v2);
   # tNA<-which(is.na(v1))
       # v1[tNA]=0;           
   # tSinatraMtrx[i,]<-v1;    
# }
rockLabel<-rep(1,length(tVRock))
#tRockMtrx<-cbind(rockLabel,tRockMtrx)
sinatraLabel<-rep(2,length(tVSinatra))
tResponses<-c(rockLabel,sinatraLabel);
#tSinatraMtrx<-cbind(sinatraLabel,tSinatraMtrx)
#tData<-rbind(tRockMtrx,tSinatraMtrx)
library(randomForest)
#pca1<-prcomp(tData[,1:35976])

#tNN<-getSongDistance(pca1$x,5);
#pca<-cbind(pca1$x[,1:10],tNN);
#pca<-pca1$x[,1:6];
load('principal_components.rdata') 
#tResponses<-c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,3,3,3,3,3,3,3,3,3,3);
tFactor<-factor(tResponses)
tCount<-0;
tListCorrect<-vector(mode='numeric',length=84);
tListInCorrect<-vector(mode='numeric',length=84);
tIndex<-1;
tPred=NULL
tActual=NULL
for(i in 1:1000)
{
	#randomly leave 5 out.   
	tOut<-sample(1:79,5);
	tPCA<-pca[-tOut,]
	tPCAFactor<-tFactor[-tOut];
    tFRF<-randomForest(tPCA[,1:6],ntree=1001,tPCAFactor,importance=T)
    music.pred <- predict(tFRF, pca[tOut,1:6]);
    tPred=c(tPred,as.numeric(music.pred))
    tActual=c(tActual,as.numeric(tFactor[tOut]))
}
caret::confusionMatrix(as.factor(tPred),as.factor(tActual),mode="everything")