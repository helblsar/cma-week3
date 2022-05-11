
#----------------------------------------------------------------------------
# Exercise 3
# Author: Sarah Helbling
# PTED | Computational Movement analysis
#----------------------------------------------------------------------------

library(tidyverse)

# Task 1 - Segmentation
caro<- read.delim("caro60.csv", sep=",")
#use a temporal window v of 6 min. -> lead(x, 3), lag(x,3)
caro<- caro%>%
  mutate(
    nMinus3 = sqrt((lag(E, 3)-E)^2 + (lag(N,3)-N)^2),
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2))

caro<- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nPlus3))
  ) %>%
  ungroup()

#----------------------------------------------------------------------------

# Task 2 - Specify and apply threshold d

#getting an overview of stepMean
ggplot(caro, aes(x=TierName, y=stepMean)) +
  geom_boxplot(fill="forestgreen") +
  stat_summary(fun = mean, geom = "point", shape=6, size = 2) 

#apply threshold
caro<- caro %>%
  ungroup() %>%
  mutate(static=stepMean < mean(stepMean, na.rm=T))

#----------------------------------------------------------------------------

# Task 3 - Visualize segmented trajectories

ggplot(caro, aes(x=E,y=N, color = static)) + 
  geom_path(color="grey") +
  geom_point() +
  coord_equal() +
  theme_minimal()

#----------------------------------------------------------------------------

# Task 4 - Segment-based analysis
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

#assign segment_id
caro <- caro %>%
  mutate(segment_id = rle_id(static)) %>%
  group_by(segment_id) %>%
  filter(static=="FALSE")

#identify the segment_ids with more than five steps (one step=1min.)
caro_longseg <- caro %>%
  tally() %>%
  filter(n>=5)

#filtering out the segments that are longer than 5min.
caro_a <- caro %>%
  filter(segment_id %in% caro_longseg$segment_id)



library("ggpubr")

all_seg<- ggplot(caro, aes(x=E,y=N, color = segment_id)) + 
  ggtitle("All segments(uncleaned)")+
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position="none")
long_seg<- ggplot(caro_a, aes(x=E,y=N, color = segment_id)) + 
  ggtitle("Long segments (removed segments <5)") +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position="none")
ggarrange(all_seg,long_seg,
          labels = ("Moving segments coloured by segment ID"),
          ncol=2)

#----------------------------------------------------------------------------

# Task 5 - Similarity measure

pedestrian<- read.delim("pedestrian.tx.txt", sep=",")


ggplot(pedestrian, aes(x=E,y=N, alpha=TrajID)) + 
  geom_path() +
  geom_point() +
  coord_equal() +
  facet_wrap(~TrajID, nrow=2) +
  ggtitle("Visual comparison of the 6 trajectories") +
  theme_minimal()

#----------------------------------------------------------------------------

# Task 6 - Calculate similarity

library(SimilarityMeasures)

traj_1<- pedestrian %>%
  filter(TrajID==1) %>%
  select(-one_of('TrajID','DatetimeUTC')) %>%
  as.matrix()
traj_2<- pedestrian %>%
  filter(TrajID==2) %>%
  select(-one_of('TrajID','DatetimeUTC')) %>%
  as.matrix()
traj_3<- pedestrian %>%
  filter(TrajID==3) %>%
  select(-one_of('TrajID','DatetimeUTC')) %>%
  as.matrix()
traj_4<- pedestrian %>%
  filter(TrajID==4) %>%
  select(-one_of('TrajID','DatetimeUTC')) %>%
  as.matrix()
traj_5<- pedestrian %>%
  filter(TrajID==5) %>%
  select(-one_of('TrajID','DatetimeUTC')) %>%
  as.matrix()
traj_6<- pedestrian %>%
  filter(TrajID==6) %>%
  select(-one_of('TrajID','DatetimeUTC')) %>%
  as.matrix()

DTW<- c(DTW(traj_1, traj_2, 4),DTW(traj_1, traj_3, 4),
        DTW(traj_1, traj_4, 4),DTW(traj_1, traj_5, 4),
        DTW(traj_1, traj_6, 4))
EditDist<- c(EditDist(traj_1, traj_2,20), EditDist(traj_1,traj_3,20),
             EditDist(traj_1, traj_4,20), EditDist(traj_1,traj_5,20),
             EditDist(traj_1, traj_6,20))
Frechet<- c(Frechet(traj_1, traj_2, testLeash=-1),Frechet(traj_1, traj_3, testLeash=-1),
            Frechet(traj_1, traj_4, testLeash=-1),Frechet(traj_1, traj_5, testLeash=-1),
            Frechet(traj_1, traj_5, testLeash=-1))
LCSS<- c(LCSS(traj_1,traj_2, 2, 2, 0.5),LCSS(traj_1,traj_3, 2, 2, 0.5),
         LCSS(traj_1,traj_4, 2, 2, 0.5),LCSS(traj_1,traj_5, 2, 2, 0.5),
         LCSS(traj_1,traj_6, 2, 2, 0.5))
r_nr<-c(2:5)
similarities<-data.frame(r_nr,DTW,EditDist,Frechet,LCSS)



dtw<-ggplot(similarities) +
  geom_bar(aes(x=r_nr, y=DTW, fill=DTW), stat="identity")+
  theme(legend.position="none")
editdist<-ggplot(similarities) +
  geom_bar(aes(x=r_nr, y=EditDist, fill=EditDist), stat="identity")+
  theme(legend.position="none")
frechet<-ggplot(similarities) +
  geom_bar(aes(x=r_nr, y=Frechet, fill=Frechet), stat="identity")+
  theme(legend.position="none")
lcss<-ggplot(similarities) +
  geom_bar(aes(x=r_nr, y=LCSS, fill=LCSS), stat="identity")+
  theme(legend.position="none")
ggarrange(dtw, editdist, frechet, lcss,
          labels=c("DTW","EditDist","Frechet","LCSS"),
          ncol=2,nrow=2)
