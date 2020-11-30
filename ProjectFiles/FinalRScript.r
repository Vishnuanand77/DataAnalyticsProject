library(data.table)
library(Metrics)


##reading the csv files from our local systems into RStudio
drivers<-fread("C:/Users/Ramesh Kini/Desktop/SEM_5/Data_Analytics/FinalProject/Analysis/drivers.csv")
races<-fread("C:/Users/Ramesh Kini/Desktop/SEM_5/Data_Analytics/FinalProject/Analysis/races.csv")
quali<-fread("C:/Users/Ramesh Kini/Desktop/SEM_5/Data_Analytics/FinalProject/Analysis/qualifying.csv")
results<-fread("C:/Users/Ramesh Kini/Desktop/SEM_5/Data_Analytics/FinalProject/Analysis/results.csv")
constructors<-fread("C:/Users/Ramesh Kini/Desktop/SEM_5/Data_Analytics/FinalProject/Analysis/constructors.csv")


## set up mergedId default to be a copy of constructorId
constructors[,mergedId:=constructorId] 
constructors[constructorId==10,mergedId:=211] ## Force India/Racing Point
constructors[constructorId==15,mergedId:=51] ## Sauber/Alfa Romeo

##Adding details about the Hungarian Grand Prix
hungarian<-results[raceId==1042,.(raceId=1043,resultId=.I+100000,driverId,constructorId,number,grid=0,position=0,positionText="",positionOrder=0)]
hungarian[driverId==1,`:=`(grid=1,position=1,positionText="1",positionOrder=1)] ## hamilton
hungarian[driverId==822,`:=`(grid=2,position=3,positionText="3",positionOrder=3)] ## bottas
hungarian[driverId==830,`:=`(grid=7,position=2,positionText="2",positionOrder=2)] ## max_verstappen
hungarian[driverId==848,`:=`(grid=13,position=5,positionText="5",positionOrder=5)] ## albon
hungarian[driverId==846,`:=`(grid=8,position=13,positionText="13",positionOrder=13)] ## norris
hungarian[driverId==815,`:=`(grid=4,position=7,positionText="7",positionOrder=7)] ## perez
hungarian[driverId==840,`:=`(grid=3,position=4,positionText="4",positionOrder=4)] ## stroll
hungarian[driverId==817,`:=`(grid=11,position=8,positionText="8",positionOrder=8)] ## ricciardo
hungarian[driverId==832,`:=`(grid=9,position=9,positionText="9",positionOrder=9)] ## sainz
hungarian[driverId==826,`:=`(grid=17,position=12,positionText="12",positionOrder=12)] ## kvyat
hungarian[driverId==8,`:=`(grid=20,position=15,positionText="15",positionOrder=15)] ## raikkonen
hungarian[driverId==825,`:=`(grid=16,position=10,positionText="10",positionOrder=10)] ## kevin_magnussen
hungarian[driverId==154,`:=`(grid=18,position=16,positionText="16",positionOrder=16)] ## grosjean
hungarian[driverId==841,`:=`(grid=19,position=17,positionText="17",positionOrder=17)] ## giovinazzi
hungarian[driverId==842,`:=`(grid=10,position=20,positionText="R",positionOrder=20)] ## gasly
hungarian[driverId==847,`:=`(grid=12,position=18,positionText="18",positionOrder=18)] ## russell
hungarian[driverId==849,`:=`(grid=15,position=19,positionText="19",positionOrder=19)] ## latifi
hungarian[driverId==839,`:=`(grid=14,position=14,positionText="14",positionOrder=14)] ## ocon
hungarian[driverId==844,`:=`(grid=6,position=11,positionText="11",positionOrder=11)] ## leclerc
hungarian[driverId==20,`:=`(grid=5,position=6,positionText="6",positionOrder=6)] ## vettel

##Adding details about the British Grand Prix
##The added data is according to the official Wikipedia page
british<-results[raceId==1042,.(raceId=1044,resultId=.I+100050,driverId,constructorId,number,grid=0,position=NA,positionText="",positionOrder=NA)]
british[driverId==1,`:=`(grid=1)] ## hamilton
british[driverId==822,`:=`(grid=2)] ## bottas
british[driverId==830,`:=`(grid=3)] ## max_verstappen
british[driverId==848,`:=`(grid=12)] ## albon
british[driverId==846,`:=`(grid=5)] ## norris
british[driverId==815,`:=`(driverId=807,number=27,grid=13)] ## nico hulkenberg drove for Perez this race
british[driverId==840,`:=`(grid=6)] ## stroll
british[driverId==817,`:=`(grid=8)] ## ricciardo
british[driverId==832,`:=`(grid=7)] ## sainz
british[driverId==826,`:=`(grid=19)] ## kvyat ## 14 + 5-place penalty
british[driverId==8,`:=`(grid=16)] ## raikkonen
british[driverId==825,`:=`(grid=14)] ## kevin_magnussen
british[driverId==154,`:=`(grid=17)] ## grosjean
british[driverId==841,`:=`(grid=15)] ## giovinazzi
british[driverId==842,`:=`(grid=11)] ## gasly
british[driverId==847,`:=`(grid=20)] ## russell ## 15 + 5-place penalty
british[driverId==849,`:=`(grid=18)] ## latifi
british[driverId==839,`:=`(grid=9)] ## ocon
british[driverId==844,`:=`(grid=4)] ## leclerc
british[driverId==20,`:=`(grid=10)] ## vettel

quali_british<-quali[raceId==1042,.(qualifyId=.I+100050,raceId=1044,driverId,constructorId,number,q1="",q2="",q3="")]
quali_british[driverId==815,`:=`(driverId=807,number=27)] ##accidental same numnering in the datatset


##Merging these results together, for finals and qualifying respectively
results<-rbind(results,hungarian,british,fill = TRUE)
quali<-rbind(quali,quali_hungary,quali_british,fill = TRUE)

##Merging the basic datasets together, to make it more coherent and easier to use
qr<-merge(
  results[,.(raceId,driverId,constructorId,grid,pos=positionOrder,positionText)]
  ,quali[,.(raceId,driverId,qPos=position)]
  ,c("raceId","driverId"),all.x=TRUE,all.y=TRUE)
qr<-merge(qr,drivers[,.(driverId,driverRef)],"driverId",all.x = TRUE)
qr<-merge(qr,races[,.(raceId,year,date,race_name=gsub(" Grand Prix","",name))]
          ,"raceId",all.x = TRUE)[order(-date,-year,raceId,pos)]

##We create a custom rankings formula so as to include 20 top rankings, rather than the default 10 awarded. 
##The points awarded follow a squares system. For example, 1st = (20-1)^2 = 361; 10th = 100; 20th = 0 (22nd also 0)
qr[,predPts:=(20-pmin(20,pos))^2]
qr<-merge(qr,constructors[,.(constructorId,mergedId,teamRef=constructorRef)],"constructorId")
qr[raceId %in% qr[,.(minQ=min(qPos)),raceId][minQ==99,raceId],qPos:=grid]

##Rolling calculations.
##If a driver arrives first in one race, and does not arrive in the top 20 in the next, 
##all the drivers are moved up a notch, by the same amount. Hence the person who came second last time would come first in the collective race and so on.
##We need to calculate lag first to avoid leakage, and also override to get first (N-1) positions (frollmean is fast rolling mean)
qr[order(driverRef,raceId),`:=`(
  roll2D=round(frollmean(lagPredPts,2,fill = NA,align = "right"))
  ,roll3D=round(frollmean(lagPredPts,3,fill = NA,align = "right"))
  ,roll4D=round(frollmean(lagPredPts,4,fill = NA,align = "right"))
  ,roll5D=round(frollmean(lagPredPts,5,fill = NA,align = "right"))
  ,roll6D=round(frollmean(lagPredPts,6,fill = NA,align = "right"))
  ,roll7D=round(frollmean(lagPredPts,7,fill = NA,align = "right"))
  ,roll8D=round(frollmean(lagPredPts,8,fill = NA,align = "right"))
  ,roll9D=round(frollmean(lagPredPts,9,fill = NA,align = "right"))
  ,roll10D=round(frollmean(lagPredPts,10,fill = NA,align = "right"))
),driverRef]

##We now cascade missings
qr[is.na(lagPredPts),lagPredPts:=0]
qr[is.na(roll2D),roll2D:=lagPredPts]
qr[is.na(roll3D),roll3D:=roll2D]
qr[is.na(roll4D),roll4D:=roll3D]
qr[is.na(roll5D),roll5D:=roll4D]
qr[is.na(roll6D),roll6D:=roll5D]
qr[is.na(roll7D),roll7D:=roll6D]
qr[is.na(roll8D),roll8D:=roll7D]
qr[is.na(roll9D),roll9D:=roll8D]
qr[is.na(roll10D),roll10D:=roll9D]

##Calculate the team lag prediction points
teamPoints<-qr[,.(teamLagPredPoints=mean(lagPredPts)),.(mergedId,raceId)]
teamPoints[order(mergedId,raceId),`:=`(
  roll5T=round(frollmean(teamLagPredPoints,5,fill = NA,align = "right"))
  ,roll10T=round(frollmean(teamLagPredPoints,10,fill = NA,align = "right"))
),mergedId]
## add results back in
qr<-merge(qr,teamPoints[,.(mergedId,raceId,roll5T,roll10T)]
          ,c("mergedId","raceId"),all.x=TRUE)
qr[,`:=`(
  rkRoll5D=frank(-roll5D)
  ,rkRoll10D=frank(-roll10D)
  ,rkRoll5T=frank(-roll5T)  
  ,rkRoll10T=frank(-roll10T)
),.(raceId)]

##Blending all thecurrent data we have, namely the last 5 drivers, last 10 drivers, last 5 teams, last 10 teams, 50/50 qualifying
qr[,combined:=round((rkRoll5D+rkRoll10D+rkRoll5T+rkRoll10T+0.5*qPos+0.5*pmin(20,grid))/5,1)]


## Now we look at the past two year's Britis Grand Prix's to look at the various ranking measuers. 
qr[race_name=="British" & year==2018
   ,.(driverRef,teamRef,race_name,year,rk5D=round(rkRoll5D),rk10D=round(rkRoll10D)
      ,rk5T=floor(rkRoll5T),rk10T=floor(rkRoll10T),qPos,grid
      ,combined,pPreQual,pos
   )][order(combined)]

qr[race_name=="British" & year==2019
   ,.(driverRef,teamRef,race_name,year,rk5D=round(rkRoll5D),rk10D=round(rkRoll10D)
      ,rk5T=floor(rkRoll5T),rk10T=floor(rkRoll10T),qPos,grid
      ,combined,pPreQual,pos
   )][order(combined)]


##We can evaluate how well our predictions do compared to the other calculated methods (mae computes the average absolute difference between two numeric vectors)
expected<-qr[year>=2017 & !is.na(pos),.(
  maeComb=round(mae(pos,combined),2)
  ,maePreQual=round(mae(pos,pPreQual),2)
  ,maeGrid=round(mae(pos,pmin(20,grid)),2)
  ,maeQ=round(mae(pos,qPos),2)
  ,mae5d=round(mae(pos,rkRoll5D),2)
  ,mae10d=round(mae(pos,rkRoll10D),2)
  ,mae5t=round(mae(pos,rkRoll5T),2)
  ,mae10t=round(mae(pos,rkRoll10T),2)
),.(
  race_name,year)][order(-maeComb)]


##Since this doesn't actually predict the future yet we shall predict it for the British GP as of now.
preds_british<-qr[race_name=="British" & year==2020
                  ,.(driverRef,teamRef,race_name,year
                     ,rk5D=round(rkRoll5D),rk10D=round(rkRoll10D)
                     ,rk5T=floor(rkRoll5T),rk10T=floor(rkRoll10T)
                     ,rkPredict=frank(combined+0.001*grid) ## grid as tie breaker
                     ,rkPreQual=frank(pPreQual+0.001*rkRoll5D)  ## rk5D as tie breaker 
                     ,qPos
                     ,grid
                     ,predict_preQual=pPreQual
                     ,prediction=combined
                     ,finish=pos
                  )][order(rkPredict)]

preds_british

