## 
## Everglades TP Rule 
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
# Data Wrangling
library(AnalystHelper);
library(plyr);
library(reshape)

library(zoo)
# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)

# Data Vis
library(flextable)
library(magrittr)

# Paths
wd="C:/Julian_LaCie/_Github/EvergladesTPRule"
paths=paste0(wd,c("/Exports","/Plots","/Data","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]

GIS.path="C:/Julian_LaCie/_GISData"

# Helper variables
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# -------------------------------------------------------------------------
# Update for updated analysis
CurWY=2020

# -------------------------------------------------------------------------
## GIS Data 
# personal geodatabase 
db.path=paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb")
ogrListLayers(db.path)

evpa=spTransform(readOGR(db.path,"EPA_Boundary"),utm17)
rs.feat=spTransform(readOGR(db.path,"RestorationStrategies_Features"),utm17)
sta=spTransform(readOGR(db.path,"EvergladesSTAs"),utm17)
bcnp=spTransform(readOGR(db.path,"BCNP"),utm17)
canal=spTransform(readOGR(db.path,"SFWMD_Canals"),utm17)
shore=spTransform(readOGR(db.path,"SFWMD_Shoreline"),utm17)
eaa=spTransform(readOGR(db.path,"EvergladesAgriculturalArea"),utm17)
wma=spTransform(readOGR(db.path,"HoleyLand_Rotenberger"),utm17)
wca=spTransform(readOGR(db.path,"WCAs"),utm17)
enp.shore=spTransform(readOGR(db.path,"Shoreline_ENPClip"),utm17)
c139=spTransform(readOGR(db.path,"C139Annex"),utm17)
enp=spTransform(readOGR(db.path,"ENP"),utm17)

# https://geo-sfwmd.hub.arcgis.com/datasets/environmental-monitoring-stations
wmd.mon=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),utm17)
wmd.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")

# stations=read.csv(paste0(data.path,"/Station_list.csv"))
# test=ddply(stations,c("Station.ID","Alias","Area"),summarise,N.val=N.obs(Class))
# test[,1:3]<-lapply(test[,1:3],as.character)

station.list=data.frame(Station.ID=c("2AC2", "2AN1", "3ASMESO", "404C2", "404Z1", "CA2-17", "CA2-6", 
                        "CA2-9", "CA217", "CA222", "CA223", "CA224", "CA26", "CA29", 
                        "CA3-11", "CA3-14", "CA3-15", "CA3-16", "CA3-19", "CA3-2", "CA3-4", 
                        "CA3-5", "CA3-6", "CA3-8", "CA3-9", "CA311", "CA314", "CA315", 
                        "CA316", "CA319", "CA32", "CA324", "CA325", "CA33", "CA34", "CA35", 
                        "CA36", "CA38", "CA39", "CA3B1", "CA3B2", "CR2", "E5", "EP", 
                        "F1", "F3", "F4", "F5", "G-3273", "LOX10", "LOX11", "LOX12", 
                        "LOX13", "LOX14", "LOX15", "LOX16", "LOX3", "LOX4", "LOX5", "LOX6", 
                        "LOX7", "LOX8", "LOX9", "LOXA101", "LOXA105", "LOXA108", "LOXA124", 
                        "LOXA130", "LOXA137", "LOXA140", "LOXAZ1", "N-217", "N1", "NE1", 
                        "NP201", "P33", "P34", "P37", "RG1", "S345B6", "SRS1B", "SRS1C", 
                        "SRS2", "TSB", "U1", "U3", "WCA2E5", "WCA2F1", "WCA2F3", "WCA2F4", 
                        "WCA2F5", "WCA2U1", "WCA2U3", "X1", "X4", "Z1"),
           Alias=c("404C2", "2AN1", "3ASMESO", "404C2", "404Z1", "CA217", "CA26", 
                   "CA29", "CA217", "CA222", "CA223", "CA224", "CA26", "CA29", "CA311", 
                   "CA314", "CA315", "CA316", "CA319", "CA32", "CA34", "CA35", "CA36", 
                   "CA38", "CA39", "CA311", "CA314", "CA315", "CA316", "CA319", 
                   "CA32", "CA324", "CA325", "CA33", "CA34", "CA35", "CA36", "CA38", 
                   "CA39", "CA3B1", "CA3B2", "CR2", "E5", "EP", "WCA2F1", "WCA2F3", 
                   "WCA2F4", "WCA2F5", "G-3273", "LOX10", "LOX11", "LOX12", "LOX13", 
                   "LOX14", "LOX15", "LOX16", "LOX3", "LOX4", "LOX5", "LOX6", "LOX7", 
                   "LOX8", "LOX9", "LOXA101", "LOXA105", "LOXA108", "LOXA124", "LOXA130", 
                   "LOXA137", "LOXA140", "Z1", "CA217", "2AN1", "NE1", "NP201", 
                   "P33", "P34", "P37", "RG1", "S345B6", "SRS1B", "SRS1C", "SRS2", 
                   "TSB", "U1", "U3", "E5", "WCA2F1", "WCA2F3", "WCA2F4", "WCA2F5", 
                   "U1", "U3", "X1", "X4", "Z1"),
           Area=c("WCA2", "WCA2", "WCA3", "WCA2", "WCA2", "WCA2", "WCA2", "WCA2", 
                  "WCA2", "WCA2", "WCA2", "WCA2", "WCA2", "WCA2", "WCA3", "WCA3", 
                  "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", 
                  "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", 
                  "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", "WCA3", 
                  "WCA3", "ENP", "WCA2", "ENP", "WCA2", "WCA2", "WCA2", "WCA2", 
                  "ENP", "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", 
                  "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", 
                  "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", "LNWR", 
                  "LNWR", "LNWR", "WCA2", "WCA2", "ENP", "ENP", "ENP", "ENP", 
                  "ENP", "ENP", "WCA3", "ENP", "ENP", "ENP", "ENP", "WCA2", "WCA2", 
                  "WCA2", "WCA2", "WCA2", "WCA2", "WCA2", "WCA2", "WCA2", "LNWR", 
                  "LNWR", "LNWR"))

impact.sites.2005=c(paste0("LOXA",c(101,105,124,130,137,140)),"X1","Z1","2AN1","404Z1",paste0("WCA2",c("F1","F3","F4")),paste0("CA",c(223,224,314,324,33,35,36)))
station.list$status_2005=with(station.list,ifelse(Alias%in%impact.sites.2005,"Impacted","Unimpacted"))

TPRule.sites=subset(wmd.mon,STATION%in%station.list$Station.ID)
# library(tmap)
# tmap_mode("view")
# tm_shape(TPRule.sites)+tm_dots()


# Water Quality Data ------------------------------------------------------
dates=date.fun(c("2004-05-01",paste0(CurWY,"-04-30")))
parameters=data.frame(Test.Number=c(23,25),param=c("OPO4","TP"))

pb=txtProgressBar(min=1,max=nrow(station.list),style=3)
wq.dat=data.frame()
for(i in 1:nrow(station.list)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],station.list$Station.ID[i],parameters$Test.Number)
  wq.dat=rbind(wq.dat,tmp)
  setTxtProgressBar(pb,i)
}

wq.dat=merge(wq.dat,station.list,"Station.ID")
wq.dat=merge(wq.dat,parameters,"Test.Number")
wq.dat$TPWeek=biweek.period(wq.dat$Date.EST)
wq.dat$WY=WY(wq.dat$Date.EST)

raw.dat=subset(wq.dat,Test.Number==25&WY%in%seq(CurWY-4,CurWY,1))[,c("Alias","Area","Date.EST","WY","TPWeek","Test.Name","Units","HalfMDL")]

P.dat.xtab=cast(wq.dat,Alias+Area+Date.EST+WY+TPWeek~param,value="HalfMDL",mean)
P.dat.xtab$OPFlag=with(P.dat.xtab,ifelse(is.na(OPO4)|OPO4==0,1,0));
P.dat.xtab$TPFlag=with(P.dat.xtab,ifelse(is.na(TP),1,0));
P.dat.xtab$Reversal=with(P.dat.xtab,ifelse(P.dat.xtab$OPFlag==1,0,ifelse(OPO4>(TP*1.3),1,0)));
plot(TP~OPO4,P.dat.xtab,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(Reversal==1,"red",NA),col=adjustcolor("grey",0.8),log="xy");abline(0,1,col="red")

P.dat.xtab$TP=with(P.dat.xtab,ifelse(Reversal==1,NA,TP));# Removes Reversal 
P.dat.xtab$TP_ugL=P.dat.xtab$TP*1000
P.dat.xtab$HydroSeason=FL.Hydroseason(P.dat.xtab$Date.EST)

## Biweekly mean TP data + Data Screening
TPRule.dat=ddply(P.dat.xtab,
                 c("Area","Alias","WY","TPWeek","HydroSeason"),summarise,
                 AMean=mean(TP_ugL),
                 N=N.obs(TP_ugL));
TPRule.dat.screen=ddply(TPRule.dat,c("Alias","WY","HydroSeason"),summarise,N=N.obs(AMean));
TPRule.dat.screen=cast(TPRule.dat.screen,Alias+WY~HydroSeason,value="N",sum);

TPRule.dat.screen$NTotal=with(TPRule.dat.screen,A_Wet+B_Dry);
TPRule.dat.screen$SeasonScreen=with(TPRule.dat.screen,ifelse(B_Dry>=1&A_Wet>=1,1,0))
TPRule.dat.screen$NScreen=with(TPRule.dat.screen,ifelse(NTotal>=6,1,0));
TPRule.dat.screen$UseData=with(TPRule.dat.screen,ifelse((SeasonScreen+NScreen)==2,"Yes","No"));

TPRule.dat=merge(TPRule.dat,TPRule.dat.screen[,c("Alias","WY","UseData")],by=c("Alias","WY"),all.x=T)
range(TPRule.dat$WY)

# Impacted Station Assessment ---------------------------------------------
# Annual Geometric Mean
TPRule.AGM.ImpactAss=ddply(subset(TPRule.dat,Alias%in%impact.sites.2005),c("Area","Alias","WY","UseData"),summarise,
                           N=N.obs(AMean),Geomean=exp(mean(log(AMean),na.rm=T)))
TPRule.AGM.ImpactAss$Geomean.Use=with(TPRule.AGM.ImpactAss,ifelse(UseData=="Yes",Geomean,NA));

TPRule.AGM.ImpactAss$AGMStatus=with(TPRule.AGM.ImpactAss,ifelse(Geomean.Use<=15,"Pass","Fail"))
TPRule.AGM.ImpactAss$Geomean.Use.rnd=round(TPRule.AGM.ImpactAss$Geomean.Use,0)
TPRule.AGM.ImpactAss=TPRule.AGM.ImpactAss[order(TPRule.AGM.ImpactAss$Area,TPRule.AGM.ImpactAss$Alias),]

# Long Term Geometric Mean 
WYs=seq(WY(dates[1]),WY(dates[2]),1)
fill.df=data.frame(Alias=sort(rep(impact.sites.2005,length(WYs))),WY=rep(WYs,length(impact.sites.2005)))
fill.df=merge(fill.df,station.list[,c("Station.ID","Area")],by.x="Alias",by.y="Station.ID",all.x=T)

TPRule.dat.LTScreen=merge(TPRule.AGM.ImpactAss,fill.df,c("Alias","WY","Area"),all.y=T)
TPRule.dat.LTScreen$LTMean=with(TPRule.dat.LTScreen,ave(Geomean.Use,Alias,FUN=function(x)c(rep(NA,4),rollmean(x,5,na.rm=T))))
TPRule.dat.LTScreen$LT.N=with(TPRule.dat.LTScreen,ave(Geomean.Use,Alias,FUN=function(x)c(rep(NA,4),rollsum(is.na(x)==0,5,na.rm=T))))
TPRule.dat.LTScreen$LT.UseData=with(TPRule.dat.LTScreen,ifelse(LT.N<5,"No","Yes"))
TPRule.dat.LTScreen$LTperiod=with(TPRule.dat.LTScreen,ave(WY,Alias,FUN=function(x)c(rep(NA,4),paste0("WY",rollapply(x,5,min),"-",rollapply(x,5,max)))))
TPRule.dat.LTScreen$LTStatus=with(TPRule.dat.LTScreen,ifelse(LT.UseData=="Yes"&round(LTMean)<=10,"Pass","Fail"))
TPRule.dat.LTScreen$LTGM.rnd=with(TPRule.dat.LTScreen,ifelse(LT.UseData=="No",NA,round(LTMean)))

TPRule.dat.LTScreen$Status=with(TPRule.dat.LTScreen,ifelse(is.na(AGMStatus)|is.na(LTStatus),"Impacted",ifelse(AGMStatus=="Pass"&LTStatus=="Pass","Unimpacted","Impacted")))



head(TPRule.dat.LTScreen)
subset(TPRule.dat.LTScreen,Alias=="LOXA101")
subset(TPRule.dat.LTScreen,Alias=="LOXA130")

## Annual summary table
imp.trans=unique(subset(TPRule.dat.LTScreen,LT.UseData=="Yes"&LTGM.rnd<=10)$Alias)

WYs=seq(2005,CurWY,1)
TPRule.AGM.ImpactAss.xtab=cast(subset(TPRule.AGM.ImpactAss,WY%in%WYs),Area+Alias~WY,value="Geomean.Use.rnd",mean)

site.order=c(paste0("LOXA",c(101,105,124,130,137,140)),c("X1","Z1"),c("2AN1","404Z1"),paste0("CA",c("223","224")),paste0("WCA2",c("F1","F3","F4")),paste0("CA",c("314",324,33,35,36)))
TPRule.AGM.ImpactAss.xtab=TPRule.AGM.ImpactAss.xtab[match(site.order,TPRule.AGM.ImpactAss.xtab$Alias),]
cols=TPRule.AGM.ImpactAss.xtab[,as.character(WYs)]<=15

Ann.TP.ImpAssess=TPRule.AGM.ImpactAss.xtab[,c("Area","Alias",as.character(WYs))]%>%
  flextable()%>%
  colformat_num(j=3:ncol(TPRule.AGM.ImpactAss.xtab),na_str="---",digits=0)%>%
  font(fontname="Arial",part="all")%>%
  fontsize(size=8,part="body")%>%
  hline_top(border = officer::fp_border(width = 1.25))%>%
  hline_bottom(border = officer::fp_border(width = 1.25))%>%
  hline(border = officer::fp_border(width = 1.25), part = "all")%>%
  fontsize(size=10)%>%
  align(j=3:ncol(TPRule.AGM.ImpactAss.xtab),align="center",part="all")%>%
  bg(j=3:ncol(TPRule.AGM.ImpactAss.xtab),bg=ifelse(cols==T,rgb(198/255,224/255,180/255),NA))%>%
  merge_v(j=c("Area"))%>%
  valign(j="Area",valign="top")%>%
  set_header_labels(Alias="Station")%>%
  footnote(i=1,j=1:2,part="header",value=as_paragraph(" Blank cells (-) indicate insufficient data to calculate annual geometric mean due to TP Rule data screening or samples not being collected due to low water levels. Cells highlighted in green indicate that the annual geometric mean TP concentration for that station and year is ≤ 15 \u00B5g/L."),
           ref_symbols = c(" a"))%>%
  footnote(i=~Alias%in%imp.trans,j=~Alias,value=as_paragraph(paste0(" Stations ",knitr::combine_words(imp.trans[order(match(imp.trans,site.order))])," transitioned from impacted to unimpacted.")),
           ref_symbols=c(" b"))%>%
  width(width=c(0.6,0.9,rep(0.5,length(WYs))))
Ann.TP.ImpAssess

# Long Term Table
periods.WYs=paste0("WY",seq(2005,CurWY-4,1),"-",seq(2005,CurWY-4,1)+4) 

TPRule.dat.LTScreen.xtab=cast(subset(TPRule.dat.LTScreen,LTperiod%in%periods.WYs),Area+Alias~LTperiod,value="LTGM.rnd",mean)
TPRule.dat.LTScreen.xtab=TPRule.dat.LTScreen.xtab[match(site.order,TPRule.dat.LTScreen.xtab$Alias),]
cols=TPRule.dat.LTScreen.xtab[,as.character(periods.WYs)]<=10

LT.TP.ImpAssess=TPRule.dat.LTScreen.xtab[,c("Area","Alias",as.character(periods.WYs))]%>%
  flextable()%>%
  colformat_num(j=3:ncol(TPRule.dat.LTScreen.xtab),na_str="---",digits=0)%>%
  theme_vanilla()%>%
  padding(padding.bottom = 0.4, padding.top = 0.4, part = "all")%>%
  fontsize(size=8,part="body")%>%
  font(fontname="Arial",part="all")%>%
  hline_top(border = officer::fp_border(width = 1.25))%>%
  hline_bottom(border = officer::fp_border(width = 1.25))%>%
  hline(border = officer::fp_border(width = 1.25), part = "all")%>%
  fontsize(size=10)%>%
  align(j=1:2,align="left",part="body")%>%
  align(j=1:2,align="center",part="header")%>%
  align(j=3:ncol(TPRule.dat.LTScreen.xtab),align="center",part="all")%>%
  rotate(j=3:ncol(TPRule.dat.LTScreen.xtab),rotation="btlr",part="header")%>%
  height(height=1.2,part="header")%>%
  hrule(i=1,rule="exact",part="header")%>%
  bg(j=3:ncol(TPRule.dat.LTScreen.xtab),bg=ifelse(cols==T,rgb(198/255,224/255,180/255),NA),part="body")%>%
  merge_v(j=c("Area"))%>%valign(j="Area",valign="top")%>%
  set_header_labels(Alias="Station")%>%
  footnote(i=1,j=1:2,part="header",value=as_paragraph(" Blank cells (-) indicate insufficient data to calculate annual geometric mean due to TP Rule data screening or samples not being collected due to low water levels. Cells highlighted in green indicate that the annual geometric mean TP concentration for that station and year is ≤ 15 \u00B5g/L."),
           ref_symbols = c(" a"))%>%
  footnote(i=~Alias%in%imp.trans,j=~Alias,value=as_paragraph(paste0(" Stations ",knitr::combine_words(imp.trans[order(match(imp.trans,site.order))])," transitioned from impacted to unimpacted.")),
           ref_symbols=c(" b"))%>%
  width(width=c(0.6,0.9,rep(0.4,length(periods.WYs))))

LT.TP.ImpAssess


## TP Rule Evaluation
TPRule.dat.LTScreen[,c("Alias","WY","Status")]
head(TPRule.dat)

TPRule.dat=merge(TPRule.dat,TPRule.dat.LTScreen[,c("Alias","WY","Status")],c("Alias","WY"),all.x=T)
TPRule.dat$Status=with(TPRule.dat,ifelse(is.na(Status)==T,"Unimpacted",as.character(Status)))
TPRule.dat$Status=with(TPRule.dat,ifelse(Area=="ENP","N/A",as.character(Status)))
TPRule.dat$Current5Year=with(TPRule.dat,ifelse(WY%in%seq(CurWY-4,CurWY,1),"Yes","No"))

TPRule.AGM=ddply(TPRule.dat,c("Area","Alias","WY","Current5Year","Status","UseData"),summarise,
                 N=N.obs(AMean),Geomean=exp(mean(log(AMean),na.rm=T)));
TPRule.AGM$Geomean.Use=with(TPRule.AGM,ifelse(UseData=="Yes",Geomean,NA));
TPRule.AGM$AnnualStatus=with(TPRule.AGM,ifelse(UseData=="Yes",ifelse(Geomean<=15,"Pass","Fail"),"N/A"))

TPRule.Ann.Network.Avg=ddply(subset(TPRule.AGM,Current5Year=="Yes"),c("Area","Status","WY"),summarise,NetworkMean=round(mean(Geomean.Use,na.rm=T),0));
TPRule.Ann.Network.Avg$NetworkStatus=with(TPRule.Ann.Network.Avg,ifelse(NetworkMean<=11,"Pass","Fail"))
TPRule.CompAssess=merge(subset(TPRule.AGM,Current5Year=="Yes"),TPRule.Ann.Network.Avg,by=c("Area","WY","Status"),all.x=T);

TPRule.Ann.Network.Avg$LT10=with(TPRule.Ann.Network.Avg,ifelse(NetworkMean<10,0,1))
TPRule.Ann.Network.Avg$Network3_5=with(TPRule.Ann.Network.Avg,ave(LT10,paste(Area,Status),FUN=function(x) c(rep(NA,4),rollsum(x,5,na.rm=T))))
TPRule.Ann.Network.Avg$Network3_5Status=with(TPRule.Ann.Network.Avg,ifelse(is.na(Network3_5),NA,ifelse(Network3_5>3,"Fail","Pass")))
TPRule.Ann.Network.Avg=subset(TPRule.Ann.Network.Avg,WY==CurWY)
TPRule.CompAssess=merge(TPRule.CompAssess,TPRule.Ann.Network.Avg[,c("Area","Status","Network3_5Status")],c("Area","Status"),all.x=T)

TPRule.LT.Network.Avg=ddply(subset(TPRule.AGM,Current5Year=="Yes"),c("Area","Status"),summarise,LTNetworkMean=round(mean(Geomean.Use,na.rm=T),0));
TPRule.LT.Network.Avg$LTNetworkStatus=with(TPRule.LT.Network.Avg,ifelse(LTNetworkMean<=10,"Pass","Fail"))
TPRule.CompAssess=merge(TPRule.CompAssess,TPRule.LT.Network.Avg,by=c("Area","Status"),all.x=T)


## Final Table
TPRule.CompAssess$Geomean.Use2=with(TPRule.CompAssess,ifelse(UseData=="No",paste0("N/A (",round(Geomean,0),")"),round(Geomean,0)))

TPRule.vars=c("Area","Status","WY","Alias","N","Geomean.Use2","AnnualStatus","NetworkMean","NetworkStatus","LTNetworkMean","LTNetworkStatus","Network3_5Status")
TPRule.CompAssess.tbl=TPRule.CompAssess[,TPRule.vars]

TPRule.CompAssess.tbl=TPRule.CompAssess.tbl[order(match(TPRule.CompAssess.tbl$Area,c("LNWR","WCA2","WCA3","ENP")),TPRule.CompAssess.tbl$Status,TPRule.CompAssess.tbl$WY,TPRule.CompAssess.tbl$Alias),]

TPRule.table=TPRule.CompAssess.tbl%>%
  #dplyr::mutate_at(dplyr::vars(TPRule.vars[8:12]),~ifelse(is.na(.)==T,"N/A",.))%>%
  flextable()%>%
  theme_vanilla()%>%
  padding(padding.bottom = 0.4, padding.top = 0.4, part = "all")%>%
  fontsize(size=8,part="all")%>%#fontsize(size=9,part="header")%>%
  font(fontname="Arial",part="all")%>%
  align(j=1:4,align="left",part="body")%>%
  align(j=1:4,align="center",part="header")%>%
  align(j=5:12,align="center",part="all")%>%
  merge_v(j=c(1,2,3), target=c(8))%>%
  merge_v(j=c(1,2,3), target=c(9))%>%
  merge_v(j=c(1,2), target=c(10))%>%
  merge_v(j=c(1,2), target=c(11))%>%
  merge_v(j=c(1,2), target=c(12))%>%
  valign(j=8:12,valign="bottom",part="body")%>%
  color(i=~AnnualStatus=="Fail",j=~AnnualStatus,color="red")%>%
  bold(i=~AnnualStatus=="Fail",j=~AnnualStatus)%>%
  color(i=~NetworkStatus=="Fail",j=~NetworkStatus,color="red")%>%
  bold(i=~NetworkStatus=="Fail",j=~NetworkStatus)%>%
  color(i=~LTNetworkStatus=="Fail",j=~LTNetworkStatus,color="red")%>%
  bold(i=~LTNetworkStatus=="Fail",j=~LTNetworkStatus)%>%
  color(i=~Network3_5Status=="Fail",j=~Network3_5Status,color="red")%>%
  bold(i=~Network3_5Status=="Fail",j=~Network3_5Status)%>%
  bg(bg=rgb(222/255,234/255,246/255),part="header")%>%
  set_header_labels(Area2="Area",
                    Status="Criterion\nNetwork",
                    WY="Water\nYear",
                    Alias="Station",
                    N="Sample\nSize",
                    Geomean.Use2="Annual\nSite\nGeometric\nMean\n(\u03BCg/L)",
                    AnnualStatus="\u2264 15 \u03BCg/L\nPass/\nFail",
                    NetworkMean="Network\nAnnual\nAverage\nGeometric\nMean\n(\u03BCg/L)",
                    NetworkStatus="\u2264 11 \u03BCg/L\nPass/\nFail",
                    LTNetworkMean="Network\nFive-Year\nAverage\nGeometric\nMean\n(\u03BCg/L)",
                    LTNetworkStatus="Network\nFive-Year\nAverage\n\u2264 10 \u03BCg/L\nPass/Fail",
                    Network3_5Status="3 of 5\nYear\nNetwork\nAverage\n\u2264 10 \u03BCg/L")%>%
  footnote(part="header",value=as_paragraph("N/A - not applicable"),ref_symbols =c(" "))%>%
  width(j=1:12,width=c(0.5,0.9,0.55,0.75,0.55,0.75,0.5,0.75,0.5,0.7,0.7,0.7))
TPRule.table


# TP Rule Figures ---------------------------------------------------------
TPRule.AGM=TPRule.AGM[with(TPRule.AGM,order(Area,Status,substr(Alias,1,2))),]
TPRule.AGM$BarColor=with(TPRule.AGM,ifelse(Status=="Impacted","Red","Green"))
TPRule.AGM$Station.label=with(TPRule.AGM,ifelse(UseData=="Yes",paste(Alias),paste("*",Alias,sep="")))

axislwd=1
areas=c("LNWR","WCA2","WCA3","ENP")
# tiff(filename=paste(plot.path,"/WY",CurWY,"_TPRule.tif",sep=""),width=5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste(plot.path,"/WY",CurWY,"_TPRule.png",sep=""),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",cex.axis=1.5,mar=c(3,3,2,1),oma=c(0.25,2,0.1,1))#,mfrow=c(3,1))
layout(matrix(1:4,4,1,byrow=T),heights=c(1,1,1,0.5))
ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
#xlim.val=c(2005,CurWY);by.x=3;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

for(i in 1:3){
  x=barplot(TPRule.AGM[TPRule.AGM$WY==CurWY&TPRule.AGM$Area==areas[i],]$Geomean,space=c(0.25,0.25),xaxs="i",yaxs="i",ylim=ylim.val,yaxt="n",col=TPRule.AGM[TPRule.AGM$WY==CurWY&TPRule.AGM$Area==areas[i],]$BarColor)
  axis_fun(1,line=-0.25,x,x,TPRule.AGM[TPRule.AGM$Phase=="Current"&TPRule.AGM$Area==areas[i],]$Station.label,las=2)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  abline(h=15,lty="dashed",col="lightskyblue",lwd=3)
  mtext(paste(areas[i]),side=3,line=-1.25,cex=1)
  if(i==2){mtext("Geometric Mean Total Phosphorus (\u03BCg/L)",side=2,line=2.5,cex=1)}
}
mtext("Station",side=1,line=5,cex=1,outer=F);
par(mar=c(1,3,1,1))
plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend(0.5,0.5,legend=c("Impacted","Unimpacted","Annual Limit"),pch=c(22,22,NA),pt.bg=c("Red","Green",NA),col=c("Black","Black","lightskyblue"),lty=c(0,0,2),lwd=c(0.1,0.1,1.5),pt.cex=2,ncol=3,cex=1,bty="n",y.intersp=0.75,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()

## Current WY figure (ENP only)
# tiff(filename=paste(plot.path,"/WY",CurWY,"_TPRule_ENP.tif",sep=""),width=6.5,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste(plot.path,"/WY",CurWY,"_TPRule_ENP.png",sep=""),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,20);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,1),oma=c(1,2,1,1),xpd=F);
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.3));
x=barplot(TPRule.AGM[TPRule.AGM$WY==CurWY&TPRule.AGM$Area==areas[4],]$Geomean,space=c(0.25,0.25),xaxs="i",yaxs="i",ylim=ylim.val,yaxt="n",col=TPRule.AGM[TPRule.AGM$WY==CurWY&TPRule.AGM$Area==areas[4],]$BarColor)
axis_fun(1,x,x,TPRule.AGM[TPRule.AGM$WY==CurWY&TPRule.AGM$Area==areas[4],]$Station.label,las=2)
axis_fun(2,line=-0.1,ymaj,ymin,ymaj,1);box(lwd=axislwd)
abline(h=15,lty="dashed",col="lightskyblue",lwd=3)
mtext("Geometric Mean\nTotal Phosphorus (\u03BCg/L)",side=2,line=2,cex=1);
mtext("Station",side=1,line=3.5,cex=1,outer=F);
plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend(0.5,-0.75,legend=c("Impacted","Unimpacted","Annual Limit"),pch=c(22,22,NA),pt.bg=c("Red","Green",NA),col=c("Black","Black","lightskyblue"),lty=c(0,0,2),lwd=c(1,1,3),pt.cex=2,ncol=3,cex=1,bty="n",y.intersp=0.75,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()

##Network Figure 
TPRule.Ann.Net.Plot=cast(subset(TPRule.AGM,Area%in%c("LNWR","WCA2","WCA3")),Area+WY~Status,value="Geomean.Use",mean,na.rm=T)
y.max=c(30,50,40)
# tiff(filename=paste(plot.path,"/Network_TPRule_WY",CurWY,".tif",sep=""),width=5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste(plot.path,"/Network_TPRule_WY",CurWY,".png",sep=""),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.5,1),oma=c(1,2,1,1),xpd=F);
layout(matrix(1:4,4,1,byrow=T),heights=c(1,1,1,0.25));
xlab=seq(2005,CurWY,1)
for(i in 1:3){
  ylim.val=c(0,y.max[i]);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(t(TPRule.Ann.Net.Plot[which(TPRule.Ann.Net.Plot$Area==areas[i]),c("Impacted","Unimpacted")]),beside=T,col=c("Red","Green"),space=c(0,0.50),yaxs="i",ylim=ylim.val,yaxt="n",xpd=F)
  if(i==3){axis_fun(1,line=-0.5,t(t(x)[,1]+0.5),t(t(x)[,1]+0.5),xlab,1)}else{axis_fun(1,t(t(x)[,1]+0.5),t(t(x)[,1]+0.5),NA,1)}
  axis_fun(2,ymaj,ymin,ymaj,1);box(lwd=axislwd)
  abline(h=c(10,11),lty=c(1,2),lwd=2)
  mtext(paste(areas[i]),side=3,line=-1.5,cex=1)
  if(i==2){mtext("Average Geometric Mean Total Phosphorus (\u03BCg/L)",side=2,line=2.25,cex=1);}
  if(i==3){mtext("Florida Water Year",side=1,line=2.25,cex=1)}
}
plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend(0.5,0.5,legend=c("Impacted","Unimpacted","Long Term Limit","Network Limit"),pch=c(22,22,NA,NA),pt.bg=c("Red","Green",NA,NA),col=c("Black"),lty=c(0,0,1,2),lwd=c(0.5,0.5,1,1),pt.cex=2,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()

## Netowrk Average Geomean TP (ENP only)
TPRule.Ann.Net.Plot.ENP=ddply(subset(TPRule.AGM,Area=="ENP"),"WY",summarise,MeanGeomean=mean(Geomean.Use,na.rm=T))
subset(TPRule.AGM,Area=="ENP"&WY==CurWY)

# tiff(filename=paste(plot.path,"/Network_TPRule_WY",CurWY,"_ENP.tif",sep=""),width=6.5,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste(plot.path,"/Network_TPRule_WY",CurWY,"_ENP.png",sep=""),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.5,1),oma=c(1,2,1,1),xpd=F);
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25));

xlim.val=c(2005,CurWY);by.x=3;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y)
x=barplot(TPRule.Ann.Net.Plot.ENP$MeanGeomean,las=1,space=c(0.25,0.25),xaxs="i",yaxs="i",ylim=ylim.val,yaxt="n",col="green")
axis_fun(1,x[seq(1,length(x),by.x)],x[seq(1,length(x),by.x/by.x)],xmaj,1)
axis_fun(2,ymaj,ymin,ymaj,1);box(lwd=axislwd)
abline(h=c(10,11),lty=c(1,2),lwd=2)
mtext("Average Geometric Mean\nTotal Phosphorus (\u03BCg/L)",side=2,line=2,cex=1);
mtext("Florida Water Year",side=1,line=2.0,cex=1)
mtext("ENP",side=3,line=-1.5,cex=1.5)
plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend(0.5,0.5,legend=c("Unimpacted","Long Term Limit","Network Limit"),pch=c(22,NA,NA),pt.bg=c("Green",NA,NA),col=c("Black"),lty=c(0,1,2),lwd=c(1,3,3),pt.cex=2,ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)

dev.off()



# map ---------------------------------------------------------------------

curwy.site.list=subset(TPRule.AGM,WY==CurWY)[,c("Area","Alias","WY",'Status')]

curwy.site.list=curwy.site.list[order(match(curwy.site.list$Area,c("LNWR","WCA2","WCA3","ENP")),curwy.site.list$Status,curwy.site.list$Alias),c("Area","Alias","Status")]

curwy.site.list%>%
  flextable()%>%
  merge_v(j=c("Area"))%>%valign(j="Area",valign="top")%>%
  fontsize(size=10,part="all")%>%#fontsize(size=9,part="header")%>%
  font(fontname="Arial",part="all")%>%
  align(j=1,align="left",part="all")%>%
  align(j=2:3,align="center",part="all")%>%fix_border_issues()%>%
  color(i=~Status=="Impacted",j=~Status,color="red")%>%
  bold(i=~Status=="Impacted",j=~Status)%>%
  set_header_labels(Alias="Station")

TP.wmd.mon=merge(subset(wmd.mon,STATION%in%curwy.site.list$Alias),curwy.site.list,by.x="STATION",by.y="Alias")
TP.wmd.mon$Status=factor(TP.wmd.mon$Status,levels=c("Impacted","Unimpacted","N/A"))

col=c("indianred1","lawngreen","grey")
col.vals=col[TP.wmd.mon$Status]
pch.vals=c(22,21,21)[TP.wmd.mon$Status]
bbox=raster::extent(473714,587635,2748300,2960854)
cols=c(rgb(255/255,235/255,175/255,1,"esri.topaz.sand"),
       rgb(190/255,232/255,255/255,1,"esri.light.blue"),
       rgb(151/255,219/255,242/255,1,"esri.light.blue1"),
       rgb(247/255,255/255,231/255,1,"KM.BCNP.col"),
       rgb(109/255,187/255,67/255,1,"esri.green1"))
# tiff(filename=paste(plot.path,"/WY",CurWY,"_TPRuleMap.tiff",sep=""),width=5.5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste(plot.path,"/WY",CurWY,"_TPRuleMap.png",sep=""),width=5.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.1,0.1,0.1,0.1),mar=c(0.1,0.1,0.1,0.1))
layout(matrix(c(1,2),1,2,byrow=T),widths=c(1,0.5))

plot(shore,ylim=bbox[3:4],xlim=bbox[1:2],bg="lightblue",border="grey",col="cornsilk",lwd=0.5)
plot(eaa,col="olivedrab1",border="grey",add=T)
plot(c139,col="grey",border=NA,add=T)
plot(sta,col="skyblue",border="grey",lwd=0.1,add=T)
plot(rs.feat,col="steelblue3",border="grey",lwd=0.1,add=T)
plot(wma,col=cols[3],border=NA,add=T)
plot(wca,col="white",add=T)
plot(bcnp,col=cols[4],border=NA,add=T)
plot(enp.shore,col="white",border="dodgerblue3",add=T,lwd=0.5)
plot(canal,lwd=2,col="dodgerblue3",add=T)
plot(canal,lwd=1,col=cols[2],add=T)
plot(evpa,border="red",col=NA,lwd=1.5,add=T)
plot(TP.wmd.mon,pch=pch.vals,cex=1,bg=col.vals,add=T,lwd=0.1)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)
box(lwd=1)

plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
legend(0.5,0.9,
       legend=c("Impacted","Unimpacted","N/A"),
       pch=c(22,21,21),pt.bg=col,col="black",lty=0,lwd=0.1,
       pt.cex=1,ncol=1,cex=0.8,bty="n",xpd=NA,xjust=0.5,
       title.adj=0,title="Water Quality\nMonitoring Locations\nTP Rule Network")
dev.off()
