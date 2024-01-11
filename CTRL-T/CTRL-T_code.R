#####################################################################
#####################################################################
# ************************************************************** ####
#                 CTRL-T: AN TOOL FOR THE                   ####
#             AUTOMATIC CALCULATION OF RAINFALL                  #### 
#            THRESHOLDS FOR LANDSLIDE OCCURRENCE                 ####
#                                                                ####
#  NOME CODE: CTRL-A                                             ####
#  AUTHORS: MASSIMO MELILLO                                      ####   
#  LICENSE: this code is licensed under GPL(version 2 or later)  ####
#  CITATION: Melillo M, Brunetti MT, Peruccacci S,               ####
#  Gariano SL, Roccati A., Guzzetti F (2017).                    ####
# ************************************************************** ####
#                CTRL-a: CTRL-A: an algorithm                    ####
#             for the automatic calculation of                   ####
#         rainfall thresholds for landslide occurrence           ####
# ************************************************************** ####
#                 This script was prepared using                 ####
#        R Core Team (2017). R: A language and environment for   ####
#        statistical computing. R Foundation for Statistical     ####
#        Computing, Vienna,Austria.                              ####
#        URL http://www.R-project.org/                           ####
#        R version: R-3.3.3                                      ####
# ************************************************************** ####
#          The script requires the following libraries:          ####
# ************************************************************** ####
#          Package: utils                                        ####
#          Version: 2.6.0                                        ####
#          Priority: base                                        ####
#          Title: Various Programming Utilities                  ####
#          Author: Henrik Bengtsson [aut, cre, cph]              ####
#          Maintainer: Henrik Bengtsson <henrikb at braju.com>   ####
#          Depends: R (??? 2.14.0), R.oo (??? 1.21.0)            ####
#          Description: Utility functions useful                 ####
#          when programming and developing R packages            ####
#          License: LGPL-2.1 | LGPL-3 [exp.from: LGPL (??? 2.1)] ####
#          Built: R 3.3.3; x86_64-w64-mingw32; 2013-09-25        ####
#          15:38:52 UTC; windows                                 ####
# ************************************************************** ####
#          Package: caTools                                      ####
#          Version: 1.17.1                                       ####
#          Priority: base                                        ####
#          Title: Tools: moving window statistics, GIF, Base64,  #### 
#          ROC AUC, etc.                                         ####
#          Author: Jarek Tuszynski                               ####
#          <jaroslaw.w.tuszynski@saic.com>                       ####
#          Maintainer: Harutyun Khachatryan <kh_harut@yahoo.com> ####
#          Depends: R (>= 2.2.0)                                 ####
#          Description: Contains several basic utility functions ####
#          including: moving (rolling, running) window statistic ####
#          functions, read/write for GIF and ENVI binary files,  ####
#          fast calculation of AUC, LogitBoost                   ####
#          classifier, base64 encoder/decoder,                   ####
#          round-off-error-free sum and cumsum, etc              ####
#          License: GPL-3                                        ####
#          Built: R 3.3.2; x86_64-w64-mingw32;                   ####
#          2017-01-23 10:48:27 UTC; windows                      ####
# ************************************************************** ####
#          Package: ggmap                                        ####
#          Version:  	2.6.1                                      ####
#          Priority: base                                        ####
#          Title: Spatial Visualization with ggplot2             #### 
#          Author: David Kahle [aut, cre], Hadley Wickham [aut]  ####
#          Maintainer: David Kahle <david.kahle at gmail.com>    ####
#          Depends: R (??? 2.14.0), ggplot2 (??? 2.0.0)          ####
#          Description: A collection of functions to visualize   ####
#          spatial data and models on top of static maps from    ####
#          various online sources                                ####
#          License: GPL-2                                        ####
#          Built: R 3.3.2; x86_64-w64-mingw32;                   ####
#          2017-01-23 10:48:27 UTC; windows                      ####
# ************************************************************** ####
#          Package: MASS                                         ####
#          Version:  	7.3-47                                     ####
#          Priority: recommended                                 ####
#          Title: Support Functions and Datasets for             ####
#          Venables and Ripley's MASS                            #### 
#          Author: Brian Ripley [aut, cre, cph],                 ####
#          Bill Venables [ctb],                                  ####
#          Douglas M. Bates [ctb],                               ####
#          Kurt Hornik [trl] (partial port ca 1998),             ####
#          Albrecht Gebhardt [trl] (partial port ca 1998),       ####
#          David Firth [ctb]                                     ####
#          Maintainer: Brian Ripley <ripley@stats.ox.ac.uk>      ####
#          Depends: R (>= 3.1.0)                                 ####
#          Description: Functions and datasets to support        ####
#          Venables and Ripley,`Modern Applied Statistics        ####
#          with S''(4th edition, 2002)                           ####
#          License: GPL-2 | GPL-3                                ####
#          Built: R 3.3.2; x86_64-w64-mingw32;                   ####
#          2017-01-23 10:48:27 UTC; windows                      ####
# ************************************************************** ####
#          INPUTS: 1) table_of_raingauge.csv                     ####
#                  2) landslides.csv.                            ####
#                  3) TimeSeries_Sensor_XXX.csv                  ####
#                                                                ####
# ************************************************************** ####
#####################################################################
#####################################################################


library("utils")
  library("caTools")
  library ("ggmap")
library("MASS")

ppp<-c()
  lim<-c()
    target_1<-1
      dd1<-0
        ee1<-0
          i1<-15 
            ran<-0
              ava<-0
            avacd<-0
          avawd<-0
        mis<-0
      smin<-0
    tck.length<--0.01
  R <- 6372.795477598
rmax<-50 #maximum number of rank
      
buffer=16
  k_antecedent<-0.84
n_k<-0


green_code<-c(rgb(40/255,0,0,1),rgb(40/255,40/255,0,1),rgb(0,1,0,1))
  yellow_code<-c(rgb(40/255,0,0,1),rgb(1,1,0,1),rgb(0,40/255,0,1))
    red_code<-c(rgb(1,0,0,1),rgb(40/255,40/255,0,1),rgb(0,40/255,0,1))
      cd_color<-c("red","green","blue","yellow","magenta", "darkgreen",
            "white", "gray20", "dodgerblue", "indianred4", 
              "mediumspringgreen", "orchid", "seagreen2", "skyblue1", "purple3", 
                "tan2", "peachpuff1", "ivory", "lavender", "khaki1", "darkviolet","yellowgreen")
classe<-rev(c('#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac'))
  xld<-expression(italic(D)["l "](h))
  xle<-expression(italic(E)["l "](mm))
xli<-expression(italic(I)["L "](mm/h))

vet.lab.yy <- c(expression(10^-2),expression(10^-1),expression(10^0),expression(10^1),expression(10^2),expression(10^3))
  vet.lab.yx <- c(expression(10^0),expression(10^1),expression(10^2),expression(10^3),expression(10^4))
  vet.lab.x <- c(expression(10^-1),expression(10^0),expression(10^1),expression(10^2),expression(10^3))
vet.lab.y <- c(0.1,1,10,100,1000,5000)

result<-as.logical()
  start_year<-as.numeric()
    end_year<-as.numeric()
      vector.EH<-as.numeric     
        totRE<-data.frame()
          stat1<-data.frame()
          stat2<-data.frame()
        land_in_buffer<-data.frame()
      land_out_buffer<-data.frame()                             
    stat_land_in_buffer<-data.frame()
  stat.1<-data.frame()  
OUT.COMB<-data.frame()


#END setting of general variables

clearhistory <- function() {
  write("", file=".blank")
    loadhistory(".blank")
      unlink(".blank")
                            }
#CURRENT DIRECTORy *********************************************************
current_dir<-getwd()

#DIRECTORY INPUT************************************************************
dir_rain_gauges<-paste(current_dir,"CTRL-T/rainfall data/",sep="")

#DIRECTORY OUTPUT***********************************************************
  dir_file_name_1<-paste(current_dir,"/CTRL-T/OUTPUT",sep="")
    dir.create(dir_file_name_1)
      dir_file_name_2<-paste(current_dir,"/CTRL-T/Processed rainfall data",sep="")
        dir.create(dir_file_name_2)
          dir_file_name_3<-paste(dir_file_name_1,"/Rainfall thresholds",sep="")
            dir.create(dir_file_name_3)
            dir_file_name_4<-paste(dir_file_name_1,"/Reconstructed rainfall conditions",sep="")
          dir.create(dir_file_name_4)
        dir_file_name_5<-paste(dir_file_name_1,"/Reconstructed rainfall events",sep="")
      dir.create(dir_file_name_5)
    dir_file_name_4.1<-paste(dir_file_name_4,"/Individual files",sep="")
  dir.create(dir_file_name_4.1)

  #READ DATA ****************************************************************
file_input_raingauge<-paste(current_dir,"/CTRL-T/table_of_raingauges.csv",sep="")
  file_input_landslides<-paste(current_dir,"/CTRL-T/landslides.csv",sep="")
    anagrafica<- read.csv(file_input_raingauge, sep=";", header=T)
      l_ana<-length(anagrafica[,i1]) 
        landslide<- read.csv(file_input_landslides, sep=";",skip=0, header=T)
          l_lan<-length(landslide[,1])
            ls<-nchar(file_input_landslides)
          n<-(1:ls);ppp<-c(ppp,substring(as.character(file_input_landslides),n,n))
        lim<-max(which(ppp=="/"))
      nameoutput<-paste(substring(file_input_landslides,lim+1,ls-4),"_","B",buffer,sep="")
    dist_of_land<-array(NA,dim=c(l_lan,l_ana))
  lat_r<-as.numeric(as.character(anagrafica[,3]))
lon_r<-as.numeric(as.character(anagrafica[,2]))


# ****************************************************************BLOCK 1***********************************************************
# *********************************************UPDATING RAINFALL EVENT FOR EACH RAINGAUGE STATION***************************************

for( j in 1:l_ana) { #cycle to read TimeSeries_Sensor files 
clearhistory ()
cv<-0
cat("\f")
print(paste("Progress:",round((j*100/l_ana),0),"%"),quote=F)

#general variables***********
vector.date<-as.numeric()
  current_file<-paste(dir_rain_gauges,"TimeSeries_Sensor_",anagrafica[j,i1],".csv", sep = "")#riferimento a campo i1 (name) in anagrafica
    dir <- file.path(  current_file)
      result<-file_test("-f", dir)
if (result==TRUE )
  {
    n.h.s1<-as.numeric()
      n.h.s2<-as.numeric()
        n.h.s4<-as.numeric()
          RE<- array(NA, dim=c(10000,7)) 
            pluviometri <- read.csv(paste(current_file, sep = ""), sep="|", header=FALSE)
              index_raingauge<-j 
  
#function for choose parametrs of different period (warm season and cold season)*******************
p.C <- function(index.month,p.CC,p.CW) ifelse(index.month>=index.month.start.warm.season
                                        && index.month<=index.month.end.warm.season,nh<-p.CW,   nh<-p.CC)
#**************************************************************************************************                                                 && index.month<=index.month.end.warm.season,nh<-p.CW,   nh<-p.CC) 

vlp<-strptime(pluviometri[,1],format("%y-%m-%d %H:00:00+00" ),tz = "UTC")  #2003-01-01 01:00:00+00
  vlc<-as.character(pluviometri[,2])         
    dif<-as.numeric(as.character(diff(vlp)))/1
      z<-which(dif>1)
        index_dif_zero<-which(dif==0)
      y<-length(z)
    insert<-c()
  delta<-0
vlc[index_dif_zero]<-33333
    
    
    if (y>=1) {
      for( k in 1:y) 
      {
        inizio<-z[k]+delta
        fine<-inizio+dif[z[k]]
        insert.na<-rep("NA",times=dif[z[k]]-1)
        vlc<-append(vlc,insert.na,after=inizio)
        delta<-delta+dif[z[k]]-1
      }
    }
    
    if (length(index_dif_zero)>0) {
      vlc<-vlc[-which(vlc==33333)]#modifica 10/02/2015 elimina data/ora uguale per cambio ora in ottobre
    }
    
    vector.date<-(seq(vlp[2],by="hours",length.out=length(vlc)))
    vlc[vlc=="--" | vlc=="na" | vlc==9999]<-"na" 
    vlc[1]<-0
    vector.EH<-as.numeric(vlc)
    
    # Setting environment variable loaded with values from the rainfall file
    GS<-as.numeric(as.character(anagrafica[index_raingauge,5]))                           #default 0.2
    P1.cold<-as.numeric(as.character(anagrafica[index_raingauge,6]))                    #default 6
    P1.warm<-as.numeric(as.character(anagrafica[index_raingauge,7]))                  #default 3
    P2.cold<-as.numeric(as.character(anagrafica[index_raingauge,8]))                #default 12
    P2.warm<-as.numeric(as.character(anagrafica[index_raingauge,9]))              #default 6
    P3<-as.numeric(as.character(anagrafica[index_raingauge,10]))                #default 96
    P4.cold<-as.numeric(as.character(anagrafica[index_raingauge,11]))         #default 96
    P4.warm<-as.numeric(as.character(anagrafica[index_raingauge,12]))       #default 48
    index.month.start.warm.season<-as.numeric(as.character(anagrafica[index_raingauge,13]))
    index.month.end.warm.season<-as.numeric(as.character(anagrafica[index_raingauge,14]))
    
    
    # STEP 0 A rainfall record may contain hourly rainfall measurements EH that are lower than the instrumental
    # sensitivity of a rain gauge (e.g., GS = 0.2 mm), EH < GS. These hourly measurements are considered noise in the rainfall
    # record, and the algorithm sets the measurements to EH = 0.0 mm.
    vector.EH[vector.EH<GS]<-0
    # END STEP 0
    
    # STEP 1: Detection and exclusion of isolated rainfall measurements (IRM)
    nzv<-which(vector.EH>0) #nzv is the vector indices of (n)ot (z)ero (v)alues 
    l<-length(nzv)
    if (l!=0)   {
      for( k in 1:l) {n.h.s1[k]<-p.C(as.numeric(format(vector.date[nzv[k]],"%m")),P1.cold,P1.warm)} #result of P.C function for not null EH value in STEP1
      z<-((nzv[2:(l-1)]-nzv[1:(l-2)]>n.h.s1[2:(l-1)])  # dsx tra  n e n-1 ;se dist>P1(c)-->TRUE --> ci sono almeno P1(c) valori 0 a sx di n
          & (nzv[(3:l)]-nzv[2:(l-1)]>n.h.s1[2:(l-1)])      # ddx tra  n+1 e n ;se dist>P1(c)-->TRUE --> ci sono almeno P1(c) valori 0 a dx di n
          & (vector.EH[nzv[2:(l-1)]]==GS))                 # check of GS threshold
      vector.EH[nzv[which(z==TRUE)+1]]<-0             # replacement on intermediate values
      if ((nzv[2]-nzv[1]>n.h.s1[1]) & vector.EH[nzv[1]]==GS) vector.EH[nzv[1]]<-0     #check first value
      if ((nzv[l]-nzv[l-1]>n.h.s1[l]) & vector.EH[nzv[l]]==GS) vector.EH[nzv[l]]<-0  #check last  value
      # END STEP 1
      
      # STEP 2: Identification of rainfall sub-events
      nzv2<-which(vector.EH >0)
      l=length(nzv2)
      for( k in 1:l) {n.h.s2[k]<-p.C(as.numeric(format(vector.date[nzv2[k]],"%m")),P2.cold,P2.warm)}
      delta<-which((nzv2[2:l]-nzv2[1:l-1]-1)>=n.h.s2[1:(l-1)])
      ld<-length(delta)
      RSE<- matrix( nrow = ld+1, ncol = 4, byrow = TRUE)  
      if (ld!=0)   { 
        # matrix of (R)ainfall (S)ub (E)vents
        n<-(2:ld);RSE[n,1]<-nzv2[delta[(n-1)]+1];RSE[n,2]<-nzv2[delta[(n)]]   # intermediate sub-events:--> number of event: n to ld-1
        RSE[1,1]<-nzv2[1];RSE[1,2]<-nzv2[delta[(1)]]                            # fist sub-event------------> number of event: 1
        RSE[ld+1,1]<-nzv2[delta[ld]+1];RSE[ld+1,2]<-nzv2[l]                       # last sub-event-------------> number of event: ld
        for(i in 1:(ld+1)) {RSE[i,4]<-sum(vector.EH[RSE[i,1]:RSE[i,2]],na.rm = TRUE);RSE[i,3]<-RSE[i,2]-RSE[i,1]+1} #calculate Ds and Es
      }
      # END STEP 2
      
      # STEP 3: Exclusion of irrelevant rainfall sub-events
      Es.under.P3<-which(RSE[,4]<=P3)
      l.2<-length(Es.under.P3)
      if (l.2!=0)   {
        for( i in 1:length(Es.under.P3)) for( k in (RSE[Es.under.P3[i],1]:RSE[Es.under.P3[i],2])) vector.EH[k]<-0    
      }  
      # END STEP 3
      
      # Step 4: Identification of rainfall events
      nzv4<-which(vector.EH >0)
      l=length(nzv4)
      for( k in 1:l) {n.h.s4[k]<-p.C(as.numeric(format(vector.date[nzv4[k]],"%m")),P4.cold,P4.warm)}
      delta<-which((nzv4[2:l]-nzv4[1:l-1]-1)>=n.h.s4[1:(l-1)])
      ld<-length(delta)
      if (ld!=0) {
        n<-(2:ld);RE[n,1]<-nzv4[delta[(n-1)]+1];RE[n,2]<-nzv4[delta[(n)]]   # intermediate sub-events:--> number of event: n to ld-1
        RE[1,1]<-nzv4[1];RE[1,2]<-nzv4[delta[(1)]]                         # fist sub-event------------> number of event: 1
        RE[ld+1,1]<-nzv4[delta[ld]+1];RE[ld+1,2]<-nzv4[l]                   # last sub-event------------> number of event: ld
        
        for(i in 1:(ld+1)) {
          vector_prov<-vector.EH[RE[i,1]:RE[i,2]]
          RE[i,4]<-sum(vector_prov,na.rm = TRUE);
          RE[i,3]<-RE[i,2]-RE[i,1]+1;RE[i,5]<-index_raingauge;
          RE[i,6]<-max(vector_prov,na.rm = TRUE);
          if (length(vector_prov)<24) {
            RE[i,7]<-RE[i,4]
          } else {
            vector_prov[-which(vector_prov!="Na")]<-0
            RE[i,7]<-max(runmean(vector_prov, 24,endrule="NA")*24,na.rm=T)
          }                
        } 
        #calculate Ds and Es
      } else {
        RE[1,5]<-index_raingauge
        cv<-1
      }
      
      # END STEP 4
      
      
      #DATA STORAGE in RE.csv file
      mede<-as.numeric()
      output<-data.frame(ID_rain_gauge=as.character(anagrafica[RE[0:ld+1,5],i1]),index_pos1=as.character(RE[0:ld+1,1]),
                         index_pos2=as.character(RE[0:ld+1,2]),RE_start_date=vector.date[RE[0:ld+1,1]-1],
                         RE_end_date=vector.date[RE[0:ld+1,2]-1],D_E= as.character(RE[0:ld+1,3]), 
                         E_E=as.character(RE[0:ld+1,4]),I_E=as.character(round((RE[0:ld+1,4]/RE[0:ld+1,3]),3)),
                         IP_E=as.character(RE[0:ld+1,6]),Emax24_E=as.character(RE[0:ld+1,7])                       )
      
output$A_class <- cut(as.numeric(as.character(output$Emax24_E)), breaks=c(0,4,16,32,64,128,1000),dig.lab=4,include.lowest = T,labels = c(1:6))
  sy<-as.numeric(substring(vector.date[RE[1,1]-1],1,4))
    ey<-as.numeric(substring(vector.date[RE[ld+1,2]-1],1,4))
      start_year<-c(start_year,sy)
        end_year<-c(end_year,ey)
          n_re<-ld+1-cv
            delta<-round(as.numeric((vector.date[RE[ld+1,2]-1]-vector.date[RE[1,1]-1])/365),2)
              av_a<-round(n_re/delta,1)
                vo_Dh<-as.numeric(as.character(output$D_E))
                vo_Emm<-as.numeric(as.character(output$E_E))
              mind<-min(vo_Dh)
            maxd<-max(vo_Dh)
          medd<-round(median(vo_Dh),0)
        meand<-round(mean(vo_Dh),0)
      mine<-min(vo_Emm)
    maxe<-max(vo_Emm)
  mede<-round(median(vo_Emm),1)
meane<-round(mean(vo_Emm),1)

stat1<-rbind(stat1,data.frame(id_R=as.character(anagrafica[RE[1,5],i1]),caa=as.character(anagrafica[RE[1,5],1]),
        lon=as.character(anagrafica[RE[1,5],2]),lat=as.character(anagrafica[RE[1,5],3]),
            sy=as.character(sy),ey=as.character(ey),years=as.character(delta),n_RE=as.character(n_re),
              mean_year=as.character(av_a),D_min=as.character(mind),D_max=as.character(maxd),
                D_median=as.character(medd),D_mean=as.character(meand),E_min=as.character(mine),
                  E_max=as.character(maxe),E_median=as.character(mede),E_mean=as.character(meane)))
      
      totRE<-rbind(totRE,output)
       #...writing the input file processed by the algorithm
      filename<-paste(dir_file_name_2,"/",anagrafica[index_raingauge,i1],"_clean.csv",sep="")
      output1<-data.frame(Date=vector.date[1:(length(vector.date)-1)],EH=as.character( vector.EH[2:length(vector.EH)]      ))               
      write.csv2(output1, file=filename,quote=F,row.names=F)
      
    }
  }
}

#END OF PROCEDURE (RECONSTRUCTION OF RAINFALL EVENTS) ******************

min_sy<-min(as.numeric(start_year),na.rm = T)
max_ey<-max(as.numeric(end_year),na.rm = T)

cat("\f")
print("Progress:...writing output files",quote=F)
filename_RE<-paste(dir_file_name_5,"/Rainfall events.csv",sep="")
if (file.access(filename_RE, mode = 0)==0)               {file.remove(filename_RE)}
write.csv2(totRE, file=filename_RE,quote=F,row.names=F)

#*********************************************************END BLOCK 1****************************************************


#*********************************************************START BLOCK 2****************************************************
#*******************Procedure for the reconstruction of the multiple conditions responsible of landslide*******************


RE<-read.csv(filename_RE, sep=";", skip=0,header=T)

for( k in 1:l_lan) 
{
  cat("\f")
  print(paste("Progress:",round((k*100/l_lan),0),"%"),quote=F)
  clearhistory()
  radLat_l<-pi*(as.numeric(landslide[k,6]))/180
  radLon_l<-pi*(as.numeric(landslide[k,5]))/180
  radLat_r<-pi*(as.numeric(lat_r))/180
  radLon_r<-pi*(as.numeric(lon_r))/180
  
  dist_of_land[k,1:l_ana]<- R*acos((sin(  radLat_l) * sin(  radLat_r)) + (cos(  radLat_l) * cos(  radLat_r) * cos(abs(radLon_l - radLon_r))))
  
  sel_buf<-which(dist_of_land[k,1:l_ana]<=buffer)
  lb<-length(sel_buf)
  if (lb!=0) {
    
    tdate<-strptime(as.character(landslide[k,8]),format("%d/%m/%Y %H:%M" ),tz = "UTC") 
    tdate<-format(tdate+(ceiling(tdate$min/60)*(60-tdate$min)*60),format("%y-%m-%d %H:%M:00+0000"),tz = "UTC")
    
    temp_l<-data.frame(id_land=as.character(landslide[k,1]),
                       code_land=as.character(landslide[k,2]),
                       id_raingauge=as.character(anagrafica[sel_buf,i1]),
                       caa=anagrafica[sel_buf,1],distance=round(dist_of_land[k,sel_buf],2),EM_date=as.character(landslide[k,8]),
                       tool_date=tdate,cx= sel_buf,rl=k,pk_sensor=as.character(anagrafica[sel_buf,4]))
    temp_l<-temp_l[order(temp_l[,5]),]
    land_in_buffer<-rbind( land_in_buffer,temp_l)
    
    stat_land_in_buffer<-rbind(stat_land_in_buffer,data.frame(id_land=as.character(landslide[k,1]),
                                                              code_land=as.character(landslide[k,2]),rain_gauges=lb,
                                                              min_distance=round(min(dist_of_land[k,sel_buf]),2),
                                                              max_distance=round(max(dist_of_land[k,sel_buf]),2),
                                                              mean_distance=round(mean(dist_of_land[k,sel_buf]),2) ))
    
  }  
  else {
    rif_min<-which(dist_of_land[k,1:l_ana]==min(dist_of_land[k,1:l_ana]))
    tdate<-strptime(as.character(landslide[k,8]),format("%d/%m/%Y %H:%M" ),tz = "UTC") 
    tdate<-format(tdate+(ceiling(tdate$min/60)*(60-tdate$min)*60),format("%y-%m-%d %H:%M:00+0000"),tz = "UTC")
    
    land_out_buffer<-rbind( land_out_buffer,data.frame(id_land=as.character(landslide[k,1]),
                                                       code_land=as.character(landslide[k,2]),                                          
                                                       id_raingauge=as.character(anagrafica[  rif_min,i1]),
                                                       caa=anagrafica[rif_min,1],distance=as.character(round(dist_of_land[k,  rif_min],2)),EM_date=as.character(landslide[k,8]),
                                                       tool_date=tdate,note="-" ))
    
  }
}
lk<-length(stat_land_in_buffer$id_land)

for (k in 1:lk) {
  code_color<-c()
  count_color<-1
  cat("\f")
  print(paste("Progress:",round((k*100/lk),0),"%"),quote=F)
  land_id<-stat_land_in_buffer$id_land[k]
  land_code<-stat_land_in_buffer$code_land[k]
  xland<-paste(land_id,"_",land_code,sep="")
  index_proj<-which(land_in_buffer$id_land==land_id & land_in_buffer$code_land==land_code )
  pdf.options(colormode="srgb")
  filename<-paste( dir_file_name_4.1,"/Landslide_",land_id,"_",land_code,".pdf",sep="")
  
  if (length(index_proj)!=0) {



    pdf(file=filename)

    df<-data.frame()
    lt<-length(index_proj) 
    # ****************************DRAW ON MAP

        if (lt!=0){

          zoom_l<-11
          dist_buf<-(11.517*buffer)*(2^(zoom_l-11))
          nrowl<-land_in_buffer$rl[index_proj][1]
          df<-data.frame(id=as.character(land_in_buffer$id_raingauge[index_proj]) ,lon=c(anagrafica[c(land_in_buffer$cx[index_proj]),2]),
                         lat=c(anagrafica[land_in_buffer$cx[index_proj],3]),
                         sh=c(rep(15,lt)),
                         si=c(rep(4,lt)),
                         col=c(rep("mediumblue",lt)),alpha=c(rep(1,lt)),distance=c(land_in_buffer$distance[index_proj])  )


          df<-rbind(df,data.frame(id= c(rep("",2),xland),lon=c(rep(landslide[ nrowl,5],3)),
                                  lat=c(rep(landslide[ nrowl,6],3)),sh=c(1,19,3),si=c(rep(dist_buf,2),10),
                                  col=c("red","red","red"),alpha=c(0.5,0.1,1),distance=c(0,0,0)))

          map <- get_googlemap(center =c(lon =df$lon[lt+1], lat =df$lat[lt+1]), zoom=zoom_l,maptype = "terrain")



          map.new<-ggmap(map,extent = "panel")+geom_point(data = df,
                                                          size = df$si,colour=df$col,shape=df$sh,alpha=df$alpha)


          print(map.new,newpage=T)

        }

    # ************************ EBD DRAW ON MAP




    tot_D_comb<-as.numeric()
    tot_E_comb<-as.numeric()
    leg.1<-c()
    leg.2<-c()

    if (lt!=0){
      n_t<-1
         # lt<-1
      for (t in 1:lt) {
        it<-index_proj[t]
        green.square.D.1<-as.numeric()
        green.square.E.1<-as.numeric()
        data.land<-land_in_buffer$tool_date[it]


        li1<-which(land_in_buffer$id_raingauge[it]==as.character(RE[,1]))
        lli1<-length(li1)
        if (lli1!=0) {                 #check raingauge ok
          ck1<-"positive"
          li2<-which(strptime(data.land,format("%y-%m-%d %H:00:00+0000"))>strptime(RE[li1,4],format("%Y-%m-%d %H:00:00" )))

          green.square.D.1<-c(RE[li1,6])
          green.square.E.1<-c(RE[li1,7])
          data_prov1<-cbind(RE[li1,],li=as.numeric(as.character(li1)))
          data_prov1<-data.frame(data_prov1[order(data_prov1[,7],decreasing = T),],row.names = NULL)

          if (length(li2)!=0) {
            ck2<-"positive"       #check period ok
            li2<-li1[max(li2)]
            target_1<-which(data_prov1$li == li2)

            leg.1<-c(leg.1,paste(land_in_buffer$id_raingauge[it],"          ",land_in_buffer$distance[it],sep=""))
            leg.2<-c(leg.2,cd_color[count_color])
            count_color<-count_color+1
            pluviometri<- read.csv(paste( dir_file_name_2,"/",land_in_buffer$id_raingauge[it],"_clean.csv",sep=""),header=T,sep=";")
            n.r<-land_in_buffer$cx[it]
            index.month.start.warm.season<-as.numeric(as.character(anagrafica[n.r,13]))
            index.month.end.warm.season<-as.numeric(as.character(anagrafica[n.r,14]))
            P2.cold<-as.numeric(as.character(anagrafica[n.r,8]))
            P2.warm<-as.numeric(as.character(anagrafica[n.r,9]))
            start_RE<-as.numeric(as.character(RE[li2,2]))-1
            end_RE<-as.numeric(as.character(RE[li2,3]))-1
            vector.EH<-as.numeric(as.character(pluviometri[ start_RE:end_RE,2]))
            vector.DATE<-strptime(pluviometri[ start_RE:end_RE,1],format("%Y-%m-%d %H:00:00" ))
            analisi_vector<-vector.DATE<=strptime(data.land,format("%y-%m-%d %H:00:00+0000"))
            p_na<-which(is.na(analisi_vector))
            analisi_vector[p_na]=analisi_vector[p_na+1]
            end_RE_land<-max(which(vector.EH[which(analisi_vector)]>smin))
            delta2<-as.numeric(difftime(strptime(data.land,format("%y-%m-%d %H:00:00+0000")), vector.DATE[end_RE_land], units="hours"))
            ifelse(delta2>48,sem_col<-red_code,ifelse(delta2<=12,sem_col<-green_code,sem_col<-yellow_code))
            vector.EH.land<-vector.EH[1:end_RE_land]#end_RE_land=last value of rain triggering for landslide
            attenuation<-k_antecedent^as.integer(seq.int(from=end_RE_land-1, to=0)/24)
            # plot(attenuation)
            vector.EH.land<-vector.EH.land*attenuation
            nzv2<-which(vector.EH.land>0)
            l=length(nzv2)
            n.h.s5<-as.numeric()
            for( m in 1:l) {n.h.s5[m]<-p.C(as.numeric(format(vector.DATE[nzv2[m]],"%m")),P2.cold,P2.warm)}
            delta<-which((nzv2[2:l]-nzv2[1:l-1])-1>=n.h.s5[2:l])
            ld<-length(delta)
            COMB.i<-as.numeric(1)
            sum.s<-as.numeric()
            sums<-as.numeric()
            check_na<-as.numeric()
            max_ip<-as.numeric()
            d_max_ip<-as.numeric()
            dp<-as.numeric()
            E_day_wm<-as.numeric()
            vector_prov<-as.numeric()
            if (ld>0)  {n<-(1:ld);COMB.i<-c(sort(nzv2[delta[n]+1],decreasing=T),COMB.i)}

            for( sum.s in 1:(ld+1)) {
              seq_vet.EH<-vector.EH.land[COMB.i[sum.s]:end_RE_land]
              vector_prov<-vector.EH.land[COMB.i[sum.s]:end_RE_land]
              if (length(vector_prov)<24) {
                E_day_wm<-c( E_day_wm,sum(vector_prov,na.rm = TRUE))
              } else {
                vector_prov[-which(vector_prov!="Na")]<-0
                E_day_wm<-c( E_day_wm,max(runmean(vector_prov, 24,endrule="NA")*24,na.rm=T))
              }

              sums<-c(sums,sum(seq_vet.EH,na.rm = TRUE))
              m_temp<-max(seq_vet.EH,na.rm = TRUE)
              max_ip<-c(max_ip,m_temp)
              d_max_ip<-c( d_max_ip,min(length(seq_vet.EH)-which( vector.EH.land[COMB.i[sum.s ]:end_RE_land]==m_temp)))

              count.na<-length(seq_vet.EH[seq_vet.EH=="NA"])
              ifelse(count.na==0,check_na<-c( check_na,""), check_na<-c( check_na,paste("!NA (",count.na,")",sep="")))
            }

            limit_out<-0#limit for selected first combination
            pl<-10
            c_temp<-which(sums>=limit_out)

            if (length(c_temp)==0) {
              comb_start_index<-ld+1
              check_2<-c(rep("*",comb_start_index))
              color_comb<-c(rep("gray40",comb_start_index))
              color_comb1<-c(rep("gray40",comb_start_index))
            }
            else {
              comb_start_index<-min(c_temp)
              check_2<-c(rep("*",comb_start_index-1))
              check_2<-c(check_2,"I")
              color_comb<-c(rep("gray40",comb_start_index-1))
              color_comb<-c(color_comb,"red")
              color_comb1<-c(rep("gray40",comb_start_index-1))
              color_comb1<-c(color_comb1,"yellow")
              vettore<-c(rep(0,comb_start_index-1))
              vettore<-c(vettore,1)
              if (ld>=comb_start_index) {
                for(i in (comb_start_index+1):(ld+1)) {
                  n<-max(which(vettore==1))
                  p<-100*(sums[i]-sums[n])/sums[n]
                  ifelse (p>=pl,vettore<-c(vettore,1),vettore<-c(vettore,0))
                  if (vettore[i]==1) {
                    check_2<-c(check_2,"ok")
                    color_comb<-c(color_comb,"red")
                    color_comb1<-c(color_comb1,"yellow")
                  }else{
                    check_2<-c(check_2,"no")
                    color_comb<-c(color_comb,"gray40")
                    color_comb1<-c(color_comb1,"gray40")
                  }
                }
              }
              #               clearhistory()

            }

            dp<-d_max_ip+delta2
            D_comb<-end_RE_land-COMB.i+1
            intensity<-round((sums/D_comb),3)
            if (n_t==1) {  n_k<-n_k+1}

            n_no_comb<-length(which(check_2=="no" |check_2=="*") )
            n_ok_comb<-length(check_2)



OUT.COMB<-rbind(OUT.COMB,data.frame(ID_project=land_id,
                                    ID_lan=land_code,
                                    date= data.land,
                                    RRG_distance=as.character(land_in_buffer$distance[it]),
                                    NL=n_k,
                                    RRG_select=n_t,
                                    CO=c(1:(ld+1)),
                                    D_L=D_comb,
                                    E_L=as.character(round(sums,1)),
                                    I_L=as.character(intensity),
                                    NA_check=check_na,
                                    COMB_check=check_2,
                                    ID_rain_gauge=land_in_buffer$id_raingauge[it],
                                    pk_sensor=land_in_buffer$pk_sensor[it],
                                    REN=(li2-li1[1]+1),
                                    cod_area=land_in_buffer$caa[it],
                                    IP_L=as.character(round(max_ip,2)),dh_peak_eRE=d_max_ip,
                                    dh_eRE_lan=delta2,
                                    dh_peak_lan=dp,Emax24_L=as.character(round(E_day_wm,2)),
                                    n_MRC= rep(ld+1-n_no_comb,ld+1)              ))

            n_t<-n_t+1
  
#plot 1
            par(mfrow=c(2,2),mar=c(4.7, 2.7,4.7, 1))
            plot.new()
            top<-1.35
            dtop<-0.025
            col_txt<-" steelblue4"
            col_txt1<-"black"
            rect(0.225, top-0.039,0.57,top+0.04, border="white",col="yellow",lwd=0.3, xpd=TRUE)
            text(-0.1,top,expression(bold(underline("RAIN GAUGE")~':')),col=col_txt, xpd=TRUE,cex=0.9,adj=0)
            text(0.23,top,land_in_buffer$id_raingauge[it],col=col_txt, xpd=TRUE,cex=0.9,adj=0,font=2)
            text(0.6,top,paste("pk_sensor:",land_in_buffer$pk_sensor[it],sep=""),col=col_txt, xpd=TRUE,cex=0.7,adj=0,font=2)
            text(-0.1,top-0.07,paste("Distance from the landslide:",land_in_buffer$distance[it],"km"),col=col_txt1, xpd=TRUE,cex=0.8,adj=0,font=1)
            text(-0.1,top-0.12-dtop,"Temporal coverage",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(-0.1,0.12 ),top-0.18-dtop,c(expression(bold("Start Date:")), paste(substring(pluviometri[1,1],1,16),"+0000",sep="")),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(c(-0.1,0.12 ),top-0.245-dtop,c(expression(bold("Stop Date:")), paste(substring(pluviometri[ length(pluviometri[,1]),1],1,16),"+0000",sep="")),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(-0.1,top-0.32-dtop,"Data Resolution",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(-0.1,0.33 ),top-0.39- dtop,c(expression(bold("Temporal Resolution:")), "hourly"),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(-0.1,top-0.465-dtop,"Rainfall events",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(-0.1,-0.055 ),top-0.535- dtop,c(expression(bold("#:")),lli1),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(-0.1,top-0.61- dtop,"Statistics:",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(0.09,0.29,0.49,0.69,0.89),top-0.68 -dtop,c("Min","Max","Mean","Median","SD"),col="black", xpd=TRUE,cex=0.8,font=2,adj=0 )
            text(c(-0.1,0.09,0.29,0.49,0.69,0.89),top-0.74-dtop,c(expression(bolditalic(D)[bold(E)]~(h)),min( RE[li1,6]),max( RE[li1,6]),round(mean( RE[li1,6]),2),
                                                                  round(median( RE[li1,6]),2),round(sd( RE[li1,6]),2)) ,col="black", xpd=TRUE,cex=0.8,font=1,adj=0 )
            text(c(-0.1,0.09,0.29,0.49,0.69,0.89),top-0.805-dtop,c(expression(bolditalic(E)[bold(E)]~(mm)),min(RE[li1,7]),max(RE[li1,7]),round(mean(RE[li1,7]),2),
                                                                   round(median(RE[li1,7]),2),round(sd(RE[li1,7]),2)),col="black", xpd=TRUE,cex=0.8,font=1,adj=0 )
            rect(0.22, top-0.98-dtop,0.51,top-0.89-dtop, border="white",col="red",lwd=0.3, xpd=TRUE)
            text(-0.1,top-0.94-dtop,expression(bold(underline("ID LANDSLIDE")~':')),col=col_txt, xpd=TRUE,cex=0.8,adj=0)
            text(0.23,top-0.94-dtop,col="white",xland, xpd=TRUE,cex=0.9,adj=0,font=2)
            text(0.55,top-0.94-dtop,col=col_txt,paste("DELAY:",delta2," h",sep=""), xpd=TRUE,cex=0.8,adj=0,font=2)
            rect(0.82, top-0.98-dtop,0.875,top-0.89-dtop, border=col_txt,col=sem_col[1],lwd=0.3, xpd=TRUE)
            rect(0.88, top-0.98-dtop,0.935,top-0.89-dtop, border=col_txt,col=sem_col[2],lwd=0.3, xpd=TRUE)
            rect(0.94, top-0.98-dtop,0.995,top-0.89-dtop, border=col_txt,col=sem_col[3],lwd=0.3, xpd=TRUE)
            text(-0.1,top-1.05-dtop,"Landslide date:",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(0.21,top-1.05-dtop,col=col_txt1,substring(data.land,1,14), xpd=TRUE,cex=0.8,adj=0,font=1)
            text(-0.1,top-1.14-dtop,"Rainfall event associated with the landslide",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(-0.1,-0.055 ),top-1.21-dtop,c(expression(bold("#:")),(li2-li1[1]+1)),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(c(-0.1,0.12 ),top-1.275-dtop,c(expression(bold("Start Date:")), paste(substring(RE[li2,4],1,16),"+0000",sep="")),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(c(-0.1,0.12 ),top-1.34-dtop,c(expression(bold("Stop Date:")), paste(substring(RE[li2,5],1,16),"+0000",sep="")),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(c(-0.1,-0.01),top-1.405-dtop,c(expression(bolditalic(D)[bold(E)]~bold(':')),paste(RE[li2,6],"h" )),col="black", xpd=TRUE,cex=0.8,adj=0 )
            text(c(-0.1,-0.01),top-1.47-dtop,c(expression(bolditalic(E)[bold(E)]~bold(":")),paste(RE[li2,7],"mm")),col="black", xpd=TRUE,cex=0.8,adj=0 )
            text(-0.1,top-1.55-dtop,"Rainfall conditions responsible for the landslide",col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(-0.1,-0.055 ),top-1.61-dtop,c(expression(bold("#:")),n_ok_comb),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
            text(-0.1,top-1.69-dtop,paste("Discarded rainfall conditions (","threshold value:",pl, "%)",sep=""),col=col_txt, xpd=TRUE,cex=0.8,adj=0,font=2)
            text(c(-0.1,-0.055 ),top-1.75-dtop,c(expression(bold("#:")),n_no_comb),col=col_txt1, xpd=TRUE,cex=0.8,adj=0)
#end plot 1

#plot 2
            par(mgp=c(1.45,0.3,0))
            xld<-expression(italic(D)["E "](h))
            xle<-expression(italic(E)["E "](mm))
            vet.lab.x <- c(expression(10^-1),expression(10^0),expression(10^1),expression(10^2),expression(10^3))
            vet.lab.y <- c(0.1,1,10,100,1000,5000)
            plot(green.square.D.1,green.square.E.1,xlim=c(0.1,3000),ylim=c(0.1,1000),log="xy",col="gray7",bg="forestgreen",
                 axes=FALSE,frame.plot = 9,pch=22,cex=0.9,xlab=xld,ylab=xle,cex.lab=1,lwd=0.1)
            lines(RE[li2,6],RE[li2,7],type="p",col="gray7",bg="red",pch=22,cex=0.9,lwd=0.1)
            axis(1,c(10^-1,10^0,10^1,10^2,10^3) ,vet.lab.x,cex.axis=0.8,las=0,tck=tck.length)
            axis(2,c(10^-1,10^0,10^1,10^2,10^3) ,vet.lab.x,cex.axis=0.8,las=2,tck=tck.length)
            legend(box.lty=0,bty="o",0.1,0.5, c("Rainfall events",paste("Rainfall event responsible for ",xland,sep="")),
                   col = "gray7", pch = 22,pt.bg=c("forestgreen","red"),cex=0.8,pt.lwd=0.1,pt.cex=0.9)

#end plot 2


#plot3
            par(mgp=c(1.45,0.3,0))
            max.value<-30#max(rain.gauge.mod.EH[(a1:a2),t])
            min.value<-1.28#(0.03*max.value)+0.5
            dt.comb<-end_RE-start_RE+1
            interval.date<-seq.int(from = 1, to =dt.comb , by =24)
            lx<-(dt.comb/242)*50
            lz<-(end_RE_land/dt.comb)
            ifelse(lz >= 0.5, xf<--lx, xf<-lx)
            ifelse(vector.EH[end_RE_land]>max.value,end_lim<-max.value,end_lim<-vector.EH[end_RE_land])
            plot( vector.EH,pch=22,col="forestgreen",type="h",ylab="Rainfall (mm)",
                  axes=FALSE,xlab="",frame.plot = 9,ylim=c(min.value,max.value),lwd=1.5,las=2,cex.lab=1,lend=2)
             lines(vector.EH.land,pch=22,col="red",type="h",lwd=1.2,lend=2)
            axis(1, at=  interval.date,labels=substr(vector.DATE[interval.date],6,16),
                 cex.axis=0.6,las=2,lend=2,tck=tck.length)
            axis(2, cex.axis=0.6,tck=tck.length)
            arrows(end_RE_land,max.value+0.8,end_RE_land,end_lim,length = 0.1,
                   angle =25,lwd=0.3,col="black",xpd=TRUE)
            rect(end_RE_land, max.value+0.8,end_RE_land+xf ,max.value-0.9, border="black",col="red",lwd=0.3)
            text(end_RE_land+(xf/2), max.value,xland,col="white", xpd=TRUE,cex=0.6,font=2 )
            text((dt.comb/2),-10,"Time (h)", xpd=TRUE,cex=1 )
            xld<-bquote("Rainfall event" ~ italic(D)[E] ~ "=" ~ .(RE[li2,6]) ~ "h  " ~ italic(E)[E] ~ "=" ~ .(RE[li2,7])~  "mm"  )
            text((dt.comb/2),max.value+2.5,xld,cex=0.8,xpd=TRUE )
#end plot 3


#plot 4
        par(mgp=c(1.45,0.3,0))
        xld<-expression(italic(D)["L "](h))
        xle<-expression(italic(E)["L "](mm))
        vet.lab.x <- c(expression(10^-1),expression(10^0),expression(10^1),expression(10^2),expression(10^3))
        vet.lab.y <- c(1,10,100,1000,5000)
        plot(   D_comb,sums,xlim=c(1,1000),ylim=c(1,1000),log="xy",col="black",bg=color_comb,
                axes=FALSE,frame.plot = 9,pch=21,cex=1.3,xlab=xld,ylab=xle,cex.lab=1)
        axis(1,c(10^-1,10^0,10^1,10^2,10^3) ,vet.lab.x,cex.axis=0.8,las=1,tck=tck.length )
        axis(2,c(10^-1,10^0,10^1,10^2,10^3) ,vet.lab.x,cex.axis=0.8,las=2,tck=tck.length )
        legend(box.lty=0,bty="o",1,3, c("Likely rainfall condition (LRC)","Discarded rainfall condition (DRC)"),pt.bg=c("red","gray40"),
               col = "black",pch = 21,cex=0.8,pt.lwd=0.4,pt.cex=1.3)
#end plot4

#plot rainfall series of combinations
            lrc<-0
            drc<-0
            for( pp in 1:(ld+1)) {
              v1.bis<-c(rep(0,COMB.i[pp]-1),vector.EH.land[COMB.i[pp]:end_RE_land])
              par(mgp=c(1.45,0.3,0))
              plot( vector.EH,pch=22,col="grey90",type="h",ylab="Rainfall (mm)",
                    axes=FALSE,xlab="",frame.plot = 9,ylim=c(min.value,max.value),lwd=1.5,las=2,cex.lab=1,lend=2)
              #           lines(v1,pch=22,col=color_comb[pp],type="h",lwd=1.5,lend=2)
              lines(v1.bis,pch=22,col=color_comb[pp],type="h",lwd=1.5,lend=2)
              axis(1, at=  interval.date,labels=substr(vector.DATE[interval.date],6,16),
                   cex.axis=0.6,las=2,lend=2,tck=tck.length)
              axis(2, cex.axis=0.6,tck=tck.length)
              arrows(end_RE_land,max.value+0.8,end_RE_land,end_lim,length = 0.1,
                     angle =25,lwd=0.3,col="black",xpd=TRUE)
              rect(end_RE_land, max.value+0.8,end_RE_land+xf ,max.value-0.9, border="black",col="red",lwd=0.3)
              text(end_RE_land+(xf/2), max.value,xland,col="white", xpd=TRUE,cex=0.6,font=2 )
              text((dt.comb/2),-10,"Time (h)", xpd=TRUE,cex=1 )

              if (color_comb[pp]=="gray40") {
                drc<-drc+1
                xld<-bquote("DRC"~.(drc)["of"~.( n_no_comb)] ~" " ~italic(D)[L] ~ "=" ~ .(D_comb[pp]) ~ "h  "
                            ~ italic(E)[L] ~ "=" ~ .(round(sums[pp],1)) ~  "mm  " ~ italic(I)[L] ~ "=" ~ .(intensity[pp])  ~  "mm/h" )

              }else{
                lrc<-lrc+1
                xld<-bquote("LRC"~.(lrc)["of"~.(n_ok_comb-n_no_comb)] ~" " ~italic(D)[L] ~ "=" ~ .(D_comb[pp]) ~ "h  "
                            ~ italic(E)[L] ~ "=" ~ .(round(sums[pp],1) ) ~  "mm  " ~ italic(I)[L] ~ "=" ~ .(intensity[pp])  ~  "mm/h" )
                tot_D_comb<-c( tot_D_comb,D_comb[pp])
                tot_E_comb<-c( tot_E_comb,round(sums[pp],1))
                code_color<-c( code_color,cd_color[count_color-1])
              }


              text((dt.comb/2),max.value+2.5,xld,cex=0.8,xpd=TRUE )

            }
            

          }else {  #non c'h ikl pluviometro quindi
            ck2<-"negative"
            li2<-"-"

            land_out_buffer<-rbind( land_out_buffer,data.frame(id_land=as.character(landslide[k,1]),
                                                               code_land=as.character(landslide[k,2]),
                                                               id_raingauge=as.character(land_in_buffer$id_raingauge[it]),
                                                               caa=land_in_buffer$caa[it],distance=as.character(land_in_buffer$distance[it]),
                                                               EM_date=as.character(land_in_buffer$EM_date[it]),
                                                               tool_date= land_in_buffer$tool_date[it],note="data out of range"))
          }


        }else{
          ck1<-"negative"
          ck2<-"negative"
          li2<-"-"
        }

        if (it!=0){
          stat.1<-rbind(stat.1,data.frame(ID_raingauge=land_in_buffer$id_raingauge[it],
                                          distance=land_in_buffer$distance[it],
                                          file_check=ck1,period_check=ck2,index_RE=as.character(li2),
                                          debug=0                           ))


        }


  #END  plot rainfall series of combinations

#********Final

      }


    }

    par(mfrow=c(1,1),mar=c(1, 1,1, 1))
    plot.new()
    text(0,0.5, paste("Landslide: ",land_id,sep=""), lwd=2, lty=1, bty="n", col="black",adj=0,cex=1)

    dev.off()
    clearhistory()
  }
  
} 


OUT.COMB$A_class<- cut(as.numeric(as.character(OUT.COMB$Emax24_L)), breaks=c(0,4,16,32,64,128,1000),dig.lab=4,include.lowest = T,labels = c(1:6))
clearhistory()
#integrazione per il calcolo dello score e scelta delle combinazioni associate a quella con score max
filter_buffer<-buffer
min_year<-1
max_year<-20
Emin<-5
n_delay<-48
region<-substring(OUT.COMB[1,1],1,3)
bit_exclusive_1<-ifelse(OUT.COMB[,11]!="",0,1)
bit_exclusive_2<-ifelse(OUT.COMB[,12]=="no",0,1)
bit_exclusive_3<-ifelse(OUT.COMB[,19]>n_delay,0,1)
bit_exclusive<-bit_exclusive_1*bit_exclusive_2*bit_exclusive_3

score<-bit_exclusive*(1/(as.numeric(as.character(OUT.COMB[,4]))+0.00001)^2)*as.numeric(as.character(OUT.COMB[,9]))*as.numeric(as.character(OUT.COMB[,10])) 
                           #exclusive*(1/RG_dist^2)*EL*IL  # matching with expert method 76.85%

OUT.COMB<-cbind(OUT.COMB,exclusive=bit_exclusive,score=as.character(score))
lista<-unique(OUT.COMB[,5])
l_lista_land<-length(lista)
autoexpert<-data.frame()
autoexpert_max<-data.frame()
pesi<-as.numeric()
pesi_max<-as.numeric()
pesi_comb_t<-as.numeric()
filter<-as.numeric()
filter_max_score<-as.numeric()
f_current<-as.numeric()
MyDate <-as.numeric(format(as.Date(OUT.COMB[,3],origin="1900-01-01"), "%Y"))  
Mymonth <-as.numeric(format(as.Date(OUT.COMB[,3],origin="1900-01-01"), "%m")) 
for( r in 1:l_lista_land ) {
  
  r1<-which(OUT.COMB[,5]==r)
  representative_RG<-OUT.COMB[r1[which.max(as.numeric(as.character(OUT.COMB$score[r1])))],6]
  r2<-which(OUT.COMB[,6]==representative_RG)
  r3<-which(OUT.COMB[,24]==1)
  r4<-which(MyDate<=max_year) #selection of a sample with year less or equal than the variable "max_year"
  r5<-which(MyDate>=min_year) #selection of a sample with year majorthan the variable "min_year"
  r6<-which(as.numeric(as.character(OUT.COMB[,9]))>Emin)
    
  f_current<-Reduce(intersect,list(r1,r2,r3,r4,r5,r6))
  l_f<-length(f_current)
  filter<-c(filter, f_current)
  filter_max_score<-c( filter_max_score, f_current[which.max(as.numeric(as.character(OUT.COMB[f_current,25])))])
  score_current<-as.numeric(as.character(OUT.COMB[f_current,25]))
  somma_score<-sum(score_current)
  
  pesi_comb_t<-c(pesi_comb_t,score_current/somma_score)
  pesi<-c(pesi,rep((1/l_f),l_f))
  pesi_max<-c(pesi_max,rep((1/l_f),1))
}


autoexpert<-data.frame(OUT.COMB[filter,])
msf<-rep(0,length(OUT.COMB[,1]))
msf[filter_max_score]<-1
autoexpert<-cbind(autoexpert,wh=as.character(round(pesi,2)),whc=as.character(round(pesi_comb_t,2)),ms_flag=msf[filter])
autoexpert_max<-data.frame(subset(autoexpert,ms_flag==1))
autoexpert_max[,27]<-1
autoexpert_max[,22]<-1

autoexpert_max_single<-data.frame(subset(autoexpert,n_MRC==1))
autoexpert_max_single[,27]<-1


a1a<-as.character(unique(autoexpert[,1]))
b1b<-as.character(unique(land_out_buffer$id_land))
c1c<-as.character(unique(landslide$ID_project))
desc<-setdiff(b1b,a1a)
desc2<-setdiff(c1c,a1a)
lista_fatti<-as.character(unique(autoexpert$ID_project))
l_lista_fatti<-length(lista_fatti)


for( r in 1:l_lista_fatti ) {
  index_mrc<-which(autoexpert[,1]==lista_fatti[r])
  n_mrc<-length(index_mrc)
    autoexpert[index_mrc,22]<-n_mrc
}



# filename<-paste(dir_file_name_4,"/ALL_MRC.csv",sep="") #all rainfall condition for each rain gauge in the circle buffer area
# write.csv2(OUT.COMB, file=filename,quote=F,row.names=F)
autoexpert_def<-data.frame(subset(autoexpert,select=c("ID_project","ID_lan","date","RRG_select","RRG_distance","D_L","E_L","I_L","ID_rain_gauge","REN",
                                                    "IP_L","Emax24_L","A_class","ms_flag","n_MRC")),row.names=NULL)




filename<-paste(dir_file_name_4,"/MRC.csv",sep="") #Multiple Rainfall Condition
write.csv2(autoexpert_def, file=filename,quote=F,row.names=F)
# 
 # filename<-paste( dir_file_name_4,"/MRC_original.csv",sep="")#Maximum Score Rainfall Condition
 # write.csv2(autoexpert, file=filename,quote=F,row.names=F)

filename<-paste( dir_file_name_4,"/MPRC.csv",sep="") #Single Rainfall Condition

autoexpert_max_def<-data.frame(subset(autoexpert_def,ms_flag==1))
write.csv2(autoexpert_max_def, file=filename,quote=F,row.names=F)

filename<-paste(dir_file_name_4,"/ Processing Summary report.txt",sep="")


if (file.access(filename,  mode = 0)==0)               {file.remove(filename)}
write(paste("Total landslide #:",length(c1c),"\n","Total landslides reconstructed #:",length(a1a),"\n",            
            "Total landslides discarded #:",length(desc2),"\n",
            "List landslides discarded:"),file = filename,ncolumns=1, append=T)

write(desc2,file = filename,ncolumns=1, append=T)




#*********************************************************END BLOCK 2****************************************************


#*********************************************************START BLOCK 3****************************************************
#*************************************************Procedure for definition the thresholds**********************************

number_of_boot<-100
xld<-expression(italic(D)["l "](h))
xle<-expression(italic(E)["l "](mm))
xli<-expression(italic(I)["L "](mm/h))
percentage.values<-c(55,60,65,68.3,70,75,80,82,84,85,86,88,90,91,92,93,94,95,95.45,96,97,98,99,99.8,99.99,30)
perc.values<-c(2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,99)
q.values<-perc.values/100
sigma.coef<-c(0.7552,.8415,.9345,1,1.0364,1.1504,1.2817,1.34,1.405,1.4398,1.475,
              1.555,1.6452,1.6958,1.7511,1.8123,1.8812,1.9607,2,2.0542,2.1705,2.3268,2.5762,3.0905,3.91,0.3849)

vet.lab.yy <- c(expression(10^-2),expression(10^-1),expression(10^0),expression(10^1),expression(10^2),expression(10^3))
vet.lab.yx <- c(expression(10^-1),expression(10^0),expression(10^1),expression(10^2),expression(10^3),expression(10^4))
bin.width = 0.1
no.bins = 5
pvl<-length(percentage.values)   
sample.number_all<-c(10,20,30,40,50,75,100,125,150,175,200,250,300,350,400,500,750,1000,1500,2000,3000)
quant<-c(0.5,0.35,0.2,0.1,0.09,0.08,0.07,0.06,0.05,0.04,0.03,0.02,0.015,0.01,0.005,0.00005)
lquant<-length(quant)
leg<-c("MRC","MPRC","SRC")
col_leg<-c("purple","orange","red")
# *****************************************************

for (th in 1:2) {
  clearhistory ()
#   rm(DB_value_out,vettore_dati,vettore_alfa_medio,tk)
  
data_table1<-autoexpert[,]

  if (th==2) {
    data_table1<-autoexpert_max[,]
     }

# if (th==3) {
#   data_table1<-autoexpert_max_single[,]
#   }

data_table2<-data.frame(subset(data_table1,ms_flag==1))
  
length_vector_combination<-length(data_table1[,1])
  list_n_land<-unique(data_table1[,1])
    n_land<-length(list_n_land)


count<-as.vector( rep(0,10),mode = "any")
for( r in 1: n_land ) {
  index_mrc<-which(data_table1[,1]==list_n_land[r])
  n_mrc<-length(index_mrc)
  count[n_mrc]<-count[n_mrc]+1
  
}
max_n_mrc<-max(data_table1[,22])
 
  
  data_table1<-data_table1[order(data_table1[,8],decreasing = F),]

class_intensity<-data_table1[,22]          #CL
length_vector<-n_land
sample.number<-c(sample.number_all[which(sample.number_all<length_vector)],length_vector)
  
  filename<-paste( dir_file_name_3,"/boot_",leg[th],".pdf",sep="")
  pdf(file=filename,paper = "special", width =30,height =21,pointsize=30,colormodel="srgb",
      useDingbats=F,fillOddEven=T,version="1.7")
 

lo<-layout(matrix(c(1,2,4, 3,3,5, 3,3,6),3,3),
           heights=c(0.6,0.6,1),widths=c(1,1,1))

  duration<-as.numeric(as.character(data_table1[,8]))
    cumulative.rainfall<-as.numeric(as.character(data_table1[,9]))
      intensity<-as.numeric(as.character(data_table1[,10]))
        probability_combinations<-as.numeric(as.character(data_table1[,27]))
          duration.original<-duration
        intensity.original<-intensity
      cumulative.original<-cumulative.rainfall
    log.duration<-log10(duration)
  log.intensity<-log10(intensity)
no.events<-length(duration)
  
  

#1
# max_value<-max(duration)
max_value<-500
par(mgp=c(1.8,0.4,0),mar=c(3,4,1,2))
plot(x=NULL,y=NULL,axes=T,ylim=c(0,1),xlim=c(0,max_value),xlab="Duration (h)",ylab="ECDF",main="",cex.lab=2,yaxs="i",xaxs="i")
lines(ecdf(duration),do.points = F,verticals =T,col=col_leg[th],
      cex=2.2,cex.axis=1.2,lwd=1)  

#2
# max_value<-max(cumulative.rainfall) 
par(mgp=c(1.8,0.4,0),mar=c(3,4,1,2))
plot(x=NULL,y=NULL,axes=T,ylim=c(0,1),xlim=c(0,max_value),xlab="Cumulative rainfall (mm)",ylab="ECDF",main="",cex.lab=2,yaxs="i",xaxs="i")
lines(ecdf(cumulative.rainfall),do.points = F,verticals =T,col=col_leg[th],
      cex=2.2,cex.axis=1.2,lwd=1)  


#3
par(mgp=c(2.2,0.8,0),mar=c(5,4,1,2))
  plot(duration,cumulative.rainfall,xaxt="n", yaxt="n",yaxs="i",xaxs="i", log="xy",type="p",
    xlab=xld, ylab=xle,ylim=c(1,1000),xlim=c(1,1000),pch=21,cex=2*probability_combinations,
      col="gray7",bg=col_leg[th],cex.lab=2)
        axis(2,c(0.01,0.1,1,10,100,1000),vet.lab.yy,cex.axis=1.6,las=1,tck=-0.01)
          axis(1,c(0.1,1,10,100,1000,10000),vet.lab.yx,cex.axis=1.6,las=1,tck=-0.01)
            rect(1.05,1.3,960,5,col="gray90")
              legend(1,2,paste(no.events,leg[th],sep=" "),pt.bg=col_leg[th],col="gray7",lwd=NA,pch=21,bty = "n",
                x.intersp =0,cex=1,box.lty=0,pt.cex=2,pt.lwd=1)
            text(1.1,4, "File input raingauge:", lwd=2, lty=1, bty="n", col="black",adj=0,cex=0.7)
          text(2.2,4,paste(current_dir,"CTRL-T/table_of_raingauge.csv",sep=""), lwd=2, lty=1, bty="n", col="red",adj=0,cex=0.7)
        text(1.1,3, "File input landslide:", lwd=2, lty=1, bty="n", col="black",adj=0,cex=0.7)
      text(2.2,3, paste(current_dir,"CTRL-T/landslides.csv",sep=""), lwd=2, lty=1, bty="n", col="red",adj=0,cex=0.7)
    text(1.1,850, "Probability", lwd=2, lty=1, bty="n", col="gray7",adj=0,cex=0.7)
  legend(rep(1.1,2),c(840,1),c("1.00","0.75","0.50","0.25","0.10"),pt.bg=col_leg[th],col="gray7",
lwd=NA,pch=21,bty = "n",cex=0.7,box.lty=0,pt.cex=2*c(1,0.75,0.5,0.25,0.1),pt.lwd=1,y.intersp = 1.25)
#



#4
par(mgp=c(1.8,0.4,0),mar=c(5,4,1,2))
  plot(x=NULL,y=NULL,axes=T,ylim=c(0,1),xlim=c(0,buffer),xlab="RRG_distance (km)",ylab="ECDF",main="",cex.lab=2,yaxs="i",xaxs="i")
    rg_distance<-as.numeric(as.character(data_table2[,4]))  #RRG_distance
  lines(ecdf(rg_distance),do.points = F,verticals =T,col=col_leg[th],
cex=2.2,cex.axis=1.2,lwd=1)                                 #ECDF of #rg_distance




#5
par(mgp=c(1.8,0.4,0),mar=c(5,4,1,2))
  rg_select<-as.numeric(as.character(data_table2[,6]))
    hist(rg_select, freq=F,ylim = c(0, 1),breaks=c(0:max(as.numeric(as.character(data_table2[,6])),na.rm=T)),yaxs="i",
  xaxs="i",col=col_leg[th],xlab="RRG_select",ylab="Density",main="",cex.lab=2)
box()

#6
par(mgp=c(1.8,0.4,0),mar=c(5,4,1,2))
nmrc<-as.numeric(as.character(data_table2[,22]))
hist(nmrc, freq=F,ylim = c(0, 1),breaks=c(0,1,2,3,4,5,6,7,8,9,10),yaxs="i",
     xaxs="i",col=col_leg[th],xlab="n_MRC",ylab="Density",main="",cex.lab=2)
box()

# *****************************************************
alpha.0_005.vec<-vector(mode = "numeric", length = number_of_boot) 
alpha.0_5.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.1.vec<-vector(mode = "numeric", length = number_of_boot) 
alpha.1_5.vec<-vector(mode = "numeric", length = number_of_boot) 
alpha.2.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.3.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.4.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.5.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.6.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.7.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.8.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.9.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.10.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.20.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.35.vec<-vector(mode = "numeric", length = number_of_boot)
alpha.50.vec<-vector(mode = "numeric", length = number_of_boot)

beta.vec<-vector(mode = "numeric", length = number_of_boot)



  DB_value_out<-data.frame()
  
  lsnu<-length(sample.number)
  for (n_time in 1:lsnu) #default of maximum lenght 
  {
    clearhistory()
    cat("\f")
    ciclo<-sample.number[n_time]
    for (n_boot in 1:number_of_boot)    #*********** open for cycle on n.boot
    {
    
      clearhistory()
      cat("\f")
      print(paste("Process #:",th," of 2",sep=""),quote=F)
      print(paste("Progress: Sample ",n_time, " of ",length(sample.number)," (sample size = ",ciclo," points)",sep=""),quote=F)
      print(paste("Progress :",round((n_boot*100/number_of_boot),0),"%"),quote=F)
      #************************** Bootstrap *************************
      index_rif_land<-c()
      no.sample_index_landslide<-sort(sample(c(1:length_vector),ciclo,replace = TRUE, prob = NULL))
      index_sample_id_land<-list_n_land[no.sample_index_landslide]
 
      for (sd_i in 1:ciclo) {
        
         tank_index<-which(data_table1$ID_project==index_sample_id_land[sd_i])
      
        ifelse(length(tank_index)==1 ,index_rif_land<-c(index_rif_land,tank_index),
               index_rif_land<-c(index_rif_land,sample(tank_index,1,prob = probability_combinations[tank_index])))

        
      }


      no.sample<-sort(index_rif_land)
       # no.sample<-sort(sample(c(1:length_vector_combination),sample.number[n_time],replace = TRUE, prob = probability_combinations))
    
      duration<-duration.original[no.sample]
        intensity<-intensity.original[no.sample]
          cumulative.rainfall<-cumulative.original[no.sample]
            no.events<-length(duration)
      
      log.duration<-log10(duration)
      log.intensity<-log10(intensity)
      fit.straight.line <- lm(log.intensity ~ log.duration)
      a.straight<-coef(fit.straight.line)[1]
      b.straight<-coef(fit.straight.line)[2]
      value.straight.line<-predict(fit.straight.line)  
  
      #********** Difference calculation **************************

      difference<-log.intensity - value.straight.line

      
      #********** Kernel density calculation **************************
      
      diff.den<-density(difference, bw = "nrd0", adjust = 1, from =-1, to=1)
      
      
      #*********** Maximum likelihood fit ****************************
      
      mlf.gaussian<-fitdistr(difference,"normal")
      mean.gaussian<-coef(mlf.gaussian)[1]
      sigma.gaussian<-coef(mlf.gaussian)[2]
      gaussian.curve<-dnorm(diff.den$x,mean=mean.gaussian,sd=sigma.gaussian)
      
      
      
      #******************** Calculation of the rainfall threshold for different percentage values **************************
      
      for (perc in 1:pvl)
      {
          a.straight.low<-a.straight-sigma.coef[perc]*sigma.gaussian
            a.straight.high<-a.straight+sigma.coef[perc]*sigma.gaussian
              assign(paste("a.straight.low.",perc,sep=""),a.straight.low)
                assign(paste("a.straight.high.",perc,sep=""),a.straight.high)
                value.straight.line.low<-a.straight.low + b.straight*log.duration
             value.straight.line.high<-a.straight.high + b.straight*log.duration
           assign(paste("value.straight.line.low.",perc,sep=""),value.straight.line.low)
          assign(paste("value.straight.line.high.",perc,sep=""),value.straight.line.high)
        
      }
      
      
      alpha.0_005.vec[n_boot]<- round(10^a.straight.low.25,1)
      alpha.0_5.vec[n_boot]<- round(10^a.straight.low.23,1)
      alpha.1.vec[n_boot]<- round(10^a.straight.low.22,1)
      alpha.1_5.vec[n_boot]<- round(10^a.straight.low.21,1)
      alpha.2.vec[n_boot]<- round(10^a.straight.low.20,1)
      alpha.3.vec[n_boot]<- round(10^a.straight.low.17,1)
      alpha.4.vec[n_boot]<- round(10^a.straight.low.15,1)
      alpha.5.vec[n_boot]<- round(10^a.straight.low.13,1)
      alpha.6.vec[n_boot]<- round(10^a.straight.low.12,1)
      alpha.7.vec[n_boot]<- round(10^a.straight.low.11,1)
      alpha.8.vec[n_boot]<- round(10^a.straight.low.9,1)
      alpha.9.vec[n_boot]<- round(10^a.straight.low.8,1)
      alpha.10.vec[n_boot]<- round(10^a.straight.low.7,1)
      alpha.20.vec[n_boot]<- round(10^a.straight.low.2,1)
      alpha.35.vec[n_boot]<- round(10^a.straight.low.26,1)
      alpha.50.vec[n_boot]<- round(10^a.straight,1)
      beta.vec[n_boot]<- round(-1*(b.straight),2)
      
      
      
      
    }  #*********** close for cycle on n.boot
    

    mean.beta<-mean(beta.vec, na.rm = T)
    mean.alpha.0_005<-mean(alpha.0_005.vec, na.rm = T)
    mean.alpha.0_5<-mean(alpha.0_5.vec, na.rm = T)
    mean.alpha.1<-mean(alpha.1.vec, na.rm = T)
    mean.alpha.1_5<-mean(alpha.1_5.vec, na.rm = T)
    mean.alpha.2<-mean(alpha.2.vec, na.rm = T)
    mean.alpha.3<-mean(alpha.3.vec, na.rm = T)
    mean.alpha.4<-mean(alpha.4.vec, na.rm = T)
    mean.alpha.5<-mean(alpha.5.vec, na.rm = T)
    mean.alpha.6<-mean(alpha.6.vec, na.rm = T) 
    mean.alpha.7<-mean(alpha.7.vec, na.rm = T)
    mean.alpha.8<-mean(alpha.8.vec, na.rm = T)
    mean.alpha.9<-mean(alpha.9.vec, na.rm = T)
    mean.alpha.10<-mean(alpha.10.vec, na.rm = T)
    mean.alpha.20<-mean(alpha.20.vec, na.rm = T)
    mean.alpha.35<-mean(alpha.35.vec, na.rm = T)
    mean.alpha.50<-mean(alpha.50.vec, na.rm = T)
    
    vettore_alfa_medio<-c(mean.alpha.50,mean.alpha.35,mean.alpha.20,mean.alpha.10,mean.alpha.9,mean.alpha.8,mean.alpha.7,
                          mean.alpha.6,mean.alpha.5,mean.alpha.4,mean.alpha.3,mean.alpha.2,mean.alpha.1_5,mean.alpha.1,mean.alpha.0_5,mean.alpha.0_005)
    
    
    
    
    
    sigma.beta<-round(sd(beta.vec, na.rm = T),5)
    sigma.alpha.0_005<-sd(alpha.0_005.vec, na.rm = T)
    sigma.alpha.0_5<-sd(alpha.0_5.vec, na.rm = T)
    sigma.alpha.1<-sd(alpha.1.vec, na.rm = T)
    sigma.alpha.1_5<-sd(alpha.1_5.vec, na.rm = T)
    sigma.alpha.2<-sd(alpha.2.vec, na.rm = T)
    sigma.alpha.3<-sd(alpha.3.vec, na.rm = T)
    sigma.alpha.4<-sd(alpha.4.vec, na.rm = T)
    sigma.alpha.5<-sd(alpha.5.vec, na.rm = T)
    sigma.alpha.6<-sd(alpha.6.vec, na.rm = T)
    sigma.alpha.7<-sd(alpha.7.vec, na.rm = T)
    sigma.alpha.8<-sd(alpha.8.vec, na.rm = T)
    sigma.alpha.9<-sd(alpha.9.vec, na.rm = T)
    sigma.alpha.10<-sd(alpha.10.vec, na.rm = T)
    sigma.alpha.20<-sd(alpha.20.vec, na.rm = T)
    sigma.alpha.35<-sd(alpha.35.vec, na.rm = T)
    sigma.alpha.50<-sd(alpha.50.vec, na.rm = T)
    
    vettore_alfa_sigma<-c(sigma.alpha.50,sigma.alpha.35,sigma.alpha.20,sigma.alpha.10,sigma.alpha.9,sigma.alpha.8,sigma.alpha.7,
                          sigma.alpha.6,sigma.alpha.5,sigma.alpha.4,sigma.alpha.3,sigma.alpha.2,sigma.alpha.1_5,sigma.alpha.1,sigma.alpha.0_5,sigma.alpha.0_005)
    
    DB_value_out<-rbind(DB_value_out,data.frame(sample=sample.number[n_time],variable="beta",probability=0.5,mean=mean.beta,sigma=sigma.beta,
                                                min=round(mean.beta-sigma.beta,2),max=round(mean.beta+sigma.beta,2)   ))
    
    DB_value_out<-rbind(DB_value_out,data.frame(sample=sample.number[n_time],variable="alfa",probability=quant[c(1:lquant)],
                                                mean=vettore_alfa_medio[1:lquant],sigma=round(vettore_alfa_sigma[1:lquant],3),
                                                min=round(vettore_alfa_medio[1:lquant]-vettore_alfa_sigma[1:lquant],1),
                                                max=round(vettore_alfa_medio[1:lquant]+vettore_alfa_sigma[1:lquant],1)       ))
    
    if (n_time==length(sample.number)) {
      
      q<-layout(matrix(c(0,0,0,0,0,0,0,   0,1,1,0,3,3,0,   0,1,1,0,3,3,0,  0,0,0,0,3,3,0, 0,0,0,0,0,0,0,  
                         0,2,2,0,4,4,0, 0,2,2,0,4,4,0, 0,2,2,0,4,4,0, 0,2,2,0,0,0,0, 0,0,0,0,0,0,0  ),7,10),
                
                heights=c(0.15,1,1,0.45,1,1,0.45),widths=c(0.5,1,1,0.5,0.2,1,0.5,1,0.5,1.4))
      
      
      par(mar=c(4,4,2,1),mgp=c(2.3,0.7,0))
      #PAGE2 UNCERTAIN
      poligoni<-data.frame()
      l_do<-max(length(duration.original),max(duration.original))
      x_vector<-seq.int(from=min(duration.original), to=max(duration.original),length.out=l_do)
      
      poligoni<-data.frame(duration=x_vector)
      
      for ( tk in 2:(length(quant)+1)) {
        #    for ( tk in 2:2) {
        punt<-seq(from=tk, to=length(sample.number)*17, by=17)
        punt_start<-seq(from=0, to=(length(sample.number)-1)*17, by=17)
        md<-max(x_vector)
        mmd<-min(x_vector)
        mid<-length(x_vector)
        punt_beta<-seq(from=1, to=length(sample.number)*17, by=17)
        
        vettore_dati<-matrix(nrow = mid, ncol = 6)
        
        gamma_new<-(1-DB_value_out$mean[punt_beta[n_time]])
        delta_gamma<-DB_value_out$max[punt_beta[n_time]]-DB_value_out$mean[punt_beta[n_time]]
        gamma_min<-gamma_new-delta_gamma
        gamma_max<-gamma_new+delta_gamma
        
        
        vettore_dati[1:mid,1]<-DB_value_out$min[tk+punt_start[n_time]]*x_vector^(gamma_new)#  E=alfa_min*D^(gamma_medio)
        vettore_dati[1:mid,2]<-DB_value_out$max[tk+punt_start[n_time]]*x_vector^(gamma_new)#  E=alfa_max*D^(gamma_medio)
        vettore_dati[1:mid,3]<-DB_value_out$mean[tk+punt_start[n_time]]*x_vector^(gamma_min)#  E=alfa_medio*D^(gamma_min)
        vettore_dati[1:mid,4]<-DB_value_out$mean[tk+punt_start[n_time]]*x_vector^(gamma_max)#  E=alfa_medio*D^(gamma_max)
        
        
        for( k in 1:l_do) {
          vettore_dati[k,5]<-min(vettore_dati[k,c(1:4)])
          vettore_dati[k,6]<-max(vettore_dati[k,c(1:4)])
        }
        poligoni<-cbind(poligoni,data.frame(min=vettore_dati[,5],max= vettore_dati[,6]   ))
        
        
        par(mgp=c(2.5,0.5,0))
        #graph 1-2 percentile incertezza
        plot(x=NULL,y=NULL,xlab=xld, ylab=xle,ylim=c(1,200),xlim=c(mmd,md),
             main="",cex.lab=1.5,cex.axis=1.4)  
   
        polygon(c(x_vector,rev(x_vector)),c(vettore_dati[,6],rev(vettore_dati[,5])),col=rgb(0.1,0.1,0.1,0.08),border=NA)        
        
        
        lines(x_vector,DB_value_out$mean[tk+punt_start[n_time]]*x_vector^(1-DB_value_out$mean[punt_beta[n_time]]),col="red")
        pq<-quant[tk-1]*100
        
        sa<-round(DB_value_out$mean[punt[n_time]],1)
        si<-round(DB_value_out$sigma[punt[n_time]],1)
        sg<-round((1-DB_value_out$mean[punt_beta[n_time]]),2)
        sib<-round(DB_value_out$sigma[punt_beta[n_time]],2)
        
        mylabel = bquote(alpha~ "("~ .(format(pq, digits = 2)) ~ "%)"   )
        text(md/2,215,mylabel,xpd=T,cex=1.4,adj=0)
    
        mylabel = bquote("T" ~ .(format(pq, digits = 2)) ~"%:"  ~ italic(E)== ~ "(" ~ .(format(sa, digits = 2,nsmall = 1)) ~ "?"~ .(format(si, digits = 1,nsmall = 1)) ~ ")" ~ 
                           italic(D)^( .(format(sg, digits = 3,nsmall = 1)) ~ "?" ~ .(format(sib, digits = 1,nsmall = 1)))            )
        
        
        
        text(md,1.41, mylabel , lty=1, bty="n", col="darkred",adj=1,cex=1.2)
        
        #plot2
        plot(0.001,0.001,log="xy",ylim=c(1,1000),xlab=xld, ylab=xle,xlim=c(0.5,1500),
             main="",axes=F,frame.plot = T,cex.lab=1.5,type="p", bg=rgb(0,0,1,0.2), pch=21,cex=1.2)
        # polygon(c(mmd:md,md:mmd),c(vettore_dati[1:mid,5],rev(vettore_dati[1:mid,6])),col=rgb(0.1,0.1,0.1,0.08),border=NA) 
        polygon(c(x_vector,rev(x_vector)),c(vettore_dati[,6],rev(vettore_dati[,5])),col=rgb(0.1,0.1,0.1,0.08),border=NA) 
        
        lines(x_vector,DB_value_out$mean[tk+punt_start[n_time]]*x_vector^(1-DB_value_out$mean[punt_beta[n_time]]),col="red")
        #       lines(duration,DB_value_out$mean[tk+punt_start[n_time]]*duration^(1-DB_value_out$mean[punt_beta[n_time]]),col="red")
        axis(2,c(0.01,0.1,1,10,100,1000),vet.lab.yy,cex.axis=1.3,las=1,tck=-0.01)
        axis(1,c(0.1,1,10,100,1000,10000),vet.lab.yx,cex.axis=1.3,las=1,tck=-0.01)
        

        
        maxy<-as.integer(DB_value_out$mean[punt[1]]+DB_value_out$sigma[punt[1]])
        miny<-as.integer(DB_value_out$mean[punt[1]]-DB_value_out$sigma[punt[1]])
        #plot3
        par(mgp=c(2.5,0.5,0))
        plot(x=NULL,y=NULL,xlab="Number of events,n", ylab=expression(alpha),
             ylim=c(miny,maxy),xlim=c(10,sample.number[n_time]),
             main=paste(expression(alpha)," (",quant[tk-1]*100,"%)",sep=""),cex.lab=1.5,cex.axis=1.3,axes=F,frame.plot = T)
        
        points(sample.number, DB_value_out$mean[punt], col = "black", pch = 18)
        arrows(sample.number,DB_value_out$mean[punt]-DB_value_out$sigma[punt],sample.number, DB_value_out$mean[punt]+DB_value_out$sigma[punt],
               code = 3, col = "red", angle = 90, length = .04,lty=1)
        axis(1,sample.number,cex.axis=1.3,las=1,tck=-0.01)
        axis(2,round(seq(from=miny, to=maxy, by=(maxy-miny)/10),0),cex.axis=1.3,las=1,tck=-0.01)
        
        
        
        #plot4
        par(mgp=c(2.5,0.5,0))
        x_ne = log10(sample.number)
        y_delta = log10(DB_value_out$sigma[punt])
        
        y_delta_alpha=log10(DB_value_out$sigma[punt]/DB_value_out$mean[punt])
        maxy<-round(max(10^y_delta),0)
        miny<-as.integer(min(10^y_delta))
        
        plot(10^x_ne,10^y_delta,xlab="Number of events,n", ylab=expression(Delta~alpha),col = "black",bg="red", pch = 22,cex=1.1,
             ylim=c(miny,maxy*1.4),xlim=c(10,sample.number[n_time]),
             main=paste(expression(alpha)," (",quant[tk-1]*100,"%)",sep=""),cex.lab=1.5,cex.axis=1.3,axes=F,frame.plot = T)
        axis(1,sample.number,cex.axis=1.3,las=1,tck=-0.01)
        axis(2,round(seq(from=miny, to=maxy*1.4, by=((maxy*1.4)-miny)/10),1),cex.axis=1.3,las=1,tck=-0.01)
        
        df <- data.frame(x_ne, y_delta)
        # m <- nls(y_delta ~ delta_alfa*x_ne^power, data = df, start = list(delta_alfa=1,power = 1))
        # m <- nls(y_delta ~ delta_alfa*(x_ne^power), start = list(delta_alfa=1,power = -1))
        m<-lm( y_delta~x_ne )
        kk<-lm(y_delta_alpha~x_ne)
        
        # m <- nls(y_delta ~ I(delta_alfa*x_ne^power), data = df, start = list(delta_alfa=1,power = 1))
        lines(10^x_ne, 10^(predict(m, list(x = x_ne ))), col = "blue",lty=1,lwd=1)
        RSS.p <- sum(residuals(m)^2)
        TSS <- sum((y_delta - mean(y_delta))^2)
        R_squared<- 1 - (RSS.p/TSS)
        max_sn<-sample.number[max(length(punt))]*0.7
        c1<-round(coef(m)[1],4)
        c2<-round(coef(m)[2],4)
        
        k1<-round(coef(kk)[1],4)
        k2<-round(coef(kk)[2],4)
        # 
        n_min_events<-format(as.double(10^((-1-k1)/k2)),digits=0)
        
        
        
        mylabel = bquote(Delta~alpha== .(format(10^c1, digits = 3)) ~ "n"^ .(format(c2, digits = 2))             )
  
        text(max_sn*0.9,max(10^y_delta*0.9),mylabel,xpd=T,cex=1.1,adj=0)
        mylabel = bquote(italic(R)^2 == .(format(R_squared, digits = 3)))
        text(max_sn*0.9,max(10^y_delta)*0.8,mylabel,xpd=T,cex=1.1,adj=0)
        
        mylabel=bquote(Delta~alpha/alpha <= 0.1 ~"n" >= ~ .(n_min_events))
        text(max_sn*0.9,max(10^y_delta)*0.7,mylabel,xpd=T,cex=1.1,adj=0)
        
        
        
        
      }  #*********** close for cycle on n.time
      delta_durata<-length(x_vector)-length(duration.original)
      delta_1<-l_do-17
      pol_base<-data.frame(DB_value_out[punt_beta[n_time]:punt[n_time],],row.names=NULL)
      poligoni<-cbind(data.frame(sample=c(DB_value_out[punt_beta[n_time]:punt[n_time],1],rep("",delta_1)),
                                 variable=c(as.character(DB_value_out[punt_beta[n_time]:punt[n_time],2]),rep("",delta_1)),
                                 probability=c(DB_value_out[punt_beta[n_time]:punt[n_time],3]*100,rep("",delta_1)),
                                 mean=c(DB_value_out[punt_beta[n_time]:punt[n_time],4],rep("",delta_1)),
                                 sigma=c(DB_value_out[punt_beta[n_time]:punt[n_time],5],rep("",delta_1)),
                                 min=c(DB_value_out[punt_beta[n_time]:punt[n_time],6],rep("",delta_1)),
                                 max=c(DB_value_out[punt_beta[n_time]:punt[n_time],7],rep("",delta_1)),
                                 do=c(duration.original,rep("",delta_durata)),
                                 eo=c(cumulative.original,rep("",delta_durata))),poligoni,
                      whc=c(probability_combinations,rep("",delta_durata)),cla=c(class_intensity,rep("",delta_durata))     )
      
      nome_col<-c()
      for( uy in 1:length(quant))  {
        nome_col<-c(nome_col,c(paste("min",quant[uy],sep=""),paste("max",quant[uy],sep="")))
        
      }
      colnames(poligoni)<-c("sample","variable","probability","mean","sigma","min","max","do","eo","D", nome_col,"whc","cla")
      
      
      
      #section of gamma
      plot(x=NULL,y=NULL,xlab=xld, ylab=xle,ylim=c(1,200),xlim=c(0.5,md),
           main="",cex.lab=1.5,cex.axis=1.4)  
      polygon(c(x_vector,rev(x_vector)),c(poligoni[,28],rev(c(poligoni[,27]))),col=rgb(0.9,0.1,0.1,0.1),border=NA) 
      lines(x_vector,DB_value_out$mean[10+punt_start[n_time]]*x_vector^(1-DB_value_out$mean[punt_beta[n_time]]),col="darkred")
      # 
      polygon(c(x_vector,rev(x_vector)),c(poligoni[,37],rev(c(poligoni[,38]))),col=rgb(0.1,0.9,0.1,0.1),border=NA) 
      lines(x_vector,DB_value_out$mean[15+punt_start[n_time]]*x_vector^(1-DB_value_out$mean[punt_beta[n_time]]),col="darkgreen")
      
      tk<-10
      pq<-quant[tk-1]*100
      sa<-round(DB_value_out$mean[punt[n_time]-7],1)
      si<-round(DB_value_out$sigma[punt[n_time]-7],1)
      sg<-round((1-DB_value_out$mean[punt_beta[n_time]]),2)
      sib<-round(DB_value_out$sigma[punt_beta[n_time]],2)
      
      mylabel = bquote("T" ~ .(format(pq, digits = 1)) ~"%:"  ~ italic(E)== ~ "(" ~ .(format(sa, digits = 2,nsmall = 1)) ~ "?"~ .(format(si, digits = 1,nsmall = 1)) ~ ")" ~ 
                         italic(D)^( .(format(sg, digits = 3,nsmall = 1)) ~ "?" ~ .(format(sib, digits = 1,nsmall = 1)))            )
      text(md,12, mylabel , lty=1, bty="n", col="darkred",adj=1,cex=1.2)
      
      
      tk1<-15
      pq1<-quant[tk1-1]*100
      sa1<-round(DB_value_out$mean[punt[n_time]-2],1)
      si1<-round(DB_value_out$sigma[punt[n_time]-2],1)
      sg1<-round((1-DB_value_out$mean[punt_beta[n_time]]),2)
      sib1<-round(DB_value_out$sigma[punt_beta[n_time]],2)
      mylabel1 = bquote("T" ~ .(format(pq1, digits = 1)) ~"%:"  ~ italic(E)== ~ "(" ~ .(format(sa1, digits = 2,nsmall = 1)) ~ "?"~ .(format(si1, digits = 1,nsmall = 1)) ~ ")" ~
                          italic(D)^( .(format(sg1, digits = 3,nsmall = 1)) ~ "?" ~ .(format(sib1, digits = 1,nsmall = 1)))            )
      text(md,1.41, mylabel1 , lty=1, bty="n", col="darkgreen",adj=1,cex=1.2)
      
      # 
      # 
      # # plot2
      plot(0.001,0.001,log="xy",ylim=c(1,1000),xlab=xld, ylab=xle,xlim=c(0.5,1500),
           main="",axes=F,frame.plot = T,cex.lab=1.5,type="p", bg=rgb(0,0,1,0.2), pch=21,cex=1.2)
      polygon(c(x_vector,rev(x_vector)),c(poligoni[,27],rev(c(poligoni[,28]))),col=rgb(0.9,0.1,0.1,0.1),border=NA) 
      lines(x_vector,DB_value_out$mean[10+punt_start[n_time]]*x_vector^(1-DB_value_out$mean[punt_beta[n_time]]),col="darkred")
      # 
      polygon(c(x_vector,rev(x_vector)),c(poligoni[,37],rev(c(poligoni[,38]))),col=rgb(0.1,0.9,0.1,0.1),border=NA) 
      lines(x_vector,DB_value_out$mean[15+punt_start[n_time]]*x_vector^(1-DB_value_out$mean[punt_beta[n_time]]),col="darkgreen")
      
      text(md,2.1, mylabel , lty=1, bty="n", col="darkred",adj=1,cex=1.2)
      text(md,1.41, mylabel1 , lty=1, bty="n", col="darkgreen",adj=1,cex=1.2)
      
      
      axis(2,c(0.01,0.1,1,10,100,1000),vet.lab.yy,cex.axis=1.3,las=1,tck=-0.01)
      axis(1,c(0.1,1,10,100,1000,10000),vet.lab.yx,cex.axis=1.3,las=1,tck=-0.01)
      
      maxy<-round((1-DB_value_out$mean[punt_beta[1]])+DB_value_out$sigma[punt_beta[1]],1)*1.1
      miny<-round((1-DB_value_out$mean[punt_beta[1]])-DB_value_out$sigma[punt_beta[1]],1)*0.9
      #plot3
      par(mgp=c(2.5,0.5,0))
      plot(x=NULL,y=NULL,xlab="Number of events,n", ylab=expression(gamma),
           ylim=c(miny,maxy),xlim=c(10,sample.number[n_time]),
           cex.lab=1.5,cex.axis=1.3,axes=F,frame.plot = T)
      
      points(sample.number, (1-DB_value_out$mean[punt_beta]), col = "black", pch = 18)
      arrows(sample.number,(1-(DB_value_out$mean[punt_beta]-DB_value_out$sigma[punt_beta])),
             sample.number, (1-(DB_value_out$mean[punt_beta]+DB_value_out$sigma[punt_beta])),
             code = 3, col = "forestgreen", angle = 90, length = .04,lty=1)
      axis(1,sample.number,cex.axis=1.3,las=1,tck=-0.01)
      axis(2,round(seq(from=miny, to=maxy, by=(maxy-miny)/5),1),cex.axis=1.3,las=1,tck=-0.01)
      
      
      # plot4 gamma
      par(mgp=c(2.5,0.5,0))
      x_ne = log10(sample.number)
      y_delta = log10(DB_value_out$sigma[punt_beta])
      maxy<-round(max(10^y_delta),2)*1.1
      miny<-round(min(10^y_delta),2)*0.9
      
      plot(10^x_ne,10^y_delta,xlab="Number of events,n", ylab=expression(Delta~gamma),col = "black",bg="forestgreen", pch = 22,cex=1.1,
           ylim=c(miny,maxy),xlim=c(10,sample.number[n_time]),cex.lab=1.5,cex.axis=1.3,axes=F,frame.plot = T)
      axis(1,sample.number,cex.axis=1.3,las=1,tck=-0.01)
      axis(2,round(seq(from=miny, to=maxy, by=((maxy)-miny)/5),2),cex.axis=1.3,las=1,tck=-0.01)
      
      m<-lm( y_delta~x_ne )
      
      lines(10^x_ne, 10^(predict(m, list(x = x_ne ))), col = "blue",lty=1,lwd=1)
      RSS.p <- sum(residuals(m)^2)
      TSS <- sum((y_delta - mean(y_delta))^2)
      R_squared<- 1 - (RSS.p/TSS)
      max_sn<-sample.number[max(length(punt))]*0.7
      c1<-round(coef(m)[1],4)
      c2<-round(coef(m)[2],4)
      
      mylabel = bquote(Delta~gamma== .(format(10^c1, digits = 3)) ~ "n"^ .(format(c2, digits = 2))             )
      #  text(c(max_sn*0.78,max_sn),max(y_delta),c(expression(Delta~alpha~"="),paste(c1,"n^",c2)),xpd=T,cex=1.1,adj=1)
      text(max_sn,max(10^y_delta*0.9),mylabel,xpd=T,cex=1.1,adj=0)
      mylabel = bquote(italic(R)^2 == .(format(R_squared, digits = 3)))
      
      
      text(max_sn,max(10^y_delta)*0.8,mylabel,xpd=T,cex=1.1,adj=0)
    } 
    
    
    
  }
  
  
  
  
  
  dev.off()
  boot_results<-subset(poligoni,select = variable:max)
  filename<-paste( dir_file_name_3,"/boot_",leg[th],".csv",sep="")
  write.table(  boot_results, file=filename,quote=F,row.names=F,qmethod = "double",sep=";")
  
}

#*********************************************************END BLOCK 3****************************************************






