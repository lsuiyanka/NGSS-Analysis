required_packages = c("devtools","tidyverse","maptools","rgeos","sp","readxl","lme4")
for (i in required_packages) {
  if (i %in% rownames(installed.packages()) == FALSE) {
    install.packages(i)
  }
  else {
    NULL
  }
}

library(devtools)

##INSTALL DEVELOPMENT VERSION OF TIDYR FROM GITHUB
install_github("tidyverse/tidyr")

library(rgeos)
library(maptools)  
library(sp)
library(tidyverse)
library(tidyr)
library(readxl)
library(lme4)

sites<- read_csv("~/Downloads/aqs_sites.csv")
head(sites)

dat=read_csv("~/Downloads/annual_conc_by_monitor_1986.csv") %>%
  filter(`Metric Used`=="Observed Values" & POC==1 & `Completeness Indicator`=="Y") %>%
  filter(`Event Type`=="No Events" | `Event Type`=="Event Excluded")


##THE  FOLLOWING CODE IS TERRIBLE AND POORLY HACKED TOGETHER BUT IT WORKS
##THERE HAS TO BE A BETTER WAY TO DO THIS TASK, BUT IM NOT YET FAMILIAR

##DATA IS IN LONG FORM AND WE WANT IT IN WIDE FORM

##TASK: EXPAND MULTIPLE VALUES (STDEV AND MEAN CONCENTRATION OF POLLUTANTS TO SINGLE KEY)

dat_select<- dat %>% 
  select(`State Code`,`County Code`,`Site Num`,`Metric Used`,`Event Type`, `POC`, `Parameter Name`,`Arithmetic Mean`,`Arithmetic Standard Dev`, `Observation Count`)

dat_wide= dat_select %>% 
  pivot_wider(data = .,names_from = `Parameter Name`,values_from = c(`Arithmetic Mean`,`Arithmetic Standard Dev`, `Observation Count`))


## I TOOK THIS FROM STACKOVERFLOW. CHANGING 'NULL' TO NA IN LIST
NulltoNA<- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

##APPLY TO EVERY COLUMN AND STORE IN R DATAFRAME
datNA= map(dat_wide,NulltoNA) %>% 
  map(.,cbind) %>% 
  data.frame


##UNLIST OUR LIST INTO DATAFRAME
for(i in 1:dim(datNA)[2]){
  datNA[,i]<-unlist(datNA[,i]) 
}

##CONVERT BACK TO TIBBLE
dattbl<-datNA %>% 
  dplyr::as_tibble()


##MERGE WITH SITE INFO
colnames(dattbl)[1:5]<-c('State Code','County Code','Site Number',"Metric Used","Event Type")

dattbl_merge<- dplyr::left_join(x=dattbl,y=sites)

write_csv(dattbl_merge,"EPA_1986_Monitoring_Expanded.csv")




##NO LONGER NEEDED FREE UP MEMORY
rm(dat)
rm(sites)

##EXTRACT ZIPCODES (I TOOK THIS FROM STACKOVERFLOW)
ztca <- readShapePoly("Downloads/tl_2010_us_zcta500/tl_2010_us_zcta500.shp", verbose=TRUE)

latlong= dattbl_merge %>% 
  select(Latitude,Longitude,`State Code`,`County Code`,`Site Number`)
latlongdf=data.frame(latlong)
longlatdf=latlongdf[,c(2,1)]

pts<- SpatialPoints(as.matrix(longlatdf[,1:2]))

zipextract<- pts %over% ztca

zipextract$Latitude<- latlongdf$Latitude
zipextract$Longitude<- latlongdf$Longitude

ziptbl<-as_tibble(zipextract)

zip_all=cbind(ziptbl,latlong[,3:5])
zip_all %>% left_join(latlong)
ziptbl_all<- as_tibble(zip_all)

write_csv(ziptbl_all,path = "Zipcodes_extracted_1986_monitoring_sites")


ziplevel_tbl<-dattbl_merge %>% 
  left_join(x=., ziptbl_all)

write_csv(ziplevel_tbl,"Zipannotated_Annual_pollutants_1986.csv")



##TAKING TO MUCH MEMORY REMOVE THINGS OUT OF THE ENVIRONMENT
rm(ztca)
rm(pts)
remove_list1<-ls(pattern = "dat")
rm(list=remove_list1)
rm(remove_list1)


NGS<-read_csv("national-geographic-smell-survey/NGS.csv")

head(NGS)


##FILTER ONLY US PARTICPANTS
US_NGS<- NGS %>% filter(COUNTRY==84) %>% filter(ZIP != "0" & SEX != "0" & SMOKE != "0")%>% filter( ETHNIC!="0" & ETHNIC !="7")
ziplevel_tbl$ZIP<-as.character(ziplevel_tbl$ZCTA5CE00)
ziplevel_tbl$ZIP<- zipcode::clean.zipcodes(ziplevel_tbl$ZIP)
US_NGS$ZIP<-zipcode::clean.zipcodes(US_NGS$ZIP)
NGS_EPA_MERGE=US_NGS %>% inner_join(x=.,y=ziplevel_tbl)


codebook<-read_excel("national-geographic-smell-survey/Data dictionary.xlsx")
codebook2=codebook %>% pivot_longer(cols=c(VALUES:87),values_drop_na = TRUE)
codebook2$name="VALUES"
codebook3<-codebook2 %>% separate(col=value,into=c("value","key"),sep="=")
codebook4<-codebook3 %>% nest(value,key)

strings=c("totcorr","a1a6","MEM","INT","QUAL","SELF_RATE")
index=c()
for(i in strings){
  query=grep(pattern = i,codebook4$VARNAME)
  index=c(index,query)
}

codebook5<-(codebook4[-c(index),])


applyfactors=function(x="data",codebook="codebook"){
  n=names(x)
  x=data.frame(x)
  for(i in seq(1,length(n))){
    for(j in seq(1,dim(codebook)[1])){
      if(isTRUE(n[i]==codebook$VARNAME[j])==TRUE){
        x[,i]<-factor(as.numeric(x[,i]))
      }
      else{
        NULL
      }
    }
  }
  return(x)
}


NGS_FACTOR=applyfactors(NGS_EPA_MERGE,codebook5)

NGS_FACTOR %>% tibble(select(Arithmetic.Mean_Suspended.particulate..TSP.:Arithmetic.Mean_Visibility))
var(NGS_FACTOR$totcorr)


AND_INT_MODEL<-summary(lm(as.numeric(AND_INT) ~ SEX + ETHNIC + AGE + WORK_ENVIOR + Arithmetic.Mean_Suspended.particulate..TSP.,data=NGS_FACTOR))
AMY_INT_MODEL<-summary(lm(as.numeric(AA_INT) ~ SEX + ETHNIC + AGE + WORK_ENVIOR + Arithmetic.Mean_Suspended.particulate..TSP.,data=NGS_FACTOR))
GALAX_INT_MODEL<-summary(lm(as.numeric(GAL_INT) ~ SEX + ETHNIC + AGE + WORK_ENVIOR + Arithmetic.Mean_Suspended.particulate..TSP.,data=NGS_FACTOR))
ROSE_INT_MODEL<-summary(lm(as.numeric(ROSE_INT) ~ SEX + ETHNIC + AGE + WORK_ENVIOR + Arithmetic.Mean_Suspended.particulate..TSP.,data=NGS_FACTOR))
MERCAP_INT_MODEL<-summary(lm(as.numeric(MER_INT) ~ SEX + ETHNIC + AGE + WORK_ENVIOR + Arithmetic.Mean_Suspended.particulate..TSP.,data=NGS_FACTOR))
EUG_INT_MODEL<-summary(lm(as.numeric(EUG_INT) ~ SEX + ETHNIC + AGE + WORK_ENVIOR + Arithmetic.Mean_Suspended.particulate..TSP.,data=NGS_FACTOR))


AND_INT_MODEL
AMY_INT_MODEL
GALAX_INT_MODEL
ROSE_INT_MODEL
MERCAP_INT_MODEL
EUG_INT_MODEL