##############################################################################
#
#Plot Multi Acmo files to boxplot and cdfplot
#
#Created by Wei Xiong, University of Florida.  10/10/2014
#############################################################################

name_unit<-function(inputcode){
    name<-c("ID","Name of experiment", "Field Overlay","Seaonal Strategy","Rotational Analysis","","Treatment Name","Climate ID code","Climate replication number",	"Region ID","Regional stratum identification number","RAP ID", "Management regimen ID","Names of institutions","Crop rotation", "Weather station ID","Soil ID", "Site Latitude", "Site Longitude",	"Crop type", "Crop model-specific cultivar ID", "Cultivar name", "Start of simulation date",	"Planting date","Observed harvested yield, dry weight", "Observed total above-ground biomass at harvest",	"Observed harvest date",	"Total number of irrigation events",	"Total amount of irrigation",	"Type of irrigation application",	"Total number of fertilizer applications",	"Total N applied",	"Total P applied",	"Total K applied",	"Manure and applied oganic matter",	"Total number of tillage applications",	"Tillage type (hand, animal or mechanized)",	"Experiment ID",	"Weather ID",	"Soil ID",	"DOME ID for Overlay",	"DOME ID for Seasonal",  "DOME ID for Rotational", "Short name of crop model used for simulations",	"Model name and version number", "Simulated harvest yield, dry matter", "Simulated above-ground biomass at harvest, dry matter",	"Simulated anthesis date",	"Simulated maturity date",	"Simulated harvest date",	"Simulated leaf area index, maximum",	"Total precipitation from planting to harvest",	"Simulated evapotranspiration, planting to harvest",	"Simulated N uptake during season", "Simulated N leached up to harvest maturity")
    unit<-c("text",	"text",	"text",	"text",	"text",	"number",	"text",	"code",	"number",	"code",	"number",	"code",	"code",	"text",	"number",	"text",	"text",	"decimal degrees",	"decimal degrees",	"text",	"text",	"text",	"yyyy-mm-dd",	"yyyy-mm-dd",	"kg/ha",	"kg/ha",	"yyyy-mm-dd",	"number",	"mm",	"text",	"number",	"kg[N]/ha",	"kg[P]/ha",	"kg[K]/ha",	"kg/ha",	"#",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"kg/ha",	"kg/ha",	"das",	"das",	"das",	"m2/m2",	"mm",	"mm",	"kg/ha",	"kg/ha")
    code<-c("SUITE_ID",	"EXNAME",	"FIELD_OVERLAY",	"SEASONAL_STRATEGY",	"ROTATIONAL_ANALYSIS",	"RUN#",	"TRT_NAME",	"CLIM_ID",	"CLIM_REP",	"REG_ID",	"STRATUM",	"RAP_ID",	"MAN_ID",	"INSTITUTION",	"ROTATION",	"WST_ID",	"SOIL_ID",	"FL_LAT",	"FL_LONG",	"CRID_text",	"CUL_ID",	"CUL_NAME",	"SDAT",	"PDATE",	"HWAH",	"CWAH",	"HDATE",	"IR#C",	"IR_TOT",	"IROP_text",	"FE_#",	"FEN_TOT",	"FEP_TOT",	"FEK_TOT",	"OM_TOT","TI_#",	"TIIMP_text",	"EID",	"WID",	"SID",	"DOID",	"DSID",	"DRID",	"CROP_MODEL",	"MODEL_VER",	"HWAH_S",	"CWAH_S",	"ADAT_S",	"MDAT_S",	"HADAT_S",	"LAIX_S",	"PRCP_S",	"ETCP_S",	"NUCM_S",	"NLCM_S")
    for (thisi in 1:length(code)) {
        if (inputcode==code[thisi]) {
            all<-paste(name[thisi],"(",unit[thisi],")")
            break
        }
    }
    return(all)
}

plots<-function(title,plottype,plotformat,plotvariable,inputdir,group,count,pngoutput,output_acmo){
    #acmoinput,title_or_group,color,plottype,plotvariable,output
    #len<-length(acmoinput)
    for (thiscount in 1:count){
        csvfiles<-list.files(paste(inputdir,"/",thiscount,sep=""),pattern="\\.csv$")
    for (thisinput in 1:length(csvfiles)){
        if (thisinput==1 && thiscount==1){
            openfile<-paste(inputdir,"/",thiscount,"/",csvfiles[thisinput],sep="")
            cat(readLines(openfile),file=output_acmo,sep="\n")
            oridata<-read.csv(openfile,header=T,skip=2,sep=",")
            df<-data.frame(CROP_MODEL=oridata$CROP_MODEL,CLIM_ID=oridata$CLIM_ID,GROUP=oridata[group],VALUE=oridata[plotvariable])
            df$CLIM_ID<-sub("^([^.]*).*","\\1",csvfiles[thisinput])
        }else{
            openfile<-paste(inputdir,"/",thiscount,"/",csvfiles[thisinput],sep="")
            temp<-readLines(openfile)
            for (i in 1:3) temp<-temp[-1]
            cat(temp,file=output_acmo,sep="\n",append=TRUE)
            oridata<-read.csv(openfile,header=T,skip=2,sep=",")
            dftemp<-data.frame(CROP_MODEL=oridata$CROP_MODEL,CLIM_ID=oridata$CLIM_ID,GROUP=oridata[group],VALUE=oridata[plotvariable])
            dftemp$CLIM_ID<-sub("^([^.]*).*","\\1",csvfiles[thisinput])
            df<-rbind(df,dftemp)
        }
    }
    }
    colnames(df)<-c("CROP_MODEL","CLIM_ID","GROUP","VALUE")
    temp<-NA
    title<-paste(title, " of ",df[1,3],sep="")
    
    #Name_Model<-unique(df$CROP_MODEL)
    #Name_Clim<-unique(df$CLIM_ID)
    #if (length(Name_Model)==1) tick<-c(1:length(Name_Clim))
    #if (length(Name_Model)==2) tick<-c(1:length(Name_Clim),seq(length(Name_Clim)+2,length(Name_Clim)*2+2,1))
    #if (length(Name_Model)==3) tick<-c(1:length(Name_Clim),seq(length(Name_Clim)+2,length(Name_Clim)*2+2,1),seq(length(Name_Clim)*2+4,length(Name_Clim)*3+4,1))
    
    #if (plotformat=="png") png(pngoutput)#,width=850,height=500)
    #if (plotformat=="pdf") pdf(pngoutput)#,width=9,height=5)
    #if (plotformat=="tiff") tiff(pngoutput)#,width=850,height=500)
    #if (plotformat=="jpeg") jpeg(pngoutput)#,width=850,height=500)
    pdf(pngoutput)
    library(ggplot2)
   
    if (plottype=="b") {
        df$CLIM_ID<-factor(df$CLIM_ID)
        p<-ggplot(df,aes(x=CLIM_ID,y=VALUE,color=CROP_MODEL)) +
           geom_boxplot(outlier.shape=NA) +
           xlab("GCMs") +
           ylab(name_unit(plotvariable)) +
           ggtitle(title)
        print(p)
        
    }
    if (plottype=="c") {
         df$CROP_MODEL<-factor(df$CROP_MODEL)
         p<-ggplot(df,aes(VALUE,color=CLIM_ID))+
            stat_ecdf()+
            facet_wrap(~CROP_MODEL)+
            xlab(name_unit(plotvariable))+
            ylab("Cumulative Frequency")
            ggtitle(title)
         print(p)
    }
    
    dev.off()

}
options(echo=TRUE)
args<-commandArgs(trailingOnly=TRUE)
#print(args)
title<-"Result"#args[1]
plottype<-"b"#args[2]
plotformat<-"pdf"#args[3]
plotvariable<-"HWAH_S"#args[4]
inputdir<-"/Users/weixiong/Development/face-it/RIA/ria_standardplots/1.0.5_either input zip or csv"#args[5]
group<-"REG_ID"#args[6]
count<-"2"#args[7]
pngoutput<-"2.pdf"#args[8]
output_acmo<-"2.csv"#args[9]
plots(title,plottype,plotformat,plotvariable,inputdir,group,count,pngoutput,output_acmo)


