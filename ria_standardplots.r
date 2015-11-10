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

plots<-function(plottype,plotvariable,acmoinput,title_or_group,color,title,output){
    
    #acmoinput,title_or_group,color,plottype,plotvariable,output
    #len<-length(acmoinput)
    for (thisinput in 1:length(acmoinput)){
        OriData<-read.csv(acmoinput[thisinput],skip=2)
        if(length(acmoinput)>=1) {
            group<-title_or_group[thisinput]
        }
        if (plotvariable %in% c("ADAT_S","MDAT_S","HADAT_S")){
            pdate<-as.POSIXct(paste(OriData$PDATE),format="%Y-%m-%d")
            ndate<-as.POSIXct(paste(OriData[[plotvariable]]),format="%Y-%m-%d")
            value<-as.numeric(ndate-pdate)
        } else {
            if (title_or_group[thisinput] %in% c("Observed","Observation","History",1960:2014) && plotvariable=="HWAH_S"){
                value<-OriData$HWAH
            }else if (title_or_group[thisinput] %in% c("Observed","Observation","History",1960:2014) && plotvariable=="HADAT_S"){
                pdate<-as.POSIXct(paste(OriData$PDATE),format="%Y-%m-%d")
                ndate<-as.POSIXct(paste(OriData$HDATE),format="%Y-%m-%d")
                value<-as.numeric(ndate-pdate)
            }else{
                value<-OriData[[plotvariable]]
            }
            
        }
        if (thisinput==1){dd0<-data.frame(group,value)}
        dd<-data.frame(group,value)
        dd0<-rbind(dd0,dd)
    }
    png(output,height=500,width=900)
    if (plottype=="b") {
        dd0$group<-ordered(dd0$group,levels=title_or_group)
        par(mar=c(10.1,4.1,4.1,9.1),xpd=TRUE)
        boxplot(value~group,data=dd0,ylab=name_unit(plotvariable),col=color,whiskcol="black",outline=FALSE,main=title)
        legend("topright",inset=c(-0.16,0),title_or_group,fill=color)
        #mtext(paste("4EFA:",GetCliSceName("4EFA"),"\n","4IFA:",GetCliSceName("4IFA"),"\n","4KFA:",GetCliSceName("4FKA"),"\n","4OFA:",GetCliSceName("4OFA"),"\n","4RFA:",GetCliSceName("4PFA"),"\n"),side=1,line=8)
        
    }
    if (plottype=="c") {
        r<-range(dd0$value,na.rm=TRUE)
        for (thisinput in 1:length(title_or_group)){
            if (thisinput==1) {
                ddsub<-ecdf(subset(dd0$value,dd0$group==title_or_group[thisinput]))
                curve(1-ddsub(x),from=r[1],to=r[2],col=color[1],xlim=r,main=title,ylab="Cumulative Frequency",xlab=name_unit(plotvariable))
                #ddsub<-ecdf(x)#subset(dd0$value,dd0$group==title_or_group[thisinput]))
                #plot(1-ddsub(x),reverse=TRUE,verticals=TRUE,do.p=FALSE,main=title,ylab="Cumulative Frequency",ylim=c(0,1),xlab=name_unit(plotvariable))
                #curve(1-ddsub(x),xlab=name_unit(plotvariable))
            } else {
                ddsub<-ecdf(subset(dd0$value,dd0$group==title_or_group[thisinput]))
                #ddsub<-ecdf(x)#subset(dd0$value,dd0$group==title_or_group[thisinput]))
                #color<-colorRampPalette(c("red","blue"))(length(title_or_group)-1)
                curve(1-ddsub(x),from=r[1],to=r[2],col=c(color[thisinput]),add=TRUE)
                #lines(ddsub,verticals=TRUE,do.p=FALSE,col.h=color[thisinput-1],col.v=color[thisinput-1],lty="solid")
                #curve(1-ddsub(x),xlab=name_unit(plotvariable))
            }
            
            #legend(1800,1,title_or_group, col=c(colorRampPalette(c("black"))(1),colorRampPalette(c("red","blue"))(length(title_or_group)-1)),lty="solid")
            legend(1800,1,title_or_group,col=color,lty="solid")
        }
        
    }
    
    dev.off()

}
options(echo=TRUE)
args<-commandArgs(trailingOnly=TRUE)
#print(args)
title<-args[1]
plottype<-args[2]
plotvariable<-args[3]
inputfile<-args[4]
output<-args[5]
df<-read.table(inputfile,header=T,sep="")
acmoinput<-as.character(df$csv)
title_or_group<-as.character(df$group)
color<-as.character(df$color)
plots(plottype,plotvariable,acmoinput,title_or_group,color,title,output)


