#This script was developed to consistently calculate the protected area extents in UK Oversea Territories.
#It was created 12/10/2020 by JNCC. 
#This script was developed using R version 3.6.1 and the following package versions:rgeos (0.5-3), rgdal (v1.5-12), raster (3.3-7), cleangeo (v0.2-3), maptools (v1.0-2), sp (v1.4-2), dplyr (v1.0-0), reshape (v0.8.8) and openxlsx (v4.1.5). 

rm(list=ls())

# First specify the required packages
packages = c("rgdal","rgeos","raster","openxlsx","dplyr","cleangeo","reshape")

# Now load or install & load all required packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Set up area calculation function 
calc_areas<- function (crs,coast_waters,PAs,PA_info,folderpath){
  
  #This first section brings in the marine and terrestrial areas shapefile, tranforms them to a custom LAEA projection and calculates the total marine and terrestrial areas
  #Outputs - Shapefile dissolved my marine/terrestrial with total areas (km2) + a dataframe of total marine/terrestrial areas.
  
  coast_waters<-spTransform(coast_waters,crs)
  coast_waters@data$Areakm2_R<-gArea(coast_waters,byid=TRUE)/1000000
  coast_waters_areas<- coast_waters@data %>% group_by(type) %>% 
		summarise(TotalArea=sum(Areakm2_R)) #Calculate total marine/terrestrial
  writeOGR(coast_waters,folderpath,paste0("c",format(Sys.time(),'%Y%m%d'),"_coast_waters_LAEA"),"ESRI Shapefile")
  areas<-data.frame("TotalArea"=as.numeric())
  areas<-rbind(areas, data.frame(coast_waters_areas))
  
  #This section brings in the protected area (PA) boundaries and spreadsheet information. It removes any PA boundaries for PAs not listed in the spreadsheet, transforms the remaining boundaries into the custom LAEA projection, intersects them with the marine/terrestrial polygons and calculates the marine/terrestrial area of each PA. 
  #Outputs - Shapefile of PAs divided into their marine/terrestrial sections with associated area values (km2) + a spreadsheet giving the marine/terrestrial areas for each PA. 
  
  PAs@data$Name<-paste0(PAs@data$Name," ",PAs@data$Desig)
  PA_info$NAME<-paste0(PA_info$NAME," ",PA_info$Desig)
  PA_data<-PAs[PAs@data$Name %in% PA_info[,"NAME"],]
  PA_data<-spTransform(PA_data,crs)
  PA_data<-aggregate(PA_data,by="Name",dissolve=T)
  PA_data<-raster::intersect(PA_data,coast_waters)
  PA_data@data$Areakm2_R<-gArea(PA_data,byid=TRUE)/1000000
  writeOGR(PA_data,folderpath,paste0("c",format(Sys.time(),'%Y%m%d'),"_All_PA_LAEA"),"ESRI Shapefile")
  Total_PA_Areas<-cast(PA_data@data,Name~type)
  write.xlsx(Total_PA_Areas,paste0(folderpath, "\\c",format(Sys.time(),'%Y%m%d'),"_","PAs_MarineTerrestrialAreas.xlsx"))
  
    #This section takes the intersected marine/terrestrial PA spatial data from the above section and removes any erroneous slithers based on information in the PA information spreadsheet. It then calculates the number of PAs in marine/terrestrial areas and the total marine/terrestrial area in all PAs. 
  #Outputs - Shapefile of PAs divided into their marine/terrestrial sections (with erroneous slithers removed) and with associated area values (km2) + a data frame of the PA count in marine/terrestrial areas and the total marine/terrestrial undissolved PA area. 
  
	PA_filtered<-PA_data[PA_data@data$type=="Marine" & PA_data@data$Name %in% PA_info[PA_info$`Marine/Terres` !="terrestrial only","NAME"]|PA_data@data$type=="Terrestrial" & PA_data@data$Name %in% PA_info[PA_info$`Marine/Terres` !="marine only","NAME"],]  	
	writeOGR(PA_filtered,folderpath,paste0("c",format(Sys.time(),'%Y%m%d'),"_PA_SlithersRemoved_LAEA"),"ESRI Shapefile")
    PA_areas<-PA_filtered@data %>% group_by(type) %>%
    summarise(PA_count=n(),Undissolved_PA_area=sum(Areakm2_R))
	all_areas<-merge(areas,data.frame(PA_areas),by="type")
  
  #This section takes the filtered (to remove slithers) PA spatial data from the above section and dissolves it into marine and terrestrial PA regions. This removes any overlaps between PAs. Final total marine/terrestrial PA extents are then calculated, as well as the PA extent % of the total marine/terrestrial area. 
  #Outputs - Shapefile of dissoved marine/terrestrial PAs regions with calculated areas + An overall statistics spreadsheet with gives the total area, number of PAs, undissolved PA area, dissolved PA area, PA percentage of total area - broken down by marine/terrestrial. 
  Dissolved<-aggregate(PA_filtered,by="type",dissolve=T)
  Dissolved@data$Areakm2_R<-gArea(Dissolved,byid=TRUE)/1000000
  Dissolved_areas<-Dissolved@data %>% group_by(type) %>%
    summarise(Dissolved_PA_area=sum(Areakm2_R))
  writeOGR(Dissolved,folderpath,paste0("c",format(Sys.time(),'%Y%m%d'),"_Dissolved_LAEA"),"ESRI Shapefile")
    all_areas<-merge(all_areas,data.frame(Dissolved_areas),by="type")
  all_areas$percent<-(all_areas$Dissolved_PA_area/all_areas$TotalArea)*100
  write.xlsx(all_areas,paste0(folderpath, "\\c",format(Sys.time(),'%Y%m%d'),"_OverallStats.xlsx"))
    return(all_areas)
}

#Preparing the data to go into the function
#Read in the custom projection
shapefile<-"Enter path to shapefile which has the custom LAEA projection"
crs <-readOGR(shapefile) %>% crs()

#Read in waters/coastline file - this defines the marine/terrestiral area
coast_waters<-readOGR("Enter path to shapfile")

#Repair any geometry issues
coast_waters<-clgeo_Clean(coast_waters)

#Ensure the dataset has an attribute called type - that defines if each polygon is marine or terrestrial. 
if("type" %in% names(coast_waters)& all(coast_waters@data$type %in% c("Marine","Terrestrial"))){
  print("Appropriate attribute data exists")
}else if("type" %in% names(coast_waters)==TRUE& all(coast_waters@data$type %in% c("Marine","Terrestrial"))==FALSE){
  print("Attribute data is incorrectly formatted - please check all values are either Marine or Terrestrial")
}else{
  print("No column called type - please add this attribute and appropriate Marine/Terestrial values")
}

#If required add attribute called 'type' with Marine/Terrestrial data values
# e.g. coast_waters@data$type<-c("Marine","Marine","Terrestrial")

#Dissolve to multipart polygons based on 'type'
coast_waters<-aggregate(coast_waters,by="type",dissolve=T)

##Read in protected area tabular information
PAinfo<-read.xlsx("Enter filepath to spreadsheet of data")

#the PA name field should be called 'NAME' and the designation type field called 'Desig' - first check these exist in the dataset
if("NAME" %in% names(PAinfo) & "Desig" %in% names(PAinfo)){
  print("Appropriate columns exists")
} else if("NAME" %in% names(PAinfo)==TRUE & "Desig" %in% names(PAinfo)==FALSE){
  print("Column Desig does not exist - you may need to create this or if appropriate rename one of the existing columns")
} else if("NAME" %in% names(PAinfo)==FALSE & "Desig" %in% names(PAinfo)==TRUE){
  print("Column NAME does not exist - you may need to create this or if appropriate rename one of the existing columns")
} else {
  print("Neither column exists - you may need to create them or if appropriate rename two of the existing columns")
}
 
#If necessary update the appropriate existing column names to NAME and Desig. Both are required to ensure differentiation between PAs with the same name. 
#names(PAinfo)[i]<-"NAME" # i is the appropriate column index
#names(PAinfo)[j]<-"Desig" # j is the appropriate column index

#There must also be a field called 'Marine/Terres' that indicates if a protected area is "marine and terrestrial". "terrestrial only" or "marine only"
if("Marine/Terres" %in% names(PAinfo)& all(PAinfo$'Marine/Terres' %in% c("marine and terrestrial", "terrestrial only","marine only"))){
  print("Appropriate attribute data exists")
}else if("Marine/Terres" %in% names(PAinfo)==TRUE & all(PAinfo$'Marine/Terres' %in% c("marine and terrestrial", "terrestrial only","marine only"))==FALSE){
  print("Attribute data is incorrectly formatted - please check all values are either 'marine and terrestrial', 'terrestrial only','marine only'")
}else{
  print("No column called Marine/Terres - please add this attribute and seek input from OT representatives on appropriate marine/terrestrial status")
}

#Read in PA boundaries
PA<-readOGR("Add shapefile filepath")

#As with the PA information the PA name field should be called 'Name' and the designation type field called 'Desig' - first check these exist in the dataset
if("Name" %in% names(PA@data) & "Desig" %in% names(PA@data)){
  print("Appropriate attributes exists")
} else if("Name" %in% names(PA@data)==TRUE & "Desig" %in% names(PA@data)==FALSE){
  print("Attribute Desig does not exist - you may need to create this or if appropriate rename one of the existing attributes")
} else if("Name" %in% names(PA@data)==FALSE & "Desig" %in% names(PA@data)==TRUE){
  print("Attribute Name does not exist - you may need to create this or if appropriate rename one of the existing attributes")
} else {
  print("Neither attribute exists - you may need to create them or if appropriate rename two of the existing attributes")
}
 
#If necessary update the appropriate existing attribute names to Name and Desig. Both are required to ensure differentiation between PAs with the same name. 
#names(PA@data)[i]<-"Name" # i is the appropriate column index
#names(PA@data)[j]<-"Desig" # j is the appropriate column index

#Repair geometries
PA<-clgeo_Clean(PA)

#Check that all the PA names+designation types in the PA info spreadsheet exist in the PA boundary information. The names must match precisely - if not these will need to be corrected. 
if(all(paste0(PAinfo$NAME, " ",PAinfo$Desig) %in% paste0(PA@data$Name," ", PA@data$Desig))){
  print("All protected area names match")
  } else {
  print("The following PA either do not exist in the shapefile or their names do not precisely match:")
  print(PAinfo[which(paste0(PAinfo$NAME, " ",PAinfo$Desig) %in% paste0(PA@data$Name," ", PA@data$Desig)==FALSE),c("NAME","Desig")])
  }

folderpath<-"Enter path to folder where you want your data to be saved"

#Run the function using OT specific data. 
areas<-calc_areas(crs,coast_waters, PA,PAinfo,folderpath)
