library(sp)
library(rgdal)
library(GISTools)
library(splancs)
#library(RColorBrewer)
#library(gridExtra)
#library(lattice)
#library(maps)
#library(maptools)
#library(rgeos)
#library(png)
#library(grid)
#library(Cairo)
#library(data.table)
#library(gridExtra)


wd = "/home/aiddata/Desktop/R_Repo/Nepal_Uncertainty/"
setwd(wd) 

#Lat/Lon extents for maps (for easy reference)
#(w,e)
w_e = c(80,88.25)

#(s,n)
s_n=c(26,30.5)

#Number of iterations for the Monte Carlo
iterations = 1

#Buffer (in km) for points that are attributed to regions - i.e., state parks.
buffer = 50

#Sector
sector = "AGRICULTURE"

#Precision Code Shape Files
#For precision codes that require a shape file to join to,
#Define the file paths to those shape files.

PC1_shapefile = "/home/aiddata/Desktop/R_Repo/Nepal_Uncertainty/Input/Nepal_AidData/GIS/NP_ADM4.shp"

PC2_shapefile = "/home/aiddata/Desktop/R_Repo/Nepal_Uncertainty/Input/Nepal_AidData/GIS/NP_ADM3.shp"

PC3_shapefile = "/home/aiddata/Desktop/R_Repo/Nepal_Uncertainty/Input/Nepal_AidData/GIS/NP_ADM2.shp"

PC4_shapefile = "/home/aiddata/Desktop/R_Repo/Nepal_Uncertainty/Input/Nepal_AidData/GIS/NP_ADM1.shp"

PC_Country_shapefile = "/home/aiddata/Desktop/R_Repo/Nepal_Uncertainty/Input/Nepal_AidData/GIS/NP_ADM0.shp"



PC1_geo = readShapePoly(PC1_shapefile)
PC2_geo = readShapePoly(PC2_shapefile)
PC3_geo = readShapePoly(PC3_shapefile)
PC4_geo = readShapePoly(PC4_shapefile)
PC_Country_geo = readShapePoly(PC_Country_shapefile)
#Maximum number of points to disburse projects across
MaxPrj = 50


Over_Rnd <- function(PC_geo,iter_geo,row_dollars,PC)
{
  if(PC == 1)
  {
    PC_Overlay = as.character(over(iter_geo,PC_geo)$V_ID)
    PC_poly = PC_geo[which(PC_geo$V_ID == PC_Overlay),]
  }
  if(PC == 2)
  {
    PC_Overlay = as.character(over(iter_geo,PC_geo)$D_ID)
    PC_poly = PC_geo[which(PC_geo$D_ID == PC_Overlay),]
  }
  if(PC == 3)
  {
    PC_Overlay = as.character(over(iter_geo,PC_geo)$Z_ID)
    PC_poly = PC_geo[which(PC_geo$Z_ID == PC_Overlay),]
  }
  if(PC == 4)
  {
    PC_Overlay = as.character(over(iter_geo,PC_geo)$R_ID)
    PC_poly = PC_geo[which(PC_geo$R_ID == PC_Overlay),]
  }
  if(PC == 0)
  {
    PC_poly = PC_geo
  }
  #Generate random points
  RndPrjCount = sample(2:MaxPrj,1)
  PC_csr_Poly = slot(slot(slot(PC_poly,"polygons")[[1]],"Polygons")[[1]],"coords")
  PC_rnd_Points = csr(PC_csr_Poly,RndPrjCount)
  
  row_rnd_tot = 0
  row_dollar_rnd = vector()
  for(l in 1:nrow(PC_rnd_Points))
  {
    row_dollar_rnd[l] = runif(1,0,10)
    row_rnd_tot = row_rnd_tot + row_dollar_rnd[l]
  }
  
  poly_return = cbind(PC_rnd_Points,0)
  for(l in 1:nrow(PC_rnd_Points))
  {
    poly_return[l,][3] = row_dollars * (row_dollar_rnd[l] /  row_rnd_tot)
  }
  
  return(poly_return)
}
GIS_csv = file.path("Input/Nepal_AidData","NPL_geocoded_projectLocations.csv")
path = normalizePath(GIS_csv)
GIS_raw_data = read.csv(path,stringsAsFactors = FALSE)

AMP_data_csv = file.path("Input/Nepal_AidData","NPL_AMP_projects.csv")
AMP_path = normalizePath(AMP_data_csv)
AMP_raw_data = read.csv(AMP_path,stringsAsFactors = FALSE)

GIS_AMP_database = merge(GIS_raw_data, AMP_raw_data, by="project_ID")

Geo_db <- GIS_AMP_database
Geo_db <- Geo_db[rowSums(is.na(Geo_db[,c("latitude","longitude")]))==0,]

Geo_db = SpatialPointsDataFrame(list(Geo_db$longitude,Geo_db$latitude), Geo_db)

#Includes all code to produce agriculture-only dataset from the full dataset.
agex <- AMP_raw_data[grep(sector, AMP_raw_data$amp_sector_name),]

total_aid = sum(agex$total_c_to_2012)

geo_agex <- Geo_db[grep(sector, Geo_db@data$amp_sector_name),]





for (MC_cnt in 1:iterations)
{
  country_scale_aid_to_distribute = 0
  
  country_scale_undistributed_project_count = 0
  j_geo = data.frame()
  for (i in 1:nrow(agex))
  {
    #For projects that are not geocoded, track them so we can distribute nation-wide later.
    #Note in all cases we track both the # of projects (for "probability of 
    #project occurence") as well as the total $ of aid.
    #Further, we need to have a minimum # of projects to randomly distribute to at the country level.
    
    
    #If not geocoded, add it into our big bucket of aid (but not if it is cancelled).
    #Has to be handled as a special case, as we don't have geocoded rows to iterate through.
    if(agex["geocoded"][[1]][i] == 0 && agex["status"][[1]][i] != "Cancelled")
    {
      country_scale_aid_to_distribute = country_scale_aid_to_distribute + agex["total_c_to_2012"][[1]][i]
      country_scale_undistributed_project_count = country_scale_undistributed_project_count + 1
    }
    
    
    #For projects that are geocoded, 
    else
    {
      #Count the total number of geocoded instances - i.e., if it's represented by multiple points.
      sum(geo_agex@data["project_ID"] == agex["project_ID"][[1]][[i]])
      
      #Total funding for this project
      total_funds_i = agex["total_c_to_2012"][[1]][i]
      
      #Iterate through each geocoded element that belongs in this project.
      iter_geo_agex = geo_agex[which(geo_agex$project_ID == agex["project_ID"][[1]][[i]]),]
      
      #Initial loop to randomly allocate dollar amounts to rows in a project.
      proj_dollar_rnd = vector()
      proj_dollar_row = vector()
      tot_rnd = 0
      for(k in 1:nrow(iter_geo_agex))
      {
        proj_dollar_rnd[k] = runif(1,0,10)
        tot_rnd = tot_rnd + proj_dollar_rnd[k]
      }
      
      for(k in 1:nrow(iter_geo_agex))
      {
        proj_dollar_row[k] = (proj_dollar_rnd[k]/tot_rnd) * total_funds_i
      }
      
      for (k in 1:nrow(iter_geo_agex))
      {
        #Precision code for this project location.
        PC_k = iter_geo_agex["precision_code"][[1]][k]
        
        #If the precision code is 1, we aggregate to GADM-4
        #This is done in all cases - even if we "know more precise information"
        #As we do not have spatial data on the location of more precise points - 
        #i.e., we don't know urban extents.          
        
        if(PC_k == 1)
        {
          j_geo = rbind(j_geo,Over_Rnd(PC1_geo,iter_geo_agex[k,],proj_dollar_row[k],1))
        }
        
        if(PC_k == 2)
        {
          j_geo = rbind(j_geo,Over_Rnd(PC2_geo,iter_geo_agex[k,],proj_dollar_row[k],2))
        }
        if(PC_k ==3)
        {
          j_geo = rbind(j_geo,Over_Rnd(PC3_geo,iter_geo_agex[k,],proj_dollar_row[k],3))
        }
        if(PC_k == 4)
        {
          j_geo = rbind(j_geo,Over_Rnd(PC4_geo,iter_geo_agex[k,],proj_dollar_row[k],4))
        }
        
        if(PC_k == 5)
        {
          #Right now, PC 5 is being treated the same as 3 ("zones"). Sub-optimal in the long term.
          j_geo = rbind(j_geo,Over_Rnd(PC3_geo,iter_geo_agex[k,],proj_dollar_row[k],3))
        }
        
        if((PC_k == 6) || (PC_k == 8))
        {
          #Add to our "country" bucket
          country_scale_aid_to_distribute = country_scale_aid_to_distribute + agex["total_c_to_2012"][[1]][i]
          country_scale_undistributed_project_count = country_scale_undistributed_project_count + 1
        }
        
      }
      
    }
    
  }
  
  #Finally, before we add to a CSV we do one final add for country-wide data
  #Using our country poly.
  j_geo = rbind(j_geo,Over_Rnd(PC_Country_geo,"NA",country_scale_aid_to_distribute,0))
  
  
  #Output this realization to a CSV for later processing
  f_name = paste(round(as.numeric(Sys.time()),0), "_dollars.csv",sep="")
  write.table(j_geo,file=f_name,sep=",",col.names=NA,qmethod="double")
}