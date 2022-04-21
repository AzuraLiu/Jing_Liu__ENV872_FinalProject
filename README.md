# <Jing_Liu__ENV872_FinalProject>
By Yikai Jing & Azura Liu

## Summary

The repository is created by Yikai Jing and Azura Liu in April, 2022, to perform analysis of water supplies and withdraws’ effects on water resources capacity in Durham, North Carolina.

## Investigators

Yikai Jing & Azura Liu
EDA Spring 2022 Students

## Keywords

#Environmental Data Analysis #Water Data Analysis #r

## Database Information

Data scraped by Yikai Jing from USGS (Precipitation, Discharge: Cape Fear River, Flat River, Little River Groundwater Table)and NCDEQ (Durham Withdraw)


## Folder structure, file formats, and naming conventions 

Codes: Contains r markdown file where analysis occurs;

Info: instruction for the project;

Data: in the format of csv files; we do not have a Raw since the data was scraped;

Processed : any final or intermediate products from the analysis;

Output: contains deliverables including Powerpoint presentation and report as a Word document.

## Metadata

The three discharge datasets all contain the same information: 	
agency_cd (USGS)

site_no

Date

Discharge (ft3/s)

Approval.Code

## Scripts and code
#Regular Water Resources
CapeFearRiverDischarge <- readNWISdv(siteNumbers = "02096500",
                                  parameterCd = "00060", # discharge (ft3/s)
                                  startDate = "1990-01-01",
                                  endDate = "2021-12-31")
names(CapeFearRiverDischarge)[4:5] <- c("CapeFear_Discharge", "Approval.Code")
c(min(CapeFearRiverDischarge$Date), max(CapeFearRiverDischarge$Date))
#"1990-01-01" "2021-12-31"
CapeFearRiverDischarge_Monthly <- CapeFearRiverDischarge %>%
  mutate(Month = format(Date,"%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Mean_CapeFear_Discharge_Bymonth = mean(CapeFear_Discharge),
            River = paste("Cape Fear River"))

FlatRiverDischarge <- readNWISdv(siteNumbers = "02085500",
                                  parameterCd = "00060", # discharge (ft3/s)
                                  startDate = "1990-01-01",
                                  endDate = "2021-12-31")
names(FlatRiverDischarge)[4:5] <- c("Flat_Discharge", "Approval.Code")
c(min(FlatRiverDischarge$Date), max(FlatRiverDischarge$Date))
#"1990-01-01" "2021-12-31"
FlatRiverDischarge_Monthly <- FlatRiverDischarge %>%
  mutate(Month = format(Date,"%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Mean_Flat_Discharge_Bymonth = mean(Flat_Discharge),
            River = paste("Flat River"))

LittleRiverDischarge <- readNWISdv(siteNumbers = "0208524975",
                                  parameterCd = "00060", # discharge (ft3/s)
                                  startDate = "1990-01-01",
                                  endDate = "2021-12-31")
names(LittleRiverDischarge)[4:5] <- c("Little_Discharge", "Approval.Code")
c(min(LittleRiverDischarge$Date), max(LittleRiverDischarge$Date))
#"1995-10-24" "2021-12-31"
LittleRiverDischarge_Monthly <- LittleRiverDischarge %>%
  mutate(Month = format(Date,"%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Mean_Little_Discharge_Bymonth = mean(Little_Discharge),
            River = paste("Little River"))

#Emergency Water Resources
EnoRiverDischarge <- readNWISdv(siteNumbers = "02085070",
                                  parameterCd = "00060", # discharge (ft3/s)
                                  startDate = "1990-01-01",
                                  endDate = "2021-12-31")
names(EnoRiverDischarge)[4:5] <- c("Discharge", "Approval.Code")
c(min(EnoRiverDischarge$Date), max(EnoRiverDischarge$Date))
#"1990-01-01" "2021-12-31"
EnoRiverDischarge_Monthly <- EnoRiverDischarge %>%
  mutate(Month = format(Date,"%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Mean_Discharge_Bymonth = mean(Discharge),
            River = paste("Eno River"))
            
#Groundwater scraping
GroundParams <- whatNWISdata(siteNumbers = "355944079013401")
DurhamGroundwater <- readNWISdv(siteNumbers = "355944079013401", #Duke Forest
                                 parameterCd = "72019", 
                                # /62610/Groundwater level above NGVD 1929 (feet)
                                 statCd = "00002",
                                 startDate = "2014-01-01",
                                 endDate = "2021-12-31")
colnames(DurhamGroundwater) <- c("Agency_Name",
                                 "Site_Number",
                                 "Date",
                                 "Groundwater_Table_feet", 
                                 "Approval.Code")
                                
#Precipitation Scraping
PreciParams <- whatNWISdata(siteNumbers = "355852078572045")
DurhamPrecipitaion <- readNWISdv(siteNumbers = "355852078572045",
                                 parameterCd = "00045", 
                                 # precipitation (inches)
                                 statCd = "00006",
                                 startDate = "2009-01-01",
                                 endDate = "2021-12-31")
colnames(DurhamPrecipitaion) <- c("Agency_Name",
                                 "Site_Number",
                                 "Date",
                                 "Precipitaion_inches", 
                                 "Approval.Code")
#Withdraw Scraping
#the PSWID of Durham
durham_pswid = '03-32-010'
#years with records
the_years = c(2018:2021)

#Scrap Function
scrape.totalwithdrawal <- function(the_pswid, the_year){
the_website <- read_html(paste0('https://www.ncwater.org/WUDC/app/LWSP/report.php?pwsid=',
the_pswid, '&year=', the_year))
water_system_name_tag <- 'div+ table tr:nth-child(1) td:nth-child(2)'
ownership_tag <- 'div+ table tr:nth-child(2) td:nth-child(4)'
avg_daily_use_tag <- '.fancy-table:nth-child(31) th+ td'
water_system_name <- the_website %>% html_nodes(water_system_name_tag) %>% html_text()
ownership <- the_website %>% html_nodes(ownership_tag) %>% html_text()
avg_daily_use <- the_website %>% html_nodes(avg_daily_use_tag) %>% html_text()
df_withdrawals <- data.frame("Year" = rep(the_year,12),
"Month" = rep(1:12),
"Avg_Daily_Use_mgd" = as.numeric(avg_daily_use)) %>%
mutate(Water_System_name = !!water_system_name,
Ownership = !!ownership,
Date = my(paste(Month,"-",Year)))
  
  print(paste('The Pswid =', the_pswid, ', The Year =', the_year))
  return(df_withdrawals)
}

## Quality assurance/quality control
We obtained data from the most prestige sources we can think of, USGS and NCDEQ are both goverment organizations.
We also checked the data upon scraping by visually checking and summarize each dataset, and both team member do so to make sure the other does not miss anything.
We all perform diagnostic graphs on our models to check for potential errors and outliers.

