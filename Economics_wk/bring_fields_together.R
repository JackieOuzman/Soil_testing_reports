library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(readr)
library(DT)
library(dplyr)
##########################################################################################################################################
## step 1 bring in the t.test data 
#1. simple comparision of GSP vs other rates
##########################################################################################################################################

GS_rates_3a <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/t_test_merged_3a.csv")

##########################################################################################################################################
# 2. Add rainfall data
##########################################################################################################################################

rainfall_fert <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_steps.csv")
str(rainfall_fert)

rainfall_fert <- rainfall_fert %>%
  dplyr::select(
    Paddock_ID,
    Rate,
    GSP	,
    Strip_Rate,
    organisati,
    contact	,
    farmer	,
    paddock,
    Strip_Type,
    av_rain,
    Total_sum_N_content,
    Total_sum_P_content
  )

str(GS_rates_3a$Zone_ID) #This is the zone code with 6 digits
str(rainfall_fert$Paddock_ID) #this is the paddock code with 5 digits
#because Harm changed the paddock codes part way through the projcet I need some extra chceked when joing paddock code to zone.
#are details and strip rate the same??

#make a field for joining in both df.
str(GS_rates_3a)
GS_rates_3a$Strip_Type <- sub("^[^_]*_", "", GS_rates_3a$paddock_ID_Type)


# I cant use a paddock code 5 digits for everything because zone code are now a mix of 6 and 7 digits
#What paddocks have 7 digit codes?

GS_rates_3a$length_zoneID <- nchar(GS_rates_3a$Zone_ID)
#remove last value in string
GS_rates_3a <- GS_rates_3a %>% 
  mutate(paddock_code =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))


GS_rates_3a <- GS_rates_3a %>% 
  dplyr::mutate(fld_for_join = paste0(paddock_code,"_", Details,"_",Strip_Type))

### need to remove the Alt GPS from the rainfall data                
#names(rainfall_fert)
#unique(rainfall_fert$GSP)
#str(rainfall_fert$GSP)
rainfall_fert$GSP <- as.character(rainfall_fert$GSP)
rainfall_fert <- rainfall_fert %>%   
  filter(is.na(GSP) | GSP == "GSP")
  



  rainfall_fert <- rainfall_fert %>% 
  dplyr::mutate(fld_for_join = paste0(Paddock_ID,"_", Strip_Rate, "_",Strip_Type))

  str(GS_rates_3a) 
str(rainfall_fert)
##########################################################################################################################################
#3.  join togther 
##########################################################################################################################################

#not sure what is happening here the joined df is larger than 3a - but it should be the same??
GS_rates_3a_plus_rain_fert_content <- left_join(GS_rates_3a, rainfall_fert, by = "fld_for_join")



##########################################################################################################################################
#4. Bring in Sean DB
##########################################################################################################################################

recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_May05 2021.xlsx")

# select only a few clms with recommedation 
recom_rateDB <- recom_rateDB %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`,
                SM_comment_Soil_N = `SM comment Soil N`,
                SM_comment_Soil_P = `SM comment Soil P`,
                SM_comment_Plant_Tissue = `SM comment Plant Tissue`
  ) 

recom_rateDB <-  dplyr::mutate(recom_rateDB,  maxN = apply(recom_rateDB[3:5], 1, max, na.rm = TRUE))


# remove redunant clm and replace inf
recom_rateDB <- recom_rateDB %>% 
  mutate(
    maxN = case_when(
      maxN >= 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )


recom_rateDB <-recom_rateDB %>% 
  dplyr::select(-n_rec_yld_low,
                -n_rec_yld_med,
                -n_rec_yld_high)



##########################################################################################################################################
#4. join Sean stuff to GS_rates_3a_plus...
##########################################################################################################################################
str(GS_rates_3a_plus_rain_fert_content$Zone_ID) # use Zone_ID 
str(recom_rateDB$Zone_ID) # use Zone_ID 
recom_rateDB$Zone_ID <- as.double(recom_rateDB$Zone_ID)

GS_rates_rain_fert_rec <- left_join(GS_rates_3a_plus_rain_fert_content, recom_rateDB)
str(GS_rates_rain_fert_rec)

 GS_rates_rain_fert_rec <- dplyr::select(GS_rates_rain_fert_rec,
-Rate.y, - X.1 , -X, - Strip_Type.y)
                                          

GS_rates_rain_fert_rec <- dplyr::rename(GS_rates_rain_fert_rec,
                                          Rate = Rate.x,
                                          Strip_Type = Strip_Type.x)


## make new clm recommdation from soil test

GS_rates_rain_fert_rec <- GS_rates_rain_fert_rec %>% 
  dplyr::mutate(soil_test_indicates = case_when(
    Strip_Type == 	"P Strip" & p_rec > 5 ~ "respose likely",
    Strip_Type == 	"P Strip" & p_rec <= 5 ~ "respose unlikely",
    Strip_Type == 	"N Strip" & maxN > 0 ~ "respose likely",
    Strip_Type == 	"N Strip" & maxN <= 0 ~ "respose unlikely",
    TRUE ~ "NA"
  ))

# just keep the joined data.
rm(list = ls()[!ls() %in% c("GS_rates_rain_fert_rec")])
##########################################################################################################################################
#5. join approx recom rate labels - this will be diffiucult
##########################################################################################################################################
rec_rate_approx_P <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/rec_rate_low_high_comparision_t_test_merged_3e.csv")
  
names(rec_rate_approx_P)                            
rec_rate_approx_P <- dplyr::select(rec_rate_approx_P,
                                  Zone_ID, rec_rate_p, Strip_Type, rec_rate_p_label)
                                   
rec_rate_approx_N <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/rec_rate_low_high_comparision_t_test_merged_3e_N.csv")
names(rec_rate_approx_N)                                 
rec_rate_approx_N <- dplyr::select(rec_rate_approx_N,
                                   Zone_ID, rec_rate_n, Strip_Type, rec_rate_n_label)
#change the name so I have a clm called rec rates

rec_rate_approx_P <- rec_rate_approx_P %>%
  rename(rec_rate = rec_rate_p,
         rec_rate_appox = rec_rate_p_label)


rec_rate_approx_N <- rec_rate_approx_N %>%
  rename(rec_rate = rec_rate_n,
         rec_rate_appox = rec_rate_n_label)


# append the rec rates into one df
rec_rate_approx_N_P <- rbind(rec_rate_approx_P, rec_rate_approx_N)

rm(rec_rate_approx_N, rec_rate_approx_P)

###########################################################################################################
# create a field that I is unquie to join the two data frames togther.
names(rec_rate_approx_N_P)
rec_rate_approx_N_P <- rec_rate_approx_N_P %>% 
  mutate(Fld_Join_Approx1 = paste0(Zone_ID, Strip_Type),
         Fld_Join_Approx2 = paste0(Zone_ID, Strip_Type, rec_rate_appox))

names(GS_rates_rain_fert_rec)
GS_rates_rain_fert_rec <- GS_rates_rain_fert_rec %>% 
  mutate(Fld_Join_Approx1 = paste0(Zone_ID, Strip_Type),
         Fld_Join_Approx2 = paste0(Zone_ID, Strip_Type, Details)) 
### join on this new field

# the remov rate from my work has decmile places best to use Sean...
names(rec_rate_approx_N_P)
rec_rate_approx_N_P <- rec_rate_approx_N_P %>%  dplyr::select(-rec_rate)
df <- left_join(GS_rates_rain_fert_rec, rec_rate_approx_N_P, by = "Fld_Join_Approx1")

names(df)

df <- df %>% 
  dplyr::select(Zone_ID =Zone_ID.x,
                Rate,
                GSP,
                Strip_Type = Strip_Type.x,
                Details,
                P_content = Total_sum_P_content,
                N_content = Total_sum_N_content,
                p_rec,
                n_rec = maxN,
                rec_rate_appox,
                Fld_Join_Approx1,
                yield,
                av_rain,
                Significant_practical,
                organisati,             
                contact,
                farmer,
                paddock)
#########################################################################################################
### make a clm that is has P content and N content in same clm called P_N_content.

df <- df %>% 
  mutate(
    N_P_content = case_when(
    Strip_Type == "P Strip" ~ P_content,
    Strip_Type == "N Strip" ~ N_content
  ))
names(df)


# check <- df %>% 
#  dplyr::select(Zone_ID ,
#                Rate,
#                GSP,
#                Strip_Type ,
#                P_content,
#                N_content,N_P_content)
names(df)
#str(df$Details)
#str(df$rec_rate_appox)

df$Details <- as.character(df$Details)
df$rec_rate_appox <- as.character(df$rec_rate_appox)

df <-  df %>% 
  mutate(
    rec_rate_label = case_when(
      rec_rate_appox == Details ~ "closest_match"
    ))

df <-  df %>% 
  mutate(
    rec_rate_appox_value = case_when(
      rec_rate_appox == Details ~ N_P_content
    ))


## let output this and check it with some stuff....
write.csv(df, "W:/value_soil_testing_prj/Economics/2020/draft_v1.csv")
