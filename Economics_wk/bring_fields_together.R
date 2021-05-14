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

GS_rates_3a <- GS_rates_3a %>% 
  dplyr::mutate(paddock5_digits = substr(Zone_ID, start = 1, stop = 5),
                Strip_Type = substr(paddock_ID_Type, start = 7, stop = 13),
                fld_for_join = paste0(paddock5_digits,"_", Details,"_",Strip_Type))

                  
                
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


##########################################################################################################################################
#5. join approx recom rate labels - this will be diffiucult
##########################################################################################################################################
rec_rate_approx_P <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/rec_rate_low_high_comparision_t_test_merged_3e.csv")
  
names(rec_rate_approx_P)                            
rec_rate_approx_P <- dplyr::select(rec_rate_approx_P,
                                  Zone_ID, rec_rate_p, Strip_Type)
                                   
rec_rate_approx_N <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/rec_rate_low_high_comparision_t_test_merged_3e_N.csv")
                                  
rec_rate_approx_N <- dplyr::select(rec_rate_approx_N,
                                   Zone_ID, rec_rate_n, Strip_Type)
