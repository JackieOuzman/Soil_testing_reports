
library(tidyverse)
library(ggplot2)

library(formattable)
library(sf)
library(readxl)
## the aim is to get this table more like what rick outlined.

# bring in the yield results and t test for the comparision of interest
high_low_comp_t <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/hign_low_t_test_merged_3b.csv")


View(high_low_comp_t)
# filter the data to one comparision - high vs low

high_v_low_comp_results <- high_low_comp_t %>% 
  filter(comparison == "high_v_low") %>%  
  dplyr::select('Zone ID' = Zone_ID,
                Strip_Type,
                
                `yield with high fert` = high,
                `yield with low fert` = low,
                #medium,
                
                
                `Mean yield difference` = high_vs_low,         
                #high_vs_medium,
                #medium_vs_low, 
                
                #`Mean yield difference` = rounded, 
                "yield response" = yld_response,
                `Significant` = Significant_practical) 
high_v_low_comp_results <- high_v_low_comp_results %>% 
  arrange(`Zone ID`)


### round the values so the tabel looks better
high_v_low_comp_results <- high_v_low_comp_results %>% 
  mutate( `yield with high fert` = round(`yield with high fert`, digits = 2),
          `yield with low fert` = round(`yield with low fert`, digits = 2),
          `Mean yield difference` = round(`Mean yield difference`, digits = 2))


##### This table has some key bits that we need... but we also need info from Sean DB


recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_April7 2021.xlsx")
##########################################################################################################################################

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

# join Sean results with the comparision
 
high_v_low_comp_results_Sean <- left_join(high_v_low_comp_results,recom_rateDB,
                          by = c(`Zone ID` = "Zone_ID" ))

names(high_v_low_comp_results_Sean)
high_v_low_comp_results_Sean <-high_v_low_comp_results_Sean %>% 
  dplyr::select(-n_rec_yld_low,
                -n_rec_yld_med,
                -n_rec_yld_high)

## make new clm recommdation from soil test

high_v_low_comp_results_Sean <- high_v_low_comp_results_Sean %>% 
  mutate(soil_test_indicates = case_when(
    Strip_Type == 	"P Strip" & p_rec > 0 ~ "respose likely",
    Strip_Type == 	"P Strip" & p_rec <= 0 ~ "respose unlikely",
    Strip_Type == 	"N Strip" & maxN > 0 ~ "respose likely",
    Strip_Type == 	"N Strip" & maxN <= 0 ~ "respose unlikely",
    TRUE ~ "NA"
  ))


names(high_v_low_comp_results_Sean)


# count number of trials N and P
Count_N_P <- high_v_low_comp_results_Sean %>%
  group_by(Strip_Type) %>%
  summarise(count = n())
N_trails <- Count_N_P[1,2]
P_trails <- Count_N_P[2,2]
P_trails
# count number of trials N and P for each yield resposne

P_trials_lowVsHigh_fert <- high_v_low_comp_results_Sean %>% filter(Strip_Type == "P Strip" ) %>% 
  group_by(`yield response`) %>% 
  summarise(count = n()) %>% 
  mutate(freq = (count / P_trails[[1]])*100  )
 
P_trials_lowVsHigh_fert


N_trials_lowVsHigh_fert <- high_v_low_comp_results_Sean %>% filter(Strip_Type == "N Strip" ) %>% 
  group_by(`yield response`) %>% 
  summarise(count = n()) %>% 
  mutate(freq = (count / N_trails[[1]])*100  )

N_trials_lowVsHigh_fert


## next step is to add the  soil_test_indicates
names(high_v_low_comp_results_Sean)

# count number of trials N and P
# for N
Count_N_soil_test_likley_step1 <- high_v_low_comp_results_Sean %>%
  group_by(Strip_Type, soil_test_indicates) %>%
  summarise(count = n())
Count_N_soil_test_likley_step1
Count_N_soil_test_likley_step2 <- Count_N_soil_test_likley_step1 %>% 
  filter(Strip_Type == "N Strip" & soil_test_indicates == "respose likely" )
Count_N_soil_test_likley <- Count_N_soil_test_likley_step2[[1,3]]


# for p
Count_p_soil_test_likley_step1 <- high_v_low_comp_results_Sean %>%
  group_by(Strip_Type, soil_test_indicates, `yield response`) %>%
  summarise(count = n())
Count_p_soil_test_likley_step1
Count_p_soil_test_likley_step2 <- Count_p_soil_test_likley_step1 %>% 
  filter(Strip_Type == "P Strip" & soil_test_indicates == "respose likely" )
Count_p_soil_test_likley <- Count_p_soil_test_likley_step2[[1,3]]

Count_N_soil_test_likley
Count_p_soil_test_likley
