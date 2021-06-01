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
#make a field for joining in both df.
str(GS_rates_3a)
GS_rates_3a$Strip_Type <- sub("^[^_]*_", "", GS_rates_3a$paddock_ID_Type)
#new clms with zone ID and strip type
GS_rates_3a <- GS_rates_3a %>% 
  mutate(Zone_ID_type = paste0(Zone_ID, "_", Strip_Type))
 
check_GS_rates_3a <- GS_rates_3a %>% 
  dplyr::distinct(Zone_ID_type,  .keep_all = TRUE)
names(check_GS_rates_3a)
check_GS_rates_3a <- check_GS_rates_3a %>% 
  dplyr::select(Zone_ID_type, Zone_ID, Strip_Type)

count(check_GS_rates_3a) #198#this includes stuff that wont get anlaysed


## have I got the same number of zones as Christina - whats on the list??

spatial_data_no_yld_zones <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Zones_2020_wgs84.shp")
spatial_data_no_yld_zones_df <- data.frame(spatial_data_no_yld_zones)
spatial_data_no_yld_zones_df_1 <- spatial_data_no_yld_zones_df %>% dplyr::select(-geometry)

##looks like the zone are duplicated if they have alt GSP not everything??
spatial_data_no_yld_zones_df_1 <- spatial_data_no_yld_zones_df_1 %>% 
  mutate(Zone_ID_Strip_type = paste0(Zone_ID,"_", Strip_Type))
zone <- dplyr::distinct(spatial_data_no_yld_zones_df_1, Zone_ID_Strip_type,  .keep_all = TRUE)
rm(spatial_data_no_yld_zones, spatial_data_no_yld_zones_df, spatial_data_no_yld_zones_df_1)


#only keep zones for economic analysis 
count(zone)
unique(zone$Status)
check_zone <- zone %>% 
  filter(Status == "5. Report Complete")

names(check_zone)
check_zone <- check_zone %>% 
  select("Zone_ID", "Strip_Type",
"organisati","contact" ,"farmer", "paddock",  "Status")  
count(check_zone)
## join the two togther and see what didnt join

str(check_zone)
str(check_GS_rates_3a)

join_check <- full_join(check_zone, check_GS_rates_3a)
join_check <-join_check %>% drop_na(contact)

list_of_zone_include <- join_check$Zone_ID
#now use this list to filter out my analysis...
GS_rates_3a_just_analysis <- GS_rates_3a %>% 
  filter(Zone_ID %in% list_of_zone_include)

count(GS_rates_3a_just_analysis)
names(GS_rates_3a_just_analysis)
GS_rates_3a_just_analysis <- GS_rates_3a_just_analysis %>% 
  filter(rate_name == "Grower_rate")

count(GS_rates_3a_just_analysis)



# I cant use a paddock code 5 digits for everything because zone code are now a mix of 6 and 7 digits
#What paddocks have 7 digit codes?

GS_rates_3a_just_analysis$length_zoneID <- nchar(GS_rates_3a_just_analysis$Zone_ID)
#remove last value in string
GS_rates_3a_just_analysis <- GS_rates_3a_just_analysis %>% 
  mutate(paddock_code =   
           case_when(length_zoneID == 6 ~ substr(Zone_ID, start = 1, stop = 5),
                     length_zoneID == 7 ~ substr(Zone_ID, start = 1, stop = 6)))


GS_rates_3a_just_analysis <- GS_rates_3a_just_analysis %>% 
  dplyr::mutate(fld_for_join = paste0(paddock_code,"_", Details,"_",Strip_Type))






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
    Total_sum_P_content,
    Status
  )

## need to remove the traiiling white space at the end of Strip Rate.

rainfall_fert$Strip_Rate <- stringi::stri_trim_right(rainfall_fert$Strip_Rate)

str(GS_rates_3a_just_analysis$Zone_ID) #This is the zone code with 6 digits
str(GS_rates_3a_just_analysis$paddock_code)
str(rainfall_fert$Paddock_ID) #this is the paddock code with 5 digits
#because Harm changed the paddock codes part way through the projcet I need some extra chceked when joing paddock code to zone.
#are details and strip rate the same??

#I have a list of zones to be included #  filter(Zone_ID %in% list_of_zone_include)
# change this to a list of paddocks some paddocks have 6 digits some have 7
#create a new list list_of_zone_include
list_of_zone_include_df <- data.frame(list_of_zone_include)
names(list_of_zone_include_df)
list_of_zone_include_df <- rename(list_of_zone_include_df, zone_incl = list_of_zone_include)

str(list_of_zone_include_df)
list_of_zone_include_df$zone_incl <- as.character(list_of_zone_include_df$zone_incl)

list_of_zone_include_df$length_zoneID <- nchar(list_of_zone_include_df$zone_incl)

list_of_zone_paddocks_include_df <- list_of_zone_include_df %>%
  mutate(
    paddock_incl = case_when(
      length_zoneID == 6 ~ substr(zone_incl, start = 1, stop = 5),
      length_zoneID == 7 ~ substr(zone_incl, start = 1, stop = 6)
           ))
list_of_paddock_include <- list_of_zone_paddocks_include_df$paddock_incl

names(rainfall_fert)

test <- rainfall_fert %>% 
  filter(Paddock_ID %in% list_of_paddock_include)

### need to remove the Alt GPS from the rainfall data                
#names(rainfall_fert)
#unique(rainfall_fert$GSP)
#str(rainfall_fert$GSP)
rainfall_fert$GSP <- as.character(rainfall_fert$GSP)
unique(rainfall_fert$GSP)
unique(rainfall_fert$Status)
rainfall_fert %>%  count(GSP) 

#remove from analysis in status clm this is paddocks
rainfall_fert <- rainfall_fert %>%  
filter(Status != "Excluded from Analysis")
rainfall_fert %>%  count(GSP) 

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
GS_rates_3a_plus_rain_fert_content <- left_join(GS_rates_3a_just_analysis, rainfall_fert, by = "fld_for_join")

count(GS_rates_3a_plus_rain_fert_content)

## lets check again how many zone and paddocks do I have? # I am happy have 85 paddock
#check how many paddocks / zones I have for N and P
# names(GS_rates_3a_plus_rain_fert_content)
# check10 <- GS_rates_3a_plus_rain_fert_content %>%  count(rate_name) #number of zone with analysis done that have GR
# check10
# check11 <- GS_rates_3a_plus_rain_fert_content %>% 
#   filter(rate_name == "Grower_rate") %>% 
#   select(Zone_ID, input_file, paddock_code, Strip_Type.x, paddock_ID_Type)
# names(GS_rates_3a_plus_rain_fert_content)
# check12 <- check11 %>%  distinct(paddock_ID_Type, .keep_all = TRUE)
# 
# check12 %>%  count(Strip_Type.x)
# count(check12) #this should be the same value as in the summary report for complete anlysis.
# # I am one out but I assume this is the excuded paddocks

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
# names(GS_rates_rain_fert_rec)
# check <- GS_rates_rain_fert_rec %>%
#   dplyr::select(
#     Strip_Type,
#     p_rec,
#     maxN,
#     soil_test_indicates, 
#     SM_comment_Soil_N,
#     SM_comment_Soil_P,
#     SM_comment_Plant_Tissue,
#     Zone_ID,
#     n_rec_yld_low,
#     n_rec_yld_med,
#     n_rec_yld_high
#   )
# 
# write.csv(check, "W:/value_soil_testing_prj/Economics/2020/check1.csv")



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
names(GS_rates_rain_fert_rec)
rec_rate_approx_N_P <- rec_rate_approx_N_P %>%  dplyr::select(-rec_rate)
df <- left_join(GS_rates_rain_fert_rec, rec_rate_approx_N_P, by = "Fld_Join_Approx1")


# CHECK THAT  we have join correctly
## lets check again how many zone and paddocks do I have? # I am happy have 85 paddock
#check how many paddocks / zones I have for N and P
# names(df$Zone_ID.x)
# check100 <- df %>%  count(rate_name) #number of zone with analysis done that have GR
# check100
# check110 <- df %>% 
#   filter(rate_name == "Grower_rate") %>% 
#   select(Zone_ID.x, input_file, paddock_code, Strip_Type.x, paddock_ID_Type)
# names(df)
# check120 <- check110 %>%  distinct(paddock_ID_Type, .keep_all = TRUE)
# 
# check120 %>%  count(Strip_Type.x)
# count(check120) #this should be the same value as in the summary report for complete anlysis.
# I am one out but I assume this is the excuded paddocks
names(df)

df <- df %>% 
  dplyr::select(Zone_ID =Zone_ID.x,
                Rate,
                rate_name,
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
                paddock,
                input_file,
                paddock_code,
                paddock_ID_Type,
                Status, 
                rate_name_order)



#########################################################################################################
### make a clm that is has P content and N content in same clm called P_N_content.

df <- df %>% 
  mutate(
    N_P_content = case_when(
    Strip_Type == "P Strip" ~ P_content,
    Strip_Type == "N Strip" ~ N_content
  ))
names(df)



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

### last check on merged data frame.
# CHECK THAT  we have join correctly
## lets check again how many zone and paddocks do I have? # I am happy have 85 paddock
#check how many paddocks / zones I have for N and P
# names(df)
# check1000 <- df %>%  count(rate_name) #number of zone with analysis done that have GR
# check1000
# check1100 <- df %>%
#   filter(rate_name == "Grower_rate") %>%
#   select(Zone_ID, input_file, paddock_code, Strip_Type, paddock_ID_Type)
# names(df)
# check1200 <- check1100 %>%  distinct(paddock_ID_Type, .keep_all = TRUE)
# 
# check1200 %>%  count(Strip_Type)
# count(check1200)






#############################################################################
#some extra clms should I put on more P / N based on the soil test?
names(df)
df <- df %>% mutate(soil_test_says_P = case_when(
  Strip_Type == "P Strip" & GSP == "GSP"& P_content < p_rec ~  "add_more_P",
  Strip_Type == "P Strip" & GSP == "GSP"& P_content == p_rec ~ "no_change",
  Strip_Type == "P Strip" & GSP == "GSP"& P_content > p_rec ~  "less_P",
))
  
df <- df %>% mutate(soil_test_says_N = case_when(
  Strip_Type == "N Strip" & GSP == "GSP"& N_content < n_rec ~  "add_more_N",
  Strip_Type == "N Strip" & GSP == "GSP"& N_content == n_rec ~ "no_change",
  Strip_Type == "N Strip" & GSP == "GSP"& N_content > n_rec ~  "less_N",
))

#########################################################################
## Add some clm about what is higher or lower than the GSP.


GS_high_low_3d <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/GSP_low_high_comparision_t_test_merged_3d.csv")
#make a field for joining in both df.
str(GS_high_low_3d)
#select a few clms.
GS_high_low_3d <- GS_high_low_3d %>% 
  select(Zone_ID,
         Strip_Type,
         higher_than_GSP_label,
         the_GSP_label,
         lower_than_GSP_label)
GS_high_low_3d <- GS_high_low_3d %>% 
  mutate(join_zone_ID_Strip_Type = paste0(Zone_ID, "_", Strip_Type))
GS_high_low_3d <- GS_high_low_3d %>% 
  select(higher_than_GSP_label,
         the_GSP_label,
         lower_than_GSP_label,
         join_zone_ID_Strip_Type)

##This is duplicated so i need to remove it.
GS_high_low_3d <- GS_high_low_3d %>% 
  distinct(join_zone_ID_Strip_Type, .keep_all=TRUE)

names(df)
df <- df %>% 
  mutate(join_zone_ID_Strip_Type = paste0(Zone_ID, "_", Strip_Type))

## join
df <- left_join(df, GS_high_low_3d, by= "join_zone_ID_Strip_Type")
names(df)

###############################################################
## check join - it looks good
# df <- df %>%
#   select(
#     "Zone_ID.x",
#     "Rate",
#     "rate_name",
#     "Strip_Type.x" ,
#     "Zone_ID.y" ,
#     "Strip_Type.y"  ,
#     "higher_than_GSP_label",
#     "the_GSP_label" ,
#     "lower_than_GSP_label",
#     "paddock_ID_Type",
#     "input_file",
#     "paddock_code"
#   )

# df <- df %>% 
#   mutate(paddock_ID_Type.x = paste0(paddock_code, "_", Strip_Type.x)) 
# check01 <- df %>%  count(rate_name) #number of zone with analysis done that have GR
# 
# check01
# check02 <- df %>%
#   filter(rate_name == "Grower_rate") %>%
#   select(Zone_ID.x, input_file, paddock_code, Strip_Type.x, paddock_ID_Type.x)
# 
# check03 <- check02 %>%  distinct(paddock_ID_Type.x, .keep_all = TRUE)
# check03 %>%  count(Strip_Type.x)
# count(check03)
#######################################################################
## now turn these extra clms into one clm high_lower_GSP
names(df)
df <- df %>% 
  mutate(high_lower_GSP = case_when(
    higher_than_GSP_label == Rate ~ "higher_than_GSP",
    the_GSP_label         == Rate ~ "the_GSP",
    lower_than_GSP_label  == Rate ~ "lower_than_GSP"
  ))


##############################################################################
### add some GM to yld results 
##############################################################################
# convert the yield to income of grain $/ha
# this assumes that all yield is in t /ha 
# all crops are wheat
# the 5 year wheat average is $286 and is same for all sites.
names(df)
df <- df %>% mutate(grain_income = yield * 286)

unique(df$rate_name_order)
df$rate_name_order <- as.character(df$rate_name_order)


##############################################################################
# Check this via summary stats grain_income

summaries_grain_income <- 
  df %>% group_by(Strip_Type, rate_name_order) %>% 
  summarise(
  count = n(),
  mean_grain_income = mean(grain_income, na.rm = TRUE),
  min_grain_income = min(grain_income, na.rm = TRUE),
  max_grain_income = max(grain_income, na.rm = TRUE)
)

summaries_grain_income$rate_name_order <- as.factor(summaries_grain_income$rate_name_order)
summaries_grain_income$rate_name_order <- factor(summaries_grain_income$rate_name_order, 
                                        levels = c("very_low", "low", "medium", "high", "very_high"))

mean_grain_income_plot <- ggplot(summaries_grain_income, aes(rate_name_order,mean_grain_income ))+
  geom_col() +
  facet_wrap(.~Strip_Type)
min_grain_income_plot <-ggplot(summaries_grain_income, aes(rate_name_order,min_grain_income ))+
  geom_col() +
  facet_wrap(.~Strip_Type)
max_grain_income_plot <-ggplot(summaries_grain_income, aes(rate_name_order,max_grain_income ))+
  geom_col() +
  facet_wrap(.~Strip_Type)

mean_grain_income_plot
min_grain_income_plot
max_grain_income_plot
##############################################################################
## cost for test $3 per ha for rates that are not the GSP

names(df)
df <- df %>% mutate(cost_test = case_when(
  GSP == "GSP" ~ 3.00,
  TRUE ~ 0.00))
  
## cost fert is based on rainfall class - define the rainfall class

df <- df %>% 
  dplyr::mutate(
    rainfall_class = case_when(
      av_rain<=350 ~ "low",
      av_rain >500 ~ "high",
      TRUE ~ "medium"
    )
  )
names(df)
df <- df %>% 
  dplyr::mutate(
    variable_costs = case_when(
      Strip_Type == "P Strip" & rainfall_class == "low" ~     194,
      Strip_Type == "P Strip" & rainfall_class == "medium" ~  358,
      Strip_Type == "P Strip" & rainfall_class == "high" ~    540,
      
      Strip_Type == "N Strip" & rainfall_class == "low" ~     220,
      Strip_Type == "N Strip" & rainfall_class == "medium" ~  340,
      Strip_Type == "N Strip" & rainfall_class == "high" ~    498))

### I don't get this step but it converts N applied from kg/ha to cost of N $ha
 
 names(df)

df <- df %>% 
  dplyr::mutate(
    Cost_P_N_dollar_ha  = case_when(
      Strip_Type == "P Strip"  ~     P_content * 2.9,
      Strip_Type == "N Strip"  ~     N_content * 1.1))     

## GM
#GM = Income grain – cost test – variable cost – cost of N
names(df)

df <- df %>% 
  dplyr::mutate(
    total_cost = cost_test + variable_costs + Cost_P_N_dollar_ha,
    GM  = grain_income - total_cost)

##############################################################################################
## more clms to to get the GM values that relate to rates 'higher than GSP' 'the GPS'  etc
# this is multiple step process first make a temp datafarme and create new clm summaries it and join it back
names(df)

temp_df <- df %>% 
  dplyr::select(Zone_ID,
                Rate,
                Strip_Type,
                Fld_Join_Approx1,
                higher_than_GSP_label,
                the_GSP_label,
                lower_than_GSP_label,
                GM,
                yield,
                Cost_P_N_dollar_ha)


#1. GM for higher than GSP rate
temp_df <- temp_df %>%
  mutate(GM_higher_than_GSP_rate = case_when(
    higher_than_GSP_label == Rate ~ GM
  ))
#2. GM for the GSP rate
temp_df <- temp_df %>%
  mutate(GM_GSP_rate = case_when(
    the_GSP_label == Rate ~ GM
  ))
#3. GM for lower than GSP rate
temp_df <- temp_df %>%
  mutate(GM_lower_than_GSP_rate = case_when(
    lower_than_GSP_label == Rate ~ GM
  ))


#4. Yld for higher than GSP rate
temp_df <- temp_df %>%
  mutate(YLD_higher_than_GSP_rate = case_when(
    higher_than_GSP_label == Rate ~ yield
  ))
#5. Yld for the GSP rate
temp_df <- temp_df %>%
  mutate(YLD_GSP_rate = case_when(
    the_GSP_label == Rate ~ yield
  ))
#6. Yld for lower than GSP rate
temp_df <- temp_df %>%
  mutate(YLD_lower_than_GSP_rate = case_when(
    lower_than_GSP_label == Rate ~ yield
  ))

#7. cost_P_N$_ha for higher than GSP rate
temp_df <- temp_df %>%
  mutate(cost_NP_higher_than_GSP_rate = case_when(
    higher_than_GSP_label == Rate ~ Cost_P_N_dollar_ha
  ))
#8. cost_P_N$_ha for the GSP rate
temp_df <- temp_df %>%
  mutate(cost_NP_GSP_rate = case_when(
    the_GSP_label == Rate ~ Cost_P_N_dollar_ha
  ))
#9. cost_P_N$_ha for lower than GSP rate
temp_df <- temp_df %>%
  mutate(cost_NP_lower_than_GSP_rate = case_when(
    lower_than_GSP_label == Rate ~ Cost_P_N_dollar_ha
  ))

## condense this so I have one line for each zone
names(temp_df)
temp_df1 <- temp_df %>% 
  group_by(Fld_Join_Approx1) %>% 
  summarise(GM_higher_than_GSP_rate  = round(max(GM_higher_than_GSP_rate, na.rm = TRUE),0),
            GM_GSP_rate              = round(max(GM_GSP_rate, na.rm = TRUE),0),
            GM_lower_than_GSP_rate   = round(max(GM_lower_than_GSP_rate, na.rm = TRUE),0),
            
            YLD_higher_than_GSP_rate = round(max(YLD_higher_than_GSP_rate, na.rm = TRUE),4),
            YLD_GSP_rate             = round(max(YLD_GSP_rate, na.rm = TRUE),4),
            YLD_lower_than_GSP_rate  = round(max(YLD_lower_than_GSP_rate, na.rm = TRUE),4),
            
            cost_NP_higher_than_GSP_rate = round(max(cost_NP_higher_than_GSP_rate, na.rm = TRUE),4),
            cost_NP_GSP_rate             = round(max(cost_NP_GSP_rate, na.rm = TRUE),4),
            cost_NP_lower_than_GSP_rate  = round(max(cost_NP_lower_than_GSP_rate, na.rm = TRUE),4)
            )

names(temp_df1)
temp_df1 <-
  temp_df1 %>% mutate(
    GM_higher_than_GSP_rate = na_if(GM_higher_than_GSP_rate,-Inf),
    GM_GSP_rate = na_if(GM_GSP_rate,-Inf),
    GM_lower_than_GSP_rate = na_if(GM_lower_than_GSP_rate,-Inf),
    
    YLD_higher_than_GSP_rate = na_if(YLD_higher_than_GSP_rate,-Inf),
    YLD_GSP_rate = na_if(YLD_GSP_rate,-Inf),
    YLD_lower_than_GSP_rate = na_if(YLD_lower_than_GSP_rate,-Inf),
    
    cost_NP_higher_than_GSP_rate = na_if(cost_NP_higher_than_GSP_rate,-Inf),
    cost_NP_GSP_rate = na_if(cost_NP_GSP_rate,-Inf),
    cost_NP_lower_than_GSP_rate = na_if(cost_NP_lower_than_GSP_rate,-Inf)
  )




# names(df)
# 
# df_select <- df %>%
#   select(
#     Zone_ID,
#     Rate,
#     rate_name,
#     GSP,
#     Strip_Type,
#     Details,
#     Fld_Join_Approx1,
#     higher_than_GSP_label,
#     the_GSP_label,
#     lower_than_GSP_label,
#     GM
#   )
# names(df_select)
# names(temp_df1)
# remove redunant clm and replace inf
df <- left_join(df, temp_df1, by= "Fld_Join_Approx1")


df <- df %>% 
  mutate(yld_diff_higher_rate = YLD_higher_than_GSP_rate-  YLD_GSP_rate,
         yld_diff_lower_rate =  YLD_GSP_rate - YLD_lower_than_GSP_rate,
         
         GM_diff_higher_rate = GM_higher_than_GSP_rate-  GM_GSP_rate,
         GM_diff_lower_rate = GM_GSP_rate - GM_lower_than_GSP_rate)


# ## let output this and check it with some stuff....
 write.csv(df, "W:/value_soil_testing_prj/Economics/2020/GSP_vs_other_withGM.csv")


#########################################################################
 ### This GM work should also go into the the 3d file
 ##1. clear the workspace on non used objcets
 ##2. make a temp df file with field to join zone ID strip type (might have one alreday)
 ##3. select only a few clms join fld, GM diff high rate , GM diff lower rate, status, rainfall class
 ##4. bring in the 3d file again
 ##5. join and save
 
 #1. 
 rm(list=ls()[!ls() %in% 'df'])
 #2. 
 names(df)
 df_temp <- df %>% 
   dplyr::select(
   Fld_Join_Approx1,
   rainfall_class,
   GM_diff_higher_rate,
   GM_diff_lower_rate,
   Status)
 names(df_temp)
 # I have duplicates but I only want one entry per zone and strip type
 df_temp <- df_temp %>% 
   distinct(Fld_Join_Approx1, .keep_all = TRUE)
#3. bring in 
 GS_high_low_3d <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/GSP_low_high_comparision_t_test_merged_3d.csv")
 GS_high_low_3d <- GS_high_low_3d %>% 
   mutate(Fld_Join_Approx1 = paste0(Zone_ID, Strip_Type))
 
 #5. join
 names(df_temp)
 names(GS_high_low_3d)
 
 GS_high_low_3d <- left_join(GS_high_low_3d, df_temp, by = "Fld_Join_Approx1")

 unique(GS_high_low_3d$yld_response)
 
 GS_high_low_3d <- GS_high_low_3d %>% 
   dplyr::mutate(
     yld_response_sig = case_when(
       Significant_practical == "significant" &     yld_response == "positive" ~       "positive_sig",
       Significant_practical == "not significant" & yld_response == "positive" ~       "no_response_sig",
       
       Significant_practical == "significant" &     yld_response == "negative" ~        "negative_sig",
       Significant_practical == "not significant" & yld_response == "negative" ~        "no_response_sig",
       
       Significant_practical == "significant" &     yld_response == "no_response" ~     "no_response_sig",
       Significant_practical == "not significant" & yld_response == "no_response" ~     "no_response_sig",
       
       ))
 
 
 ## write out to 
 write.csv(GS_high_low_3d, "W:/value_soil_testing_prj/Economics/2020/GSP_vs_high_low_withGM.csv")
 
 
 
 ### should check GM


check_GM1 <- df %>%  count(rate_name) #number of zone with analysis done that have GR
check_GM1
check_GM2 <- df %>%
  filter(rate_name == "Grower_rate") %>%
  select(Zone_ID, input_file, paddock_code, Strip_Type, paddock_ID_Type)
names(df)
check_GM3 <- check_GM2 %>%  distinct(paddock_ID_Type, .keep_all = TRUE)

check_GM3 %>%  count(Strip_Type)
count(check_GM3)


### This should be all the bits we need!!

#Refer the the csv file or additional R script for plots and tables

