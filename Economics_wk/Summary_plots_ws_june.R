## June 2021 work
#df <- read.csv( write.csv(df, "W:/value_soil_testing_prj/Economics/2020/GSP_vs_other_withGM.csv")


### how many zone are there when the soil test says increase P
df$Strip_Type <- as.character(df$Strip_Type)
df$rec_rate_appox <- as.double(df$rec_rate_appox)

names(df)

plot_soil_test_info <- df %>% 
  dplyr::select("Zone_ID"  = "Zone_ID.x",
         "Strip_Type" = "Strip_Type.y",
         "join_zone_ID_Strip_Type",
         "rec_rate_appox_value", "soil_test_says_P", "soil_test_says_N",
         "p_rec",
         "n_rec")
plot_soil_test_info_N <- plot_soil_test_info %>% 
  filter(!is.na(soil_test_says_N))

plot_soil_test_info_N %>% 
  group_by(soil_test_says_N) %>% 
  summarise(count = n())
count(plot_soil_test_info_N)


plot_soil_test_info_P <- plot_soil_test_info %>% 
  filter(!is.na(soil_test_says_P))

plot_soil_test_info_P %>% 
  group_by(soil_test_says_P) %>% 
  summarise(count = n())
count(plot_soil_test_info_P)


########################################################################################################################

#Plots for recom higher rates

#high_low <- read.csv( write.csv(df, "W:/value_soil_testing_prj/Economics/2020/GSP_vs_high_low_withGM.csv")
high_low <- GS_high_low_3d
names(high_low)
unique(high_low$comparison)

high <- GS_high_low_3d %>% 
  filter(comparison == "GSP_v_high")

high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
  summarise(count = n())
count(high)

high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
              summarise( 
              mean_yld_GSP = round(mean(the_GSP, na.rm = TRUE),2),
              mean_yld_higher_rate = round(mean(higher_than_GSP, na.rm = TRUE),2))

high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
  summarise( 
    mean_GSP_vs_higher = abs(round(mean(GSP_vs_higher, na.rm = TRUE),2)))
    
high %>% 
  group_by(Strip_Type, rainfall_class) %>% 
  summarise( 
    mean_se_comp_GSP_high = abs(round(mean(se_comp_GSP_high, na.rm = TRUE),2)))
