## scripts for MESH project

#Install and load required packages
required_packages <- c("dplyr","ggplot2", "tidyr", "purrr", "readr","readxl","patchwork",
                       "lubridate", "stringr", "forcats", "scales","sf","gridExtra","grid","lattice","ggpubr","gt")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
# Load packages
lapply(required_packages, library, character.only = TRUE)

# Set working directory
sys <- Sys.info()
user <- sys["user"]
setwd(paste0("C:/Users/", user, "/OneDrive - NIVA/Projekter/2025/MESH"))

# Load grid EEA 100x20km grid
grid_minus_land <- st_read("data/grid/assessment grid 100x20km/assessment_grid_100_20_minus_land.shp") %>%
  mutate(GRIDCODE = tolower(GRIDCODE)) %>%
  select(GRIDCODE, Region, Include)

## Load EQR data for HEAT, BEAT, CHASE, dfMALTH ###

                    #######Supporting (HEAT) #########
# HEAT_int <- read_delim("data/heat/HEAT.csv",  ### Integrated HEAT is not used, since we need to exclude parameters used in BEAT
#                                delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
#   mutate(GRIDCODE = tolower(GRIDCODE)) 

dfHEAT <- read_delim("data/heat/HEAT_parameter.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  mutate(GRIDCODE = tolower(GRIDCODE)) %>%
  select(-remove)

# csv file with updated exclude/include parameters for supporting assessment
exclude_supporting_updated <- read_delim("C:/Users/sio/OneDrive - NIVA/Projekter/2025/MESH/data/heat/exclude_supporting_updated.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  distinct(Parameter, Exclude)

 ######Biological (BEAT) ########
dfBEAT2 <- read_excel("data/beat/BEAT Results.xlsx")%>% #similar to HEAT, some parameters need to be removed, since they are used as supporting
  mutate(GRIDCODE = tolower(GRIDCODE))

dfBEAT <- read_delim("C:/Users/sio/OneDrive - NIVA/Projekter/2025/MESH/data/beat/Results_Indicators_BEAT.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(GRIDCODE = tolower(SpatialAssessmentUnit))


####### #Chemical (CHASE)  ########
dfCHASE <- read_delim("data/chase/CHASE.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  mutate(GRIDCODE = tolower(GRIDCODE)) 

dfCHASEParam <- read_delim("data/chase/chase_parameter.csv",
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(GRIDCODE = tolower(GridID))
#### #Marine Litter (MALT) #######
dfMALT <- read_delim("C:/Users/sio/OneDrive - NIVA/Projekter/2025/MESH/data/malt/MALT_EQR_per_category.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(GRIDCODE = tolower(GRIDCODE))
  

#--------- PREPARE Biological, Chemical and Supporting data for MESH calculations ----


#--------------- Prepare Biological (BEAT) data ---------------------
# Vector with BEAT parameters to exclude from the biological assessment
exclude_bio <- c("Secchi Depth") 

# Calculate EQR for biological parameters, excluding those used in supporting assessment
#For species
dfBioSpec <- dfBEAT %>%
  filter(!Indicator %in% exclude_bio) %>% # Remove parameters used in supporting assessment
  group_by(GRIDCODE, Group,SpeciesGroup, Species) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE),
            n = n())
#For species group
dfBioSpecGrp <- dfBioSpec %>%
  group_by(GRIDCODE, Group,SpeciesGroup) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE),
            n = sum(n))
# For groups and pivot
dfBio <- dfBioSpecGrp %>%
  group_by(GRIDCODE, Group) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE),
            n = sum(n))
# Avg EQR per gridcell
dfBioQE <- dfBio %>%
  group_by(GRIDCODE) %>%
  summarise(EQR = mean(as.numeric(EQR), na.rm = TRUE),
            n_indi = sum(n)) %>%
  mutate(QE = "Biology") %>%
  ungroup() 



#---------------- ## Prepare Chemichal (CHASE) data -----------

dfChemSub <- dfCHASEParam %>%
  group_by(GRIDCODE, Category,Substance) %>%
  summarise(EQR = mean(EQR_cat, na.rm = TRUE)) %>%
  ungroup() 

# Avg EQR pr gridcell
dfChemCat <- dfChemSub %>%
  group_by(GRIDCODE,Category) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE),
            n = n())%>%
  ungroup() 

dfChemQE <- dfChemCat %>%
  group_by(GRIDCODE) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE),
            n_indi = sum(n)) %>%
  ungroup() %>%
  mutate(QE = "Chemistry") # Add column for Quality Element
### Prepare Marine litter data

#---------------- ## Prepare Supporting (HEAT) data -----------
# For supporting some parameters are removed as they are included in the BEAT assessment.
# NOTE! Data coverage between HEAT and BEAT is different for chlorophyll and Secchi.
# HEAT has better data coverage for chlorophyll and secchi and they are lost when filtering supporting as is done currently.


dfSupportHEAT<- left_join(dfHEAT,exclude_supporting_updated) %>%
  filter(Exclude == 0) %>% # Only keep supporting parameters
  select(GRIDCODE, EQR = ER) %>%
  mutate(QE = "Supporting") 

dfSupportMALT <- dfMALT %>%
  select(GRIDCODE,EQR) %>%
  mutate(QE = "Supporting") %>%
  filter(!is.na(EQR))

dfSupportQE <- bind_rows(dfSupportHEAT,dfSupportMALT) %>%
  group_by(GRIDCODE,QE) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE),
            n_indi = n()) %>%
  ungroup()

####





df <- bind_rows(dfSupportQE,dfChemQE,dfBioQE) 

####
df2 <- df %>%
  group_by(GRIDCODE) %>%
  summarise(n_thematic = n(),
            n_bio = sum(QE == "Biology", na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Condition = ifelse(n_thematic >= 2 & n_bio >= 1, "OK", "Not OK"))

# Without removing indicator or tool conditions
df_worst_EQR <- df %>%
  group_by(GRIDCODE) %>%
  summarise(EQR = min(EQR, na.rm = TRUE)) %>%
  mutate(EQR=ifelse(is.infinite(EQR),NA,EQR)) %>%
  ungroup()

#########################################################################

# find the quality element responsible
df_worst_QE <- df_worst_EQR %>%
  left_join(df, by = c("GRIDCODE", "EQR")) %>%
  select(GRIDCODE, QE, EQR)

# if two QEs have the same worst EQR value in the same Grid cell, take only the first one
df_worst_QE <- df_worst_QE %>%
  group_by(GRIDCODE) %>%
  arrange(GRIDCODE, EQR) %>%
  slice(1) %>%
  ungroup() %>%
  rename(Worst = QE)

# arrange QE EQR values in columns (wide)
df_QE <- df %>%
  select(-n_indi)%>%
  filter(!is.na(EQR)) %>%
  pivot_wider(names_from = QE,values_from = EQR)

# include columns for the final overall (worst) EQR and show the worst QE
df_MESH <- df_QE %>%
  left_join(df_worst_QE)

## make spatial
df_MESH <- left_join(grid_minus_land, df_MESH, by = c("GRIDCODE" = "GRIDCODE")) %>%
  mutate(Status = case_when(
    Include == "N" ~ "Not Included",
    EQR >= 0.8 ~ "High",
    EQR >= 0.6 ~ "Good",
    EQR >= 0.4 ~ "Moderate",
    EQR >= 0.2 ~ "Poor",
    EQR < 0.2 ~ "Bad",
    is.na(EQR) ~ "No Data"
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad",  "No Data","Not Included"))) %>%
  mutate(geometry = st_make_valid(geometry))

# Plotting without transparency
MESH_p_noTransp <- ggplot() +
  geom_sf(data = df_MESH, aes(geometry = geometry, fill = Status)) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" = "green",
      "High" = "blue",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = NULL)


################## With transparency #################

df_MESH2 <- df_MESH %>%
  left_join(df2, by = "GRIDCODE") %>%
  mutate(Alpha = ifelse(Condition == "OK", 1, 0.5)) %>%
  mutate(Alpha = ifelse(Status %in% c("No Data", "Not Included"), 1, Alpha))

MESH_p_Transp <- ggplot() +
  geom_sf(data = df_MESH2, aes(geometry = geometry, fill = Status, alpha = Alpha)) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" = "green",
      "High" = "blue",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  scale_alpha_continuous(range = c(0.5, 1), guide = "none") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = NULL)


MESH_comb <- MESH_p_noTransp + MESH_p_Transp + plot_layout(nrow = 1, guides = "collect") + 
  plot_annotation(tag_levels = 'a',title = "MESH Results with (a) no transparency and (b) transparency on filtered conditions.",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))& theme(legend.position = "bottom")

# save
ggsave("figures/mesh_comb.png",
       plot = MESH_comb,
       width = 14, height = 8, dpi = 300, bg = "white")


##
# Plotting NPA and PA without transparency

df_MESH_category <- df_MESH %>%
  mutate(Classification = case_when(
    Status %in% c("High", "Good") ~ "NPA",
    Status %in% c("Moderate", "Poor", "Bad") ~ "PA",
    Status == "No Data" ~ "No Data",
    Status == "Not Included" ~ "Not Included"
  )) %>%
  mutate(Classification = factor(Classification, levels = c("NPA", "PA", "No Data", "Not Included"))) 

plot_MESH_category <- ggplot() +
  geom_sf(data = df_MESH_category, aes(geometry = geometry, fill = Classification)) +
  scale_fill_manual(
    values = c(
      "NPA" = "limegreen",
      "PA" = "firebrick",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom") +  
  labs(title = NULL)


# Plotting NPA and PA with transparency
df_MESH_transp <- df_MESH %>%
  left_join(df2, by = "GRIDCODE") %>%
  mutate(Alpha = ifelse(Condition == "OK", 1, 0.5)) %>%
  mutate(Alpha = ifelse(Status %in% c("No Data", "Not Included"), 1, Alpha))

df_MESH_category_transp <- df_MESH_transp %>%
  mutate(Classification = case_when(
    Status %in% c("High", "Good") ~ "NPA",
    Status %in% c("Moderate", "Poor", "Bad") ~ "PA",
    Status == "No Data" ~ "No Data",
    Status == "Not Included" ~ "Not Included"
  )) %>%
  mutate(Classification = factor(Classification, levels = c("NPA", "PA", "No Data", "Not Included"))) 

# Plotting NPA and PA
plot_MESH_category_transp <- ggplot() +
  geom_sf(data = df_MESH_category_transp, aes(geometry = geometry, fill = Classification, alpha = Alpha)) +
  scale_fill_manual(
    values = c(
      "NPA" = "limegreen",
      "PA" = "firebrick",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") + 
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom") +  
  labs(title = NULL)


GES_comb <- plot_MESH_category + plot_MESH_category_transp + plot_layout(nrow = 1, guides = "collect") + 
  plot_annotation(tag_levels = 'a',title = "Non-problem areas (NPA) and problem areas (PA) in MESH Results with (a) no transparency and (b) transparency on filtered conditions.",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))& theme(legend.position = "bottom")

# save
ggsave("figures/mesh_npa_pa_comb.png",
       plot = GES_comb,
       width = 14, height = 8, dpi = 300, bg = "white")


## 

# plot supporting (HEAT) indicators used in the MESH assessment
## Notice the supporting plot is different from the HEAT assessment, since chla and secchi are removed (INCLUDED IN BIOLOGICAL)
# Also this plot is the mean across the remaining parameters, not the worst EQR.

supporting_spatial <- left_join(grid_minus_land,dfSupportQE, by = "GRIDCODE") %>%
  mutate(Status = case_when(
    EQR >= 0 & EQR < 0.2 ~ "Bad",
    EQR >= 0.2 & EQR < 0.4 ~ "Poor",
    EQR >= 0.4 & EQR < 0.6 ~ "Moderate",
    EQR >= 0.6 & EQR < 0.8 ~ "Good",
    EQR >= 0.8 ~ "High",
    is.na(EQR) ~ "No Data"
  ),
  Status = case_when(
    Include == "N" ~ "Not Included",
    TRUE ~ Status
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad",  "No Data","Not Included"))) %>%
  mutate(geomtry = st_make_valid(geometry))


plot_supporting <-ggplot() +
  geom_sf(data = supporting_spatial, aes(fill = Status)) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" =  "green",
      "High" ="blue",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal()+
  theme(legend.position = "bottom")+  
  labs(title = NULL)

print(plot_supporting)


#Plot biological (BEAT) indicators used in the MESH assessment
biological_spatial <- left_join(grid_minus_land, dfBioQE, by = c("GRIDCODE" = "GRIDCODE")) %>%
  mutate(Status = case_when(
    EQR >= 0 & EQR < 0.2 ~ "Bad",
    EQR >= 0.2 & EQR < 0.4 ~ "Poor",
    EQR >= 0.4 & EQR < 0.6 ~ "Moderate",
    EQR >= 0.6 & EQR < 0.8 ~ "Good",
    EQR >= 0.8 ~ "High",
    is.na(EQR) ~ "No Data"
  ),
  Status = case_when(
    Include == "N" ~ "Not Included",
    TRUE ~ Status
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad",  "No Data","Not Included"))) %>%
  mutate(geomtry = st_make_valid(geometry))

plot_biological <-ggplot() +
  geom_sf(data = biological_spatial, aes(fill = Status)) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" =  "green",
      "High" ="blue",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = NULL)
print(plot_biological)

## Plot chemical (CHASE) indicators used in the MESH assessment
chemical_spatial <- left_join(grid_minus_land, dfChemQE, by = c("GRIDCODE" = "GRIDCODE")) %>%
  mutate(Status = case_when(
    EQR >= 0 & EQR < 0.2 ~ "Bad",
    EQR >= 0.2 & EQR < 0.4 ~ "Poor",
    EQR >= 0.4 & EQR < 0.6 ~ "Moderate",
    EQR >= 0.6 & EQR < 0.8 ~ "Good",
    EQR >= 0.8 ~ "High",
    is.na(EQR) ~ "No Data"
  ),
  Status = case_when(
    Include == "N" ~ "Not Included",
    TRUE ~ Status
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad",  "No Data","Not Included"))) %>%
  mutate(geomtry = st_make_valid(geometry))

plot_chemical <-ggplot() +
  geom_sf(data = chemical_spatial, aes(fill = Status)) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" =  "green",
      "High" ="blue",
      "No Data" = "grey",
      "Not Included" = "azure2"
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = NULL)
print(plot_chemical)



plot1 <- MESH_p_noTransp + labs(title = "MESH")
plot2 <- plot_biological + labs(title = "Biological")
plot3 <- plot_chemical + labs(title = "Chemical")
plot4 <- plot_supporting + labs(title = "Supporting")

combined_plot <- (plot1 + plot2 + plot3 + plot4) +
  plot_layout(nrow = 2, guides = "collect") +
  plot_annotation(
    title = "Results for MESH, Biological, Chemical and Supporting",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  ) &
  theme(legend.position = "bottom")

## Save
ggsave("figures/combined_plot.png",
       plot = combined_plot,
       width = 14, height = 10, dpi = 300, bg = "white")



####### Macaronesia #############
macaronesia <- st_read("data/grid/grid_macaronesia/macaronesia.shp") %>%
  mutate(GRIDCODE = tolower(GRIDCODE)) %>%
  mutate(subregion = "Macaronesia")%>%
  select(GRIDCODE, subregion)


dfBEAT_macaronesia <- left_join(dfBEAT,macaronesia, by = "GRIDCODE") %>%
  filter(!is.na(subregion))

macaronesia_spec <- dfBEAT_macaronesia %>%
  group_by(GRIDCODE,SpeciesGroup) %>%
  summarise(EQR = mean(EQR),
            n = n())
# Create a spatial object for Macaronesia

macaronesia_spec_data <- left_join(macaronesia, macaronesia_spec, by = "GRIDCODE") %>%
  mutate(Status = case_when(
    EQR >= 0.8 ~ "High",
    EQR >= 0.6 ~ "Good",
    EQR >= 0.4 ~ "Moderate",
    EQR >= 0.2 ~ "Poor",
    EQR < 0.2 ~ "Bad",
    is.na(EQR) ~ "No Data"
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad"))) %>%
  st_as_sf() %>% filter(!is.na(SpeciesGroup))

#plot MESH map
macaronesia_plot <- ggplot() +
  geom_sf(data = macaronesia_spec_data, aes(geometry = geometry,fill = Status)) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" =  "green",
      "High" ="blue"
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  facet_wrap(~SpeciesGroup, ncol = 2) 
