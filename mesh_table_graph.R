# Analysis of the data

grid_area <- grid_minus_land %>%
  mutate(area = st_area(.)) %>%
  st_drop_geometry(.)

# Total area of the grid in km2
Area_assessment <- grid_area %>%
  filter(Include == "Y") %>%
  group_by(Region) %>%
  summarise(AtotGrid = as.numeric(sum(area, na.rm = TRUE)/1e6)) %>% #m2 to km2
  ungroup() %>%
  add_row(., Region = "Europe", AtotGrid = sum(.$AtotGrid)) # Add row with total EU grid area

## Determining fraction of problem and non problem areas - Regions
NPA_PA_regions <- df_MESH %>%
  st_drop_geometry(.) %>%
  left_join(grid_area) %>%
  filter(Include == "Y") %>%
  group_by(Region, Status) %>%
  summarise(AtotAssessed = as.numeric(sum(area, na.rm = TRUE))/10^6) %>% #converting from m^2 to km^2
  left_join(Area_assessment, by = "Region") %>%
  mutate(A_frac = AtotAssessed / AtotGrid ) 

## Determining fraction of problem and non problem areas - All of Europe
NPA_PA_EU <- df_MESH %>%
  st_drop_geometry(.) %>%
  left_join(grid_area) %>%
  filter(Include == "Y") %>%
  group_by(Status) %>%
  summarise(AtotAssessed = as.numeric(sum(area, na.rm = TRUE))/10^6) %>% #converting from m^2 to km^2
  mutate(Region = "Europe") %>%
  left_join(Area_assessment, by = "Region") %>%
  mutate(A_frac = AtotAssessed / AtotGrid ) 

# Combine the two dataframes
NPA_PA_combined <- bind_rows(NPA_PA_regions, NPA_PA_EU) 

NPA_PA_combined_wide <- NPA_PA_combined %>%
  select(Region, Status, A_frac) %>%
  pivot_wider(names_from = Region, values_from = A_frac) 
  #mutate(Status = factor(Status, levels = c("High", "Good", "Moderate", "Poor", "Bad", "No Data", "Not Included"))) 

#Create table
NPA_PA_combined_wide %>%
  select(
    Status,
    `Baltic Sea`,
    `North-east Atlantic Ocean`,
    `Mediterranean Sea`,
    `Black Sea`,
    Europe
  ) %>%
  gt(rowname_col = "Status") %>%
  tab_header(
    title = "Fraction of each status classification in European Seas"
  ) %>%
  cols_label(
    `Baltic Sea` = "Baltic Sea",
    `North-east Atlantic Ocean` = "North-East Atlantic Ocean",
    `Mediterranean Sea` = "Mediterranean Sea",
    `Black Sea` = "Black Sea",
    Europe = "Europe"
  ) %>%
  fmt_percent(
    columns = everything(),
    decimals = 0
  )

### Creating a dataframe with the coverage of each Quality Element (QE) in the assessment regions
QE_area <- left_join(dfEQR_indi, grid_area, by = "GRIDCODE") %>%
  st_drop_geometry(.) %>%
  filter(Include == "Y",
         !is.na(EQR)) %>%
  group_by(QE, Region) %>%
  summarise(AtotQE = as.numeric(sum(area, na.rm = TRUE))/10^6) %>% #converting from m^2 to km^2
  ungroup() %>%
  add_row(Region = c("Europe", "Europe", "Europe","Europe"), 
          QE = c("Biology", "Chemistry", "Supporting", "Litter"), 
          AtotQE = c(sum(.$AtotQE[.$QE == "Biology"]), 
                   sum(.$AtotQE[.$QE == "Chemistry"]), 
                   sum(.$AtotQE[.$QE == "Supporting"]),
                   sum((.$AtotQE[.$QE == "Litter"])))) # Add row with total EU area for each QE))

# Creating  a dataframe with the coverage of MESH in the assessment regions
MESH_area <- left_join(df_MESH, grid_area) %>%
  st_drop_geometry(.) %>%
  filter(Include == "Y",
         !is.na(EQR)) %>%
  group_by(Region) %>%
  summarise(AtotQE = as.numeric(sum(area, na.rm = TRUE))/10^6) %>% #converting from m^2 to km^2
  mutate(QE = "MESH") %>%
  ungroup() %>%
  add_row(Region = "Europe", QE = "MESH", AtotQE = sum(.$AtotQE)) # Add row with total EU MESH area

# Combine the two dataframes and add assessment area coverage and calculate percentage coverage for each QE and the MESH
# Used as the table in powerpoint
dfCombArea <- bind_rows(QE_area, MESH_area) %>%
  left_join(Area_assessment, by = "Region") %>%
  mutate(A_frac = round(AtotQE / AtotGrid, 2)) %>%
  mutate(QE = factor(QE, levels = c("Biology", "Chemistry", "Supporting","Litter", "MESH")),
         Region = factor(Region, levels = c("Baltic Sea", "North-east Atlantic Ocean","Mediterranean Sea","Black Sea", "Europe")))



## Bar plots with assessment units/gridcells in different status classes for each region
# create number of assessment units for each region
n_mesh_reg <- left_join(df,grid_minus_land) %>%
  st_drop_geometry(.) %>%
  select(-c(n_indi,geometry))%>%
  filter(Include == "Y",
         !is.na(EQR)) %>%
  mutate(Status = case_when(
    Include == "N" ~ "Not Included",
    EQR >= 0.8 ~ "High",
    EQR >= 0.6 ~ "Good",
    EQR >= 0.4 ~ "Moderate",
    EQR >= 0.2 ~ "Poor",
    EQR < 0.2 ~ "Bad",
    is.na(EQR) ~ "No Data"
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad"))) %>%
  group_by(Region, QE, Status) %>%
  summarise(n = n()) %>%
  ungroup()


# Create number of assessment units for Europe
n_mesh_EU<-left_join(df,grid_minus_land) %>%
  st_drop_geometry(.) %>%
  select(-c(n_indi,geometry))%>%
  filter(Include == "Y",
         !is.na(EQR)) %>%
  mutate(Status = case_when(
    EQR >= 0.8 ~ "High",
    EQR >= 0.6 ~ "Good",
    EQR >= 0.4 ~ "Moderate",
    EQR >= 0.2 ~ "Poor",
    EQR < 0.2 ~ "Bad"
  ),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad"))) %>%
  group_by(QE, Status) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Region= "Europe") 

#Combine the two dataframes
df_n_mesh <- bind_rows(n_mesh_reg, n_mesh_EU) %>%
  mutate(Status = factor(Status, levels = c("High", "Good", "Moderate", "Poor", "Bad", "No Data", "Not Included"))) %>%
  mutate(Region = factor(Region, levels = c("Baltic Sea", "North-east Atlantic Ocean","Mediterranean Sea","Black Sea", "Europe")))

bar_mesh <- ggplot() +
  geom_col(data = df_n_mesh,
           aes(x = QE, y = n, fill = Status),
           position = "dodge") +
  facet_wrap(~Region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("Bad" = "red", "Poor" = "orange", "Moderate" = "yellow","Good" = "green", "High" = "blue")) +
  theme_minimal() +
  theme(strip.background = element_blank(), # Fjerner baggrund, hvis ønsket
    axis.title.x = element_blank(),
    axis.text = element_text(face = "bold",size = 18),
    legend.position = "bottom",
    legend.text = element_text(face="bold",size = 18),
    legend.title = element_text(face="bold", size = 18),
    axis.title = element_text(face="bold", size = 18),
    plot.title = element_text(face="bold",size = 18),
    plot.title.position = "plot",
    strip.text = element_text(face="bold", hjust=0, size=18)
  ) +
  labs(y = "Number of assessment units", fill = "Status",
       title = NULL)


# save single plot
ggsave("figures/mesh_bar_regions.png",
       plot = bar_mesh,
       width = 16, height = 14, dpi = 300, bg = "white")
