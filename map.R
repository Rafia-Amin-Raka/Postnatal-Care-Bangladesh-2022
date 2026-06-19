#Packages
library(tidyverse)
library(haven)
library(sf)
library(srvyr)          
library(scico)          
library(ggtext)         
library(shadowtext)     
library(patchwork)      
library(scales)
library(ggspatial)  
library(survey)
library(ggplot2)
library(dplyr)
library(viridis)
library(geodata)
library(tidyr)



bgd_adm2 <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm2.rds")
bgd_adm1 <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm1.rds")
map_data <- readRDS("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/map_data.rds")
#  Weighted district-level PNC % 
# dist_pnc_spatial <- map_data %>%
#   filter(!is.na(ADM2NAME), !is.na(PNC_care)) %>%
#   mutate( wt = V005 / 1e6,
#     pnc_bin = ifelse(PNC_care == "1", 1, 0)
#   ) %>%
#   group_by(ADM2NAME) %>%
#   summarise(
#     n_total = n(),
#     n_yes = sum(pnc_bin == 1, na.rm = TRUE),
#     n_no = sum(pnc_bin == 0, na.rm = TRUE),
#     pct_yes_wtd = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100,
#     pct_no_wtd = 100 - pct_yes_wtd,
#     .groups = "drop"
#   ) %>%
#   mutate(
#     Total = paste0(n_total, " (100%)"),
#     Yes = paste0(n_yes, " (", round(pct_yes_wtd, 1), "%)"),
#     No = paste0(n_no, " (", round(pct_no_wtd, 1), "%)")
#   ) %>%
#   rename(NAME_2 = ADM2NAME)
# bgd_adm2_pnc <- bgd_adm2 %>%
#   left_join(dist_pnc_spatial, by = "NAME_2")
# unmatched <- bgd_adm2_pnc %>%
#   filter(is.na(pct_yes_wtd)) %>%
#   pull(NAME_2)
# if(length(unmatched) > 0){
#   cat("\n⚠ Unmatched districts:\n")
#   print(unmatched)
# }
# bgd_adm2_label <- bgd_adm2_pnc %>%
#   filter(!is.na(pct_yes_wtd)) %>%
#   mutate(label = paste0(NAME_2,"\n",round(pct_yes_wtd, 1),"%\n(n=",n_total,")"
#     ),
#     lon = st_coordinates(st_centroid(geometry))[,1],
#     lat = st_coordinates(st_centroid(geometry))[,2]
#   )
# print(bgd_adm2_pnc)
# write.csv(
#   st_drop_geometry(bgd_adm2_pnc),
#   "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/district_pnc_spatial_table.csv",
#   row.names = FALSE)



district_pnc <- map_data %>%
  filter(!is.na(ADM2NAME)) %>%
  mutate(wt      = V005 / 1e6,
         pnc_bin = as.integer(PNC_care == "1")) %>%
  group_by(ADM2NAME) %>%
  summarise(
    district_PNC = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100,
    n_clusters   = n(),
    .groups      = "drop"
  )

cat("\n── District PNC summary ──\n")
print(arrange(district_pnc, desc(district_PNC)))
# write.csv(  district_pnc,
#   "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/model 3/data/district_pnc_weighted_table.csv",
#   row.names = FALSE)


#Merge with shapefile 
bgd_adm2_pnc <- left_join(bgd_adm2,district_pnc,by = c("NAME_2" = "ADM2NAME"))

#Check unmatched districts
unmatched <- bgd_adm2_pnc %>%
  filter(is.na(district_PNC)) %>%
  pull(NAME_2)

if (length(unmatched) > 0) {
  cat("\n Unmatched districts (check spelling):\n")
  print(unmatched)
}

# district_pnc <- district_pnc %>%
#   mutate(ADM2NAME = recode(ADM2NAME, "Brahmanbaria" = "Brahman Baria"))
#  District centroids for labels 

bgd_adm2_label <- bgd_adm2_pnc %>%
  mutate(
    label = paste0(NAME_2, "\n", round(district_PNC, 1), "%\n(n=", n_clusters, ")"),
    lon   = st_coordinates(st_centroid(geometry))[, 1],
    lat   = st_coordinates(st_centroid(geometry))[, 2]
  )




# ── DIVISION-LEVEL PNC MAP ────────────────────────────────────────────────────

Weighted division-level PNC % 
divi_pnc_spatial <- map_data %>%
  filter(!is.na(ADM1NAME), !is.na(PNC_care)) %>%
  mutate( wt = V005 / 1e6,pnc_bin = ifelse(PNC_care == "1", 1, 0)
  ) %>%
  group_by(ADM1NAME) %>%
  summarise(n_total = n(),
    n_yes = sum(pnc_bin == 1, na.rm = TRUE),
    n_no = sum(pnc_bin == 0, na.rm = TRUE),
    pct_yes_wtd = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100,
    pct_no_wtd = 100 - pct_yes_wtd,
    .groups = "drop"
  ) %>%
  mutate(
    Total = paste0(n_total, " (100%)"),
    Yes = paste0(n_yes, " (", round(pct_yes_wtd, 1), "%)"),
    No = paste0(n_no, " (", round(pct_no_wtd, 1), "%)")
  ) %>%
  rename(NAME_1 = ADM1NAME)
bgd_adm1_pnc <- bgd_adm1 %>%
  left_join(divi_pnc_spatial, by = "NAME_1") %>%
  filter(!is.na(pct_yes_wtd))
bgd_adm1_label <- bgd_adm1_pnc %>%
  mutate(label = paste0(NAME_1,"\n",round(pct_yes_wtd, 1),
      "%\n(n=",n_total,")"),
    lon = st_coordinates(st_centroid(geometry))[,1],
    lat = st_coordinates(st_centroid(geometry))[,2]
  )
print(bgd_adm1_pnc)
write.csv(
  st_drop_geometry(bgd_adm1_pnc),
  "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/division_pnc_spatial_table.csv",
  row.names = FALSE)




division_pnc <- map_data %>%
  filter(!is.na(ADM1NAME)) %>%
  mutate(wt      = V005 / 1e6,
         pnc_bin = as.integer(PNC_care == "1")) %>%
  group_by(ADM1NAME) %>%
  summarise(
    division_PNC = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100,
    n_women      = n(),
    .groups      = "drop"
  )
cat("\n── Division PNC summary ──\n")
print(arrange(division_pnc, desc(division_PNC)))

#Merge with division shapefile 
bgd_adm1_pnc <- left_join(bgd_adm1,division_pnc,by = c("NAME_1" = "ADM1NAME"))

# Check unmatched
unmatched_div <- bgd_adm1_pnc %>% filter(is.na(division_PNC)) %>% pull(NAME_1)
if (length(unmatched_div) > 0) {
  cat("\n⚠ Unmatched divisions:\n"); print(unmatched_div)
}

#Division centroids for labels
bgd_adm1_label <- bgd_adm1_pnc %>%
  mutate(
    label = paste0(NAME_1, "\n", round(division_PNC, 1), "%\n(n=", n_women, " )"),
    lon   = st_coordinates(st_centroid(geometry))[, 1],
    lat   = st_coordinates(st_centroid(geometry))[, 2]
  )

#small size____________________
p_div <- ggplot() +
  # Division choropleth fill 
  geom_sf(data = bgd_adm1_pnc,aes(fill= division_PNC),color= "white",linewidth = 0.4
  ) +
  # Division labels: name + % + n 
  geom_text(data = bgd_adm1_label,aes(x = lon, y = lat, label = label),
            size = 2.5,fontface = "bold",color = "black",lineheight    = 0.88,check_overlap = FALSE
  ) +
  scale_fill_viridis_c(
    option    = "viridis",
    direction = 1,
    name      = "% PNC Utilization",
    limits    = c(0, 100),
    breaks    = seq(0, 100, 25),
    labels    = paste0(seq(0, 100, 25), "%"),
    na.value  = "#c9b99a",
    guide     = guide_colorbar( 
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(4, "cm"),    # adjusted to fit map width
      barheight      = unit(0.3, "cm"),  # slightly thinner
      frame.colour   = NA,
      ticks.colour   = "#6b4f3a"
    )
  ) +
  coord_sf( xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 9) +
  theme(
    axis.text        = element_text(size = 5, colour = "#3b2f2f"),  
    axis.title       = element_blank(),                              
    axis.ticks       = element_line(colour = "#6b4f3a", linewidth = 0.5),
    panel.grid.major = element_line(colour = "white", linewidth = 0.3,
                                    linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "#6b4f3a", fill = NA,
                                    linewidth = 1.2),
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position   = c(0.33, 0.08),
    legend.direction  = "horizontal",
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.title      = element_text(face = "bold", size = 10,
                                     colour = "#3b2f2f"),
    legend.text       = element_text(size = 7, colour = "#3b2f2f"),
    plot.margin = margin(5, 5, 5, 5)
  )
print(p_div)
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/division_PNC_map.tiff",
       plot = p_div, dpi= 600,width= 9,height= 11,units= "cm",compression="lzw",bg= "white")
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/division_PNC_map.png",
       plot = p_div,dpi= 300,width= 9,height = 11,units = "cm",bg = "white")

























#__________________district level ________________________________________________
#===small size
p_dist <- ggplot() +
  geom_sf(data= bgd_adm2_pnc,aes(fill  = district_PNC),color= "white",linewidth = 0.15
  ) +
  geom_sf(data = bgd_adm1,fill= NA,color= "#f0e8d8",linewidth = 0.6
  ) +
  geom_text(data= bgd_adm2_label, aes(x = lon, y = lat, label = label),
    size = 1.5,fontface = "plain",color = "black",lineheight    = 0.8,
    check_overlap = FALSE 
  ) +
  scale_fill_viridis_c(
    option    = "viridis",
    direction = 1,
    name      = "% PNC Utilization",
    limits    = c(0, 100),
    breaks    = seq(0, 100, 25),
    labels    = paste0(seq(0, 100, 25), "%"),
    na.value  = "#c9b99a",
    guide     = guide_colorbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(5, "cm"),
      barheight      = unit(0.5, "cm"),
      frame.colour   = NA,
      ticks.colour   = "#6b4f3a"
    )
  ) +
  coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 9) +
  theme(
    axis.text        = element_text(size = 5, colour = "#3b2f2f"),
    axis.title       = element_blank(),
    axis.ticks       = element_line(colour = "#6b4f3a", linewidth = 0.5),
    
    panel.grid.major = element_line(colour = "white", linewidth = 0.3,
                                    linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "#6b4f3a", fill = NA,
                                    linewidth = 1.2),
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position   = c(0.37, 0.08),
    legend.direction  = "horizontal",
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.title      = element_text(face = "bold", size = 14,
                                     colour = "#3b2f2f"),
    legend.text       = element_text(size = 10, colour = "#3b2f2f"),
    plot.margin = margin(5, 5, 5, 5)
  )
print(p_dist)
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/district_PNC_map.tiff",
       plot = p_dist,dpi= 600,width= 13,height= 15,units= "cm",compression = "lzw",bg = "white")
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/district_PNC_map.png",
       plot = p_dist,dpi = 300,width  = 13,height = 15,units  = "cm",bg= "white")






#spatial autocorrelation
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)
library(gridExtra)

# Spatial weights 
knn    <- knearneigh(coords, k = 4)
nb     <- knn2nb(knn)
lw     <- nb2listw(nb, style = "W")

#Moran's I 
moran_test <- moran.test(bgd_adm2_pnc$district_PNC, lw)
moran_i <- round(moran_test$estimate["Moran I statistic"], 6)
z_score <- round(moran_test$statistic, 6)
p_value <- format(moran_test$p.value, scientific = FALSE, digits = 6)

 
x_vals   <- seq(-4, 4, length.out = 1000)
curve_df <- data.frame(x = x_vals, y = dnorm(x_vals))
legend_df <- data.frame(xmin  = rep(2.18, 7),
  xmax  = rep(2.45, 7),
  ymin  = c(0.388, 0.350, 0.312, 0.274, 0.236, 0.198, 0.160),
  ymax  = c(0.418, 0.380, 0.342, 0.304, 0.266, 0.228, 0.190),
  fill  = c("#3182bd", "#9ecae1", "#d9f0d3",
            "#f3f0b5",
            "#fdd49e", "#fc8d59", "#e31a1c"),
  pval  = c("0.01", "0.05", "0.10", "---", "0.10", "0.05", "0.01"),
  cval  = c("< -2.58", "-2.58 – -1.96", "-1.96 – -1.65","-1.65 – 1.65","1.65 – 1.96", "1.96 – 2.58", "> 2.58"),
  y_mid = c(0.403, 0.365, 0.327, 0.289, 0.251, 0.213, 0.175))

curve_plot <- ggplot(curve_df, aes(x, y)) +  
  geom_area(data = subset(curve_df, x >= -1.65 & x <= 1.65), fill = "#f3f0b5") +
  geom_area(data = subset(curve_df, x >= 1.65 & x <= 1.96), fill = "#fdd49e") +
  geom_area(data = subset(curve_df, x >= 1.96 & x <= 2.58), fill = "#fc8d59") +
  geom_area(data = subset(curve_df, x > 2.58), fill = "#e31a1c") +
  geom_area(data = subset(curve_df, x >= -1.96 & x <= -1.65), fill = "#d9f0d3") +
  geom_area(data = subset(curve_df, x >= -2.58 & x <= -1.96), fill = "#9ecae1") +
  geom_area(data = subset(curve_df, x < -2.58), fill = "#3182bd") +
  geom_line(linewidth = 1, colour = "black") + 
  geom_segment(aes(x = z_score, xend = z_score, y = 0, yend = -0.07),colour = "red",linewidth = 1.2,linetype = "dotted") +
  annotate("text", x = -3.95, y = 0.40,hjust = 0, size = 3.8,fontface = "bold",label = paste0("Moran's Index: ", moran_i,
      "\nz-score: ", z_score,"\np-value: ", p_value)) +  
  annotate( "text",x = 0,y = 0.13,label = "(Random)", size = 5,fontface = "bold") +
  annotate("segment",x = -3.5,xend = -1.7,y = 0.08,yend = 0.08,arrow = arrow(length = unit(0.18, "cm")),linewidth = 0.8) +  
  annotate("segment",x = 2,xend = 3.8, y = 0.08, yend = 0.08,arrow = arrow(length = unit(0.18, "cm")),linewidth = 0.8) +   
  annotate("text", x = -2.3,y = 0.045,label = "Significant",size = 4.2,fontface = "bold") +  
  annotate("text",x = 3,y = 0.045,label = "Significant",size = 4.2,fontface = "bold") +  
  annotate("text",x = 2.30,y = 0.438,hjust = 0.5,size = 3.5,fontface = "bold",label = "Significance Level\n(p-value)") +  
  annotate("text",x = 3.10,y = 0.438,hjust = 0,size = 3.5,fontface = "bold",label = "Critical Value\n(z-score)") +  
  geom_rect(data = legend_df,aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax,fill = fill),inherit.aes = FALSE,color = "gray30",linewidth = 0.25) +  
  scale_fill_identity() +
  geom_text(data = legend_df, aes(x = xmin - 0.04,y = y_mid,label = pval),inherit.aes = FALSE,hjust = 1,size = 3.1,fontface = "bold") +  
  geom_text(data = legend_df, aes(x = xmax + 0.04,y = y_mid,label = cval),inherit.aes = FALSE,hjust = 0,size = 3.1,fontface = "bold") +  
  coord_cartesian(xlim = c(-4, 4),ylim = c(-0.02, 0.42),clip = "off") + 
  theme_void() +  
  theme(plot.margin = margin(5, 5, 0, 5), plot.background = element_rect(fill = "white",colour = NA))

set.seed(123)
bgd_adm2_pnc$disp_fill <- runif(nrow(bgd_adm2_pnc),0,100)
bgd_adm2_pnc$random_fill <- sample(bgd_adm2_pnc$district_PNC)
map_dispersed <- ggplot() +  
  geom_sf(data = bgd_adm2_pnc,aes(fill = disp_fill),color = "gray70",linewidth = 0.15) +  
  scale_fill_gradient(low = "white",high = "gray20") +
  theme_void() +
  
  theme(legend.position = "none",panel.border = element_rect(colour = "black",fill = NA,linewidth = 0.8),)
map_random <- ggplot() + 
  geom_sf(data = bgd_adm2_pnc, aes(fill = random_fill),color = "gray70",linewidth = 0.15) +  
  scale_fill_gradient(low = "white",high = "gray20") +  
  theme_void() +  
  theme(legend.position = "none",panel.border = element_rect(colour = "black",fill = NA,linewidth = 0.8 ),   
    plot.margin = margin(2, 2, 15, 2))
map_clustered <- ggplot() +  
  geom_sf(data = bgd_adm2_pnc,aes(fill = district_PNC),color = "gray70",linewidth = 0.15) +  
  scale_fill_gradient(low = "white",high = "gray20"
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "none",
    panel.border = element_rect(
      colour = "red",
      fill = NA,
      linewidth = 1.5
    ),
    
  
    plot.margin = margin(2, 2, 15, 2)
  )


map1 <- ggdraw(map_dispersed) +
  draw_label(
    "Dispersed",
    x = 0.5,
    y = -0.03,
    size = 12,
    fontface = "bold"
  )

map2 <- ggdraw(map_random) +
  draw_label(
    "Random",
    x = 0.5,
    y = -0.03,
    size = 12,
    fontface = "bold"
  )

map3 <- ggdraw(map_clustered) +
  draw_label(
    "Clustered",
    x = 0.5,
    y = -0.03,
    size = 12,
    fontface = "bold"
  )


bottom_maps <- map1 + map2 + map3 +
  plot_layout(ncol = 3)
final_moran_plot <- curve_plot /  bottom_maps +  plot_layout(heights = c(3, 1)  )
#print(final_moran_plot)
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/moran_full_plot.tiff",
  plot = final_moran_plot,dpi = 600,width = 18,height = 18,units = "cm",compression = "lzw",bg = "white")
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/moran_full_plot3.png",
  plot = final_moran_plot,dpi = 600,width = 18,height = 18,units = "cm",bg = "white")


























#===============Hotspot analysis==================================
library(sf)
library(dplyr)
library(spdep)
library(ggplot2)
library(ggspatial)

bgd_adm2_pnc <- left_join(bgd_adm2,district_pnc,by = c("NAME_2" = "ADM2NAME"))
# Create neighbors
coords <- st_coordinates(st_centroid(bgd_adm2_pnc))

knn <- knearneigh(coords, k = 4)
nb  <- knn2nb(knn)
lw  <- nb2listw(nb, style = "W")

# Getis-Ord Gi
gi <- localG(bgd_adm2_pnc$district_PNC,lw)
bgd_adm2_pnc$GiZScore <- as.numeric(gi)
# Hotspot categories
bgd_adm2_pnc <- bgd_adm2_pnc %>%
  mutate(Hotspot = case_when(
      GiZScore >= 2.58  ~ "99% Hot Spot",
      GiZScore >= 1.96  ~ "95% Hot Spot",
      GiZScore >= 1.65  ~ "90% Hot Spot",
      GiZScore <= -2.58 ~ "99% Cold Spot",
      GiZScore <= -1.96 ~ "95% Cold Spot",
      GiZScore <= -1.65 ~ "90% Cold Spot",
      TRUE ~ "Not Significant"))

p_hotspot <- ggplot() +  
  geom_sf(data = bgd_adm2_pnc,aes(fill = Hotspot),color= "#bdbdbd",linewidth = 0.25) +  
  geom_sf(data = bgd_adm1,fill = NA,color = "black",linewidth = 0.3) +  
  scale_fill_manual(values = c(
      "99% Hot Spot" = "#b2182b",
      "95% Hot Spot" = "#ef6548",
      "90% Hot Spot" = "#fdd0a2",
      "99% Cold Spot" = "#08306b",
      "95% Cold Spot" = "#2171b5",
      "90% Cold Spot" = "#6baed6",
      "Not Significant" = "gray95"),    
    name = "Hotspot Analysis") +
  
  coord_sf(xlim = c(88.0, 92.7),ylim = c(20.7, 26.7),expand = FALSE) +  
  annotation_scale(location = "tr",width_hint = 0.22,text_cex = 0.5,height = unit(0.15, "cm")) +  
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering,height = unit(0.8, "cm"),width = unit(0.8, "cm"),
    pad_y = unit(0.7, "cm")) +  
  labs(x = NULL, y = NULL,) +  
  theme_bw(base_size = 7) +  
  theme(axis.text = element_text( size = 4.5, colour = "#3b2f2f"),    
    axis.title = element_blank(),    
    axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.4),    
    panel.grid.major = element_line(colour = "white",linewidth = 0.2,linetype = "dotted"),    
    panel.grid.minor = element_blank(),    
    panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 0.8),    
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),     
    legend.position = "right",    
    legend.direction = "vertical",    
    legend.background = element_blank(),
    legend.key = element_blank(),    
    legend.title = element_text(face = "bold",size = 5,colour = "#3b2f2f"),    
    legend.text = element_text(size = 4.2,colour = "#3b2f2f"))
#print(p_hotspot)
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/district_hotspot_coldspot.tiff",plot = p_hotspot,
  dpi = 600,width = 8,height = 8,units = "cm",compression = "lzw",bg = "white")
ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/district_hotspot_coldspot.png",plot = p_hotspot,
  dpi = 600,width = 8, height = 8,units = "cm",bg = "white")





















# ── CLUSTER-LEVEL PNC MAP ─────────────────────────────────────────────────────
#Weighted cluster-level PNC %
clus_pnc_spatial <- map_data %>%
  filter(!is.na(V001), !is.na(PNC_care)) %>%
  mutate( wt = V005 / 1e6, pnc_bin = ifelse(PNC_care == "1", 1, 0)) %>%
  group_by(V001) %>%
  summarise(n_total = n(),n_yes = sum(pnc_bin == 1, na.rm = TRUE),n_no = sum(pnc_bin == 0, na.rm = TRUE),pct_yes_wtd = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100,
    pct_no_wtd = 100 - pct_yes_wtd,.groups = "drop") %>%
  mutate( Total = paste0(n_total, " (100%)"), Yes = paste0(n_yes, " (", round(pct_yes_wtd, 1), "%)"), No = paste0(n_no, "(", round(pct_no_wtd, 1),"%)")) %>%
  rename(DHSCLUST = V001)
clus_pnc_spatial_map <- gps_data %>% select(DHSCLUST, geometry) %>% left_join(clus_pnc_spatial, by = "DHSCLUST") %>%  filter(!is.na(pct_yes_wtd))
print(clus_pnc_spatial_map)
write.csv(st_drop_geometry(clus_pnc_spatial_map),"C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/clus_pnc_spatial_table.csv",
  row.names = FALSE)




# cluster_pnc <- map_data %>%
#   filter(!is.na(V001)) %>%
#   mutate(wt = V005 / 1e6, pnc_bin = as.integer(PNC_care == "1")) %>%
#   group_by(V001) %>%                             
#   summarise(cluster_PNC = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100,
#     n_women= n(), .groups= "drop"  )
# cat("\n── Cluster PNC summary ──\n")
# cat("Total clusters:", nrow(cluster_pnc), "\n")
# print(summary(cluster_pnc$cluster_PNC))

# # Join cluster PNC %to GPS coordinates
# cluster_map <- left_join(gps_data %>% select(DHSCLUST, geometry), cluster_pnc,by = c("DHSCLUST" = "V001")) %>%
#   filter(!is.na(cluster_PNC))                   
# p_clust <- ggplot() +
#     geom_sf(data = bgd_adm2,fill = "gray95",color= "#bdbdbd",linewidth = 0.3) +
#   geom_sf(data= bgd_adm1,fill = NA,color= "black",linewidth = 0.3) +
#   geom_sf(data= cluster_map,aes(color = cluster_PNC),size  = 0.8 ,alpha = 0.8) +
#   scale_color_viridis_c( option    = "viridis",
#     name      = "% PNC Utilization",
#     limits    = c(0, 100),
#     breaks    = seq(0, 100, 25),
#     labels    = paste0(seq(0, 100, 25), "%"),
#     na.value  = "#c9b99a",
#     guide     = guide_colorbar(title.position = "top",title.hjust = 0.5,barwidth = unit(4, "cm"), barheight = unit(0.3, "cm"),frame.colour   = NA,
#       ticks.colour = "#6b4f3a",)) +
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE) +
#   labs(x = NULL, y = NULL) +
#   theme_bw(base_size = 9) +
#   theme(axis.text        = element_text(size = 5, colour = "#3b2f2f"),
#     axis.title       = element_blank(),
#     axis.ticks       = element_line(colour = "#6b4f3a", linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white", linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border     = element_rect(colour = "#6b4f3a", fill = NA, linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background  = element_rect(fill = "white", color = NA),
#     legend.position   = c(0.33, 0.08),
#     legend.direction  = "horizontal",
#     legend.background = element_blank(),
#     legend.key        = element_blank(),
#     legend.title      = element_text(face = "bold", size = 10, colour = "#3b2f2f"),
#     legend.text       = element_text(size = 7, colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_clust)
# ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/cluster_PNC_map.tiff",
#        plot = p_clust,dpi = 600,width= 9,height= 11,units= "cm",compression = "lzw",bg= "white")
# ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/cluster_PNC_map.png",
#        plot = p_clust,dpi = 300,width  = 9,height = 11,units  = "cm",bg = "white")







#alll 3 cluster plot

# Weighted cluster prevalence
cluster_pnc <- map_data %>% filter(!is.na(V001)) %>% mutate(wt = V005 / 1e6, pnc_bin = as.integer(PNC_care == "1")) %>%
  group_by(V001, Residence) %>%
  summarise(cluster_PNC = weighted.mean(pnc_bin, wt, na.rm = TRUE) * 100, n_women = n(),.groups = "drop")

# Join GPS
cluster_map <- left_join  gps_data %>% select(DHSCLUST, geometry),
  cluster_pnc, by = c("DHSCLUST" = "V001")) %>% filter(!is.na(cluster_PNC))


cluster_all <- cluster_map
cluster_urban <- cluster_map %>% filter(Residence == 1)
cluster_rural <- cluster_map %>% filter(Residence == 2)

plot_cluster_map <- function(data_map, title_text) {
  ggplot() +
    geom_sf(data = bgd_adm2,fill = "gray95",color = "#bdbdbd",linewidth = 0.25) +
    geom_sf(data = bgd_adm1,fill = NA,color = "black",linewidth = 0.3) +
    geom_sf(data = data_map,aes(color = cluster_PNC),size = 0.6,alpha = 0.8) +
    scale_color_viridis_c(option = "viridis",name = "% PNC Utilization",
      limits = c(0, 100),breaks = seq(0, 100, 25),labels = paste0(seq(0, 100, 25), "%"),
      guide = guide_colorbar(title.position = "top",title.hjust = 0.5,
        barwidth = unit(2.8, "cm"),barheight = unit(0.22, "cm"), frame.colour = NA,ticks.colour = "#6b4f3a")) +
    coord_sf(xlim = c(88.0, 92.7),ylim = c(20.7, 26.7),expand = FALSE) +
    annotation_scale(location = "tr",width_hint = 0.22,text_cex = 0.5, height = unit(0.15, "cm"))+
    annotation_north_arrow( location = "tr", which_north = "true", style = north_arrow_fancy_orienteering,
                            height = unit(0.8, "cm"), width = unit(0.8, "cm"), pad_y = unit(0.7, "cm") ) +
    labs(x = NULL,y = NULL,title = title_text) +
    theme_bw(base_size = 7) +
    theme(plot.title = element_text(face = "bold",size = 8,hjust = 0.2,colour = "#3b2f2f",margin = margin(b = 2)),
      axis.text = element_text(size = 4.5,colour = "#3b2f2f"),
      axis.title = element_blank(),
      axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.4),
      panel.grid.major = element_line(colour = "white",linewidth = 0.2,linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 0.8),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = c(0.33, 0.08),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(face = "bold",size = 4.8,colour = "#3b2f2f"),
      legend.text = element_text(size = 4,colour = "#3b2f2f"),
      plot.margin = margin(2, 2, 2, 2)    )
}
p_all <- plot_cluster_map(cluster_all, "Overall")
p_urban <- plot_cluster_map(cluster_urban, "Urban")
p_rural <- plot_cluster_map(cluster_rural, "Rural")
cluster_panel <- wrap_plots(p_all, p_urban,p_rural,ncol = 3)
#print(cluster_panel)
ggsave( "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/cluster_panel_overall_urban_rural.tiff",
  plot = cluster_panel,  dpi = 600,width = 18,height = 9,units = "cm",compression = "lzw",bg = "white")
ggsave( "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/cluster_panel_overall_urban_rural.png",
  plot = cluster_panel,  dpi = 600,width = 18,height = 9,units = "cm",bg = "white")
cat("Saved → cluster_panel_overall_urban_rural\n")































# #panel at right side
# p_clust = ggplot() +
#   
#   #District borders 
#   geom_sf(
#     data      = bgd_adm2,
#     fill      = "gray95",         # district fill colour
#     color     = "#bdbdbd",        # district border colour — change here
#     linewidth = 0.3               # district border thickness — change here
#   ) +
#   #Division borders
#   geom_sf(
#     data      = bgd_adm1,
#     fill      = NA,
#     color     = "black",          # division border colour — change here
#     linewidth = 0.5               # division border thickness — change here
#   ) +
#   
#   geom_sf(
#     data  = cluster_map,
#     aes(color = cluster_PNC),
#     size  = 3,
#     alpha = 0.8
#   ) +
#   
#   scale_color_viridis_c(
#     option = "virdis",
#     name   = "PNC (%)"
#   ) +
#   
#   theme_minimal()
# 
# p_clust
# #Save
# ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/pnc_cluster.png",
#   plot  = p_clust,width = 12, height = 10, units = "in",dpi   = 1200, bg = "white")
# ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/pnc_cluster.tiff",
#   plot= p_clust,width = 12, height = 10, units = "in",dpi = 600, bg = "white", compression = "lzw")
# cat("\n✔ Cluster map saved successfully.\n")


































#=======================shap maps=======================================================
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(patchwork)
library(grid)

bgd_adm1 <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm1.rds")
div_shap <- read.csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/xgb/division_wise_shap_value_xgb.csv")

bd_shap_div <- bgd_adm1 %>%left_join(div_shap, by = c("NAME_1" = "Division")) %>% filter(!is.na(Place_of_delivery))

# Top 10 variables
features <- c("Place_of_delivery","ANC_visit","Wealth_index","Husbands_education","Mothers_Education","Media_exposure",
  "Wanted_pregnancy","Residence","Age","Husbands_occupation")

nice_labels <- c("Place of Delivery","ANC Visit","Wealth Index", "Husband's Education", "Mother's Education", "Media Exposure",
  "Wanted Pregnancy","Residence","Age","Husband's Occupation")

div_plot_shap <- function(feat, lab) {  
  ggplot(bd_shap_div) +    
    geom_sf(aes_string(fill = feat),color = "white",linewidth = 0.35) +    
    scale_fill_viridis_c(option = "viridis", direction = 1, name = "Mean |SHAP|",     
      guide = guide_colorbar(title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(2, "cm"),
        barheight = unit(0.22, "cm"),
        frame.colour = NA,
        ticks.colour = "#6b4f3a"
      )
    ) +
    
    coord_sf(
      xlim = c(88.0, 92.7),
      ylim = c(20.7, 26.7),
      expand = FALSE
    ) +
    annotation_scale(location = "tr",width_hint = 0.22,text_cex = 0.35, height = unit(0.15, "cm"))+
    annotation_north_arrow( location = "tr", which_north = "true", style = north_arrow_fancy_orienteering,
                            height = unit(0.6, "cm"), width = unit(0.6, "cm"), pad_y = unit(0.6, "cm") ) +
    
    
    labs(
      x = NULL,
      y = NULL,
      title = lab
    ) +
    
    theme_bw(base_size = 6) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 7,
        hjust = 0.2,
        colour = "#3b2f2f",
        margin = margin(b = 2)
      ),
      
      axis.text = element_text(size = 4.5,colour = "#3b2f2f"),
      axis.title = element_blank(),
      axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.25 ),
      
      panel.grid.major = element_line(
        colour = "white",
        linewidth = 0.15,
        linetype = "dotted"
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.border = element_rect(
        colour = "#6b4f3a",
        fill = NA,
        linewidth = 0.7
      ),
      
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      
      legend.position = c(0.33, 0.09),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.key = element_blank(),
      
      legend.title = element_text(
        face = "bold",
        size = 4.5,
        colour = "#3b2f2f"
      ),
      
      legend.text = element_text(
        size = 4,
        colour = "#3b2f2f"
      ),
      
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Build maps
map_list <- mapply(
  div_plot_shap,
  features,
  nice_labels,
  SIMPLIFY = FALSE
)

# Combine maps
div_shap_panel <- wrap_plots(
  map_list,
  ncol = 5,
  nrow = 2
)

# Show plot
#print(div_shap_panel)

# Save TIFF
ggsave(
  "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/xgb/division_shap_top10_panel.tiff",
  plot = div_shap_panel,
  dpi = 600,
  width = 20,
  height = 12,
  units = "cm",
  compression = "lzw",
  bg = "white"
)

# Save PNG
ggsave(
  "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/xgb/division_shap_top10_panel.png",
  plot = div_shap_panel,
  dpi = 600,
  width = 20,
  height = 12,
  units = "cm",
  bg = "white"
)

cat("Saved → division_shap_top10_panel\n")





















#==============district wise shap map===================

# dist_shap = read.csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/dist_wise_shap_value.csv")
# dist_shap
# 
# # ── Merge SHAP values with district shapefile
# bd_shap_dist <- bgd_adm2 %>%
#   left_join(dist_shap, by = c("NAME_2" = "District")) %>%
#   filter(!is.na(Place_of_delivery))
# 
# #Features and clean labels 
# features <- c( "Place_of_delivery","ANC_visit", "Wealth_index","Husbands_education", "Mothers_Education",
#                "Media_exposure" ,"Wanted_pregnancy","Residence", "Age", "Husbands_occupation","Birth_order" , "Religion" )
# 
# nice_labels <- c("Place of Delivery","ANC_visit", "Wealth Index","Husband's Education","Mother's Education", 
#                  "Media_exposure" , "Wanted Pregnancy","Residence", "Age", "Husband's Occupation","Birth_order" , "Religion")
# 
# # ── Plot function 
# dist_plot_shap <- function(feat, lab) {
#   ggplot(bd_shap_dist) +
#     geom_sf(aes_string(fill = feat), color = "white", linewidth = 0.4) +
#     scale_fill_viridis_c(
#       option    = "viridis",
#       direction = 1,
#       name      = "Mean |SHAP|",
#       guide     = guide_colorbar(
#         title.position = "top",
#         title.hjust    = 0.5,
#         barwidth       = unit(2.5, "cm"),
#         barheight      = unit(0.25, "cm"),
#         frame.colour   = NA,
#         ticks.colour   = "#6b4f3a"
#       )
#     ) +
#     coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#     ) +
#     labs(x = NULL, y = NULL, title = lab) +
#     theme_bw(base_size = 7) +
#     theme(
#       plot.title       = element_text(face = "bold", size = 9,
#                                       hjust = 0.5, colour = "#3b2f2f"),
#       axis.text        = element_blank(),
#       axis.title       = element_blank(),
#       axis.ticks       = element_blank(),
#       panel.grid.major = element_line(colour = "white", linewidth = 0.2,
#                                       linetype = "dotted"),
#       panel.grid.minor = element_blank(),
#       panel.border     = element_rect(colour = "#6b4f3a", fill = NA,
#                                       linewidth = 0.8),
#       panel.background = element_rect(fill = "white"),
#       plot.background  = element_rect(fill = "white", color = NA),
#       legend.position   = c(0.33, 0.08),
#       legend.direction  = "horizontal",
#       legend.background = element_blank(),
#       legend.key        = element_blank(),
#       legend.title      = element_text(face = "bold", size = 5,
#                                        colour = "#3b2f2f"),
#       legend.text       = element_text(size = 4.5, colour = "#3b2f2f"),
#       plot.margin       = margin(2.5, 3, 2.5, 3)
#     )
# }
# 
# map_list <- mapply(dist_plot_shap,features,nice_labels,SIMPLIFY = FALSE)
# dist_shap_plot_panel <- wrap_plots(map_list, ncol = 3)
# print(dist_shap_plot_panel)
# 
# # ── Save 
# ggsave("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/district_shap_panel.tiff",
#   plot = dist_shap_plot_panel,dpi = 600,width = 18,height = 22,units = "cm",compression = "lzw",bg = "white")
# 










library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)
library(grid)

# Load district shapefile
bgd_adm2 <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm2.rds")

# Load district SHAP values
dist_shap <- read.csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/xgb/district_wise_shap_value_xgb.csv")

# Merge
bd_shap_dist <- bgd_adm2 %>%
  left_join(dist_shap, by = c("NAME_2" = "District")) %>%
  filter(!is.na(Place_of_delivery))

# Top 10 variables
features <- c(
  "Place_of_delivery",
  "ANC_visit",
  "Wealth_index",
  "Husbands_education",
  "Mothers_Education",
  "Media_exposure",
  "Wanted_pregnancy",
  "Residence",
  "Age",
  "Husbands_occupation"
)

# Labels
nice_labels <- c(
  "Place of Delivery",
  "ANC Visit",
  "Wealth Index",
  "Husband's Education",
  "Mother's Education",
  "Media Exposure",
  "Wanted Pregnancy",
  "Residence",
  "Age",
  "Husband's Occupation"
)

# Plot function
dist_plot_shap <- function(feat, lab) {
  
  ggplot(bd_shap_dist) +
    
    geom_sf(
      aes_string(fill = feat),
      color = "white",
      linewidth = 0.25
    ) +
    
    scale_fill_viridis_c(
      option = "viridis",
      direction = 1,
      name = "Mean |SHAP|",
      
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(1.8, "cm"),
        barheight = unit(0.2, "cm"),
        frame.colour = NA,
        ticks.colour = "#6b4f3a"
      )
    ) +
    
    coord_sf(
      xlim = c(88.0, 92.7),
      ylim = c(20.7, 26.7),
      expand = FALSE
    ) +
    annotation_scale(location = "tr",width_hint = 0.22,text_cex = 0.35, height = unit(0.15, "cm"))+
    annotation_north_arrow( location = "tr", which_north = "true", style = north_arrow_fancy_orienteering,
                            height = unit(0.6, "cm"), width = unit(0.6, "cm"), pad_y = unit(0.6, "cm") ) +
    
   
    labs(
      x = NULL,
      y = NULL,
      title = lab
    ) +
    
    theme_bw(base_size = 6) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 6.5,
        hjust = 0.2,
        colour = "#3b2f2f",
        margin = margin(b = 2)
      ),
      
      # Longitude latitude values
      axis.text = element_text( size = 3.5,colour = "#3b2f2f"),
      
      axis.title = element_blank(),
      
      axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.25 ),
      
      panel.grid.major = element_line(
        colour = "white",
        linewidth = 0.12,
        linetype = "dotted"
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 0.6
      ),
      
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = c(0.33, 0.09),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(face = "bold",size = 4,colour = "#3b2f2f"),
      legend.text = element_text(size = 3.8,colour = "#3b2f2f"),
      
      plot.margin = margin(1, 1, 1, 1)
    )
}

# Create plots
map_list <- mapply(
  dist_plot_shap,
  features,
  nice_labels,
  SIMPLIFY = FALSE
)

# Combine plots
dist_shap_plot_panel <- wrap_plots(
  map_list,
  ncol = 5,
  nrow = 2
)

#print(dist_shap_plot_panel)
# Save TIFF
ggsave(
  "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/xgb/district_shap_top10_panel.tiff",
  plot = dist_shap_plot_panel,
  dpi = 600,
  width = 20,
  height = 12,
  units = "cm",
  compression = "lzw")

# Save PNG
ggsave(
  "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/New folder/xgb/district_shap_top10_panel.png",
  plot = dist_shap_plot_panel,
  dpi = 600,
  width = 20,
  height = 12,
  units = "cm",
  bg = "white"
)

cat("Saved → district_shap_top10_panel\n")















# # Load shapefile and SHAP CSV
# bd_div   <- readRDS("C:/Users/Raka/OneDrive/Thesis/data/bgd_adm1.rds")
# div_shap <- read_csv("C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/xgb/division_wise_shap_value_xgb.csv")
# names(bd_shap)
# 
# # Check name match
# print(sort(bd_div$NAME_1))
# print(sort(div_shap$Division))
# 
# #Fix division name mismatches 
# div_shap <- div_shap %>%
#   mutate(Division = recode(Division,"Barishal"= "Barisal","Chattogram" = "Chittagong"))
# 
# # ── Merge shapefile with SHAP values
# bd_shap <- bd_div %>% left_join(div_shap,by = c("NAME_1" = "Division"))
# 
# # ── Create division label coordinates 
# bd_shap_label <- st_centroid(bd_shap) %>%
#   mutate(
#     lon = st_coordinates(.)[,1],
#     lat = st_coordinates(.)[,2],
#     label = NAME_1
#   ) %>%
#   st_drop_geometry()
# 
# # ── Plot Wealth Index SHAP map
# p_shap_wealth <- ggplot() +
#   # Division polygons
#   geom_sf(data= bd_shap,aes(fill= Wealth_index),color= "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface = "bold",
#     color         = "black",lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(
#     option    = "viridis",
#     direction = 1,
#     name      = "Mean |SHAP|",
#     na.value  = "#c9b99a",
#     guide = guide_colorbar(
#       title.position = "top",
#       title.hjust    = 0.5,
#       barwidth       = unit(4, "cm"),
#       barheight      = unit(0.3, "cm"),
#       frame.colour   = NA,
#       ticks.colour   = "#6b4f3a"
#     )
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate("text",x = 92.4,y = 26.4,label = "Wealth Index",hjust = 1,vjust = 1,
#     size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_shap_wealth)
# # ── Save high-quality TIFF 
# ggsave(filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/wealth_index_shap_map.tiff",
#   plot = p_shap_wealth, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# ggsave(filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/wealth_index_shap_map.png",
#   plot = p_shap_wealth, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# # ── Plot place of delivery SHAP map ─────────────────────────────────────────────
# p_Place_of_delivery <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Place_of_delivery),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#     lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#     guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#       barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Place of Delivery", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Place_of_delivery)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/place_of_delivery_shap_map.tiff",
#   plot = p_Place_of_delivery, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/place_of_delivery_shap_map.png",
#   plot = p_Place_of_delivery, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# # ── Plot Age_group SHAP map ─────────────────────────────────────────────
# p_Age_group <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Age_group),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Age", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Age_group)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Age_group_shap_map.tiff",
#   plot = p_Age_group, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Age_group_shap_map.png",
#   plot = p_Age_group, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# # ── Plot Mothers_Education SHAP map ─────────────────────────────────────────────
# p_Mothers_Education <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Mothers_Education),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Mother's Education", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Mothers_Education)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Mothers_Education_shap_map.tiff",
#   plot = p_Mothers_Education, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Mothers_Education_shap_map.png",
#   plot = p_Mothers_Education, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# 
# 
# # ── Plot Residence SHAP map ─────────────────────────────────────────────
# p_Residence <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Residence),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Residence", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Residence)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Residence_shap_map.tiff",
#   plot = p_Residence, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Residence_shap_map.png",
#   plot = p_Residence, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# # ── Plot Religion SHAP map ─────────────────────────────────────────────
# p_Religion <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Religion),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Religion", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Religion)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Religion_shap_map.tiff",
#   plot = p_Religion, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Religion_shap_map.png",
#   plot = p_Religion, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# # ── Plot Husband_education SHAP map ─────────────────────────────────────────────
# p_Husband_education <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Husband_education),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Husband's Education", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Husband_education)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Husband_education_shap_map.tiff",
#   plot = p_Husband_education, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Husband_education_shap_map.png",
#   plot = p_Husband_education, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# # ── Plot Husband_occupation SHAP map ─────────────────────────────────────────────
# p_Husband_occupation <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Husband_occupation),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Husband's Occupation", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Husband_occupation)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Husband_occupation_shap_map.tiff",
#   plot = p_Husband_occupation, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Husband_occupation_shap_map.png",
#   plot = p_Husband_occupation, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# # ── Plot Wanted_pregnency SHAP map ─────────────────────────────────────────────
# p_Wanted_pregnency <- ggplot() +
#   # Division polygons
#   geom_sf(data      = bd_shap,aes(fill  = Wanted_pregnency),color     = "white",linewidth = 0.4
#   ) +
#   # Division labels
#   geom_text(data = bd_shap_label,aes(x = lon,y = lat,label = label),size = 2.5,fontface= "bold",color= "black",
#             lineheight    = 0.88,check_overlap = FALSE
#   ) +
#   # Color scale
#   scale_fill_viridis_c(option = "viridis",direction = 1,name = "Mean |SHAP|",na.value  = "#c9b99a",
#                        guide = guide_colorbar(title.position = "top",title.hjust    = 0.5,barwidth       = unit(4, "cm"),
#                                               barheight      = unit(0.3, "cm"), frame.colour   = NA,ticks.colour   = "#6b4f3a")
#   ) +
#   # Bangladesh extent
#   coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
#   ) +
#   annotate( "text", x = 92.4,y = 26.4,label = "Wanted Pregnancy", hjust = 1, vjust = 1,size = 3.5,fontface = "bold"
#   ) +
#   # Labels
#   labs(x = NULL,y = NULL
#   ) +
#   # Theme
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text = element_text(size = 5,colour = "#3b2f2f"),
#     axis.title = element_blank(),
#     axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
#     panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white",color = NA),
#     legend.position = c(0.33, 0.08),
#     legend.direction = "horizontal",
#     legend.background = element_blank(),
#     legend.key = element_blank(),
#     legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
#     legend.text = element_text(size = 7,colour = "#3b2f2f"),
#     plot.margin = margin(5, 5, 5, 5)
#   )
# print(p_Wanted_pregnency)
# # ── Save ─────────────────────────────────────────────────
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Wanted_pregnancy_shap_map.tiff",
#   plot = p_Wanted_pregnency, width = 9,height = 11,dpi = 600,units  = "cm",compression = "lzw", bg="white")
# 
# ggsave(
#   filename = "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/data/weighted map/Wanted_pregnancy_shap_map.png",
#   plot = p_Wanted_pregnency, width = 9,height = 11,dpi = 600,units  = "cm", bg="white")
# 
# 
# 
# 
# 
# # # ── Variables to map ───────────────────────────────────────────────────────
# # features <- c(
# #   "Age_group",
# #   "Residence",
# #   "Mothers_Education",
# #   "Wealth_index",
# #   "Wanted_pregnency",
# #   "Religion",
# #   "Place_of_delivery",
# #   "Husband_education" , "Husband_occupation"
# # )
# # 
# # # ── Create all SHAP maps automatically ─────────────────────────────────────
# # shap_maps <- lapply(features, function(var_name) {
# #   
# #   ggplot() +
# #     
# #     # Division polygons
# #     geom_sf(
# #       data      = bd_shap,
# #       aes(fill = .data[[var_name]]),
# #       color     = "white",
# #       linewidth = 0.4
# #     ) +
# #     
# #     # Division labels
# #     geom_text(
# #       data = bd_shap_label,
# #       aes(x = lon, y = lat, label = label),
# #       size = 2.5,
# #       fontface = "bold",
# #       color = "black",
# #       lineheight = 0.88,
# #       check_overlap = FALSE
# #     ) +
# #     
# #     # Color scale
# #     scale_fill_viridis_c(
# #       option    = "viridis",
# #       direction = 1,
# #       name      = "Mean |SHAP|",
# #       na.value  = "#c9b99a",
# #       
# #       guide = guide_colorbar(
# #         title.position = "top",
# #         title.hjust    = 0.5,
# #         barwidth       = unit(4, "cm"),
# #         barheight      = unit(0.3, "cm"),
# #         frame.colour   = NA,
# #         ticks.colour   = "#6b4f3a"
# #       )
# #     ) +
# #     
# #     # Bangladesh extent
# #     coord_sf(xlim   = c(88.0, 92.7),ylim   = c(20.7, 26.7),expand = FALSE
# #     ) +
# #     
# #     # Variable title INSIDE plot
# #     annotate("text",x = 92.4,y = 26.4,label = gsub("_", " ", var_name),
# #       hjust = 1,vjust = 1,size = 3.5,fontface = "bold"
# #     ) +
# #     
# #     labs(x = NULL, y = NULL) +
# #     theme_bw(base_size = 9) +
# #     theme(
# #       axis.text = element_text(size = 5,colour = "#3b2f2f"),
# #       axis.title = element_blank(),
# #       axis.ticks = element_line(colour = "#6b4f3a",linewidth = 0.5),
# #       panel.grid.major = element_line(colour = "white",linewidth = 0.3,linetype = "dotted"),
# #       panel.grid.minor = element_blank(),
# #       panel.border = element_rect(colour = "#6b4f3a",fill = NA,linewidth = 1.2),
# #       panel.background = element_rect(fill = "white"),
# #       plot.background = element_rect(fill = "white",color = NA),
# #       
# #       legend.position = c(0.33, 0.08),
# #       legend.direction = "horizontal",
# #       legend.background = element_blank(),
# #       legend.key = element_blank(),
# #       legend.title = element_text(face = "bold",size = 10,colour = "#3b2f2f"),
# #       legend.text = element_text(size = 7,colour = "#3b2f2f"),
# #       plot.margin = margin(5, 5, 5, 5)
# #     )
# # })
# # 
# # # ── Name the list ──────────────────────────────────────────────────────────
# # names(shap_maps) <- features
# # 
# # # ── Example: show one map ──────────────────────────────────────────────────
# # print(shap_maps[["Wealth_index"]])
# # 
# # # ── Save all maps automatically ────────────────────────────────────────────
# # for(i in seq_along(features)) {
# #   
# #   ggsave(
# #     filename = paste0(
# #       "C:/Users/Raka/OneDrive/Thesis/Raka-Thesis/label encoding/models2/xgb/",
# #       features[i],
# #       "_shap_maps.tiff"
# #     ),
# #     
# #     plot = shap_maps[[i]],
# #     
# #     width = 10,
# #     height = 9,
# #     dpi = 600,
# #     compression = "lzw",
# #     bg= "white"
# #   )
# # }






























