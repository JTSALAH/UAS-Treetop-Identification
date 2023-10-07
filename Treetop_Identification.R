# ---- 0: Load Packages ----

library(ForestTools)
library(raster)
library(sp)
library(sf)
library(rayshader)
library(rgl)
library(tidyverse)

# ---- 1: Create Canopy Height Model ----

  # Note: Ensure projection is in meters.
  dsm = raster(here::here("data", "DSM.tif"))
  dtm = raster(here::here("data", "DTM.tif"))
  chm = dsm - dtm
  
# ---- 2: Detect Tree Tops ----
  
  lin <- function(x){x * 0.1 + 1}      # Adjust function to fit forest characteristics
  ttops <- vwf(chm, winFun = lin, 
               minHeight = 12)         # Adjust minHeight to fit forest characteristics
  
  {
    # Plot CHM
    plot(chm, xlab = "", ylab = "", xaxt='n', yaxt = 'n')
    
    # Add treetops to the plot
    plot(ttops, col = "blue", pch = 20, cex = 0.5, add = TRUE)
  }
  
# ---- 3: Detect Tree Canopies ----
  
  mcws = mcws(ttops, chm,
              minHeight = 5,
              format = "Polygon",
              verbose = TRUE)
  {
    # Plot CHM
    plot(chm)
    
    # Plot Tree Canopy Polygons
    plot(mcws, col = sample(rainbow(50)), border = "blue", lwd = 0.5, add = TRUE)
  }
  
# ---- 4: Calculate Treetop Volume & Statistics ----
  
  summary = sp_summarise(mcws, variables = c("crownArea", "height"))
  
  # Compute crown Diameter & Radius
  mcws[["crownDiameter"]] <- sqrt(mcws[["crownArea"]]/ pi) * 2
  mcws[["crownRadius"]] <- mcws[["crownDiameter"]]/2

  # Computer Canopy Volume (Cone & Hemicircle)
  mcwsvi$Volume_Cone <- 1/3 * pi * mcwsvi$crownRadius^2 * mcwsvi$height
  mcwsvi$Volume_Hemi <- 2/3 * pi * mcwsvi$crownRadius^3
  
  mean(mcwsvi$Volume_Cone) # meters cubed
  mean(mcwsvi$Volume_Hemi) # meters cubed
  
# ---- 5: Render CHM w/ RayShader Package ----
  
  # Reclassify the raster
  rclmat  = matrix(c(-Inf, 0, 0), ncol = 3, byrow = TRUE)
  chm_rcl = reclassify(chm, rcl = rclmat)
  
  # Convert the raster to a data frame
  chm_df = as.data.frame(chm_rcl, xy = TRUE)
  
  # Rename columns for clarity
  names(chm_df) <- c("Longitude", "Latitude", "Value")

  # Reverse the terrain.colors palette and get the first color
  reversed_colors <- rev(terrain.colors(20))
  na_color <- reversed_colors[1]
  
  canopy = ggplot(data = chm_df, aes(x = Longitude, y = Latitude, fill = Value)) +
    geom_raster(interpolate = TRUE) +
    geom_tile(aes(x = Longitude, y = Latitude, fill = Value)) +
    scale_fill_gradientn("Height", 
                         colours = reversed_colors, na.value = na_color,
                         labels = function(x) paste0(x, " m"),
                         guide = guide_colourbar(title.position = "bottom")) +
    coord_equal() +
    scale_y_continuous(position = "right") +
    theme(
      axis.title.y.right = element_text(angle = 90),
      axis.text.y.right = element_text(angle=90),
      legend.position = "left",
      legend.background = element_rect(fill = na_color, colour = na_color),
      legend.key = element_rect(fill = na_color, colour = na_color),
      plot.margin = margin(3, 0, 1, 0, "lines"),
      plot.background = element_rect(fill = na_color, colour = na_color),
      panel.background = element_rect(fill = na_color, colour = na_color),
      panel.grid.major = element_line(colour = na_color),
      panel.grid.minor = element_line(colour = na_color)
    )
  
  canopy %>%
    plot_gg(width = 5, height = 5,
            raytrace = TRUE, 
            zoom = 0.85, phi = 35, theta = 30, sunangle = 225, soliddepth = -100) 
  