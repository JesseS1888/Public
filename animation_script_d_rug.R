##### load packages #####
library(magick)
library(ggplot2)
library(lubridate)
library(stringr)
library(raster)
library(dplyr)

###### create raster ######
r <- raster(ncol=10, nrow=10)

# add values to cells
set.seed(0)
values(r) <- runif(ncell(r), 0, 100)

#plot to see what it looks like
plot(r, main='Raster with 100 cells')

#convert to dataframe
r_dataframe <- as.data.frame(r, xy = TRUE)


##### make dataframe to offset layer values #####
x <- 0:30
df <- data.frame(x)
df <- df %>%
  mutate(y = (-x^2) + (30*x))


##### create a temporary directory to which the images will be written #####
dir_out <- file.path(tempdir(), "animation")
dir.create(dir_out, recursive = TRUE)

##### for loop #####
for (val in 1: 30)
{
  #every loop creates a dataframe with a layer offset by the parabola
  Scratch_data <- r_dataframe %>%
    mutate(offset_layer = df$y[val] - layer)
  #adds a time stamp to each dataframe (in this case just x1 value)
  Scratch_data <- Scratch_data %>%
    mutate(stamp = df$x[val])
  
  
  #categorize values
  Scratch_data <- Scratch_data %>%
    mutate(Category = case_when(offset_layer < 100 ~ "Low",
                                offset_layer >= 100 & offset_layer < 150 ~ "Medium",
                                offset_layer >= 150 & offset_layer < 200 ~ "High",
                                offset_layer >= 200 ~ "Very high"))
  
  #create plot of single frame
  p <-
    ggplot(Scratch_data, aes(y=y, x=x, fill= Category)) + 
    geom_raster()+
    scale_fill_manual(values = c("Low" = "#ff6d6d", "Medium" = "#70ad48", "High" = "#4d78c6", "Very high" = "#7b40a7"))+
    labs(fill = "Value:")+
    theme_void()+
    #ggtitle provides timestamp
    ggtitle(Scratch_data$stamp)+
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
  
  #names each .png with val (0001.png, 0002.png, 0010.png, etc.)
  fp <- file.path(dir_out, paste0(stringr::str_pad(val, 4, pad = "0"), ".png"))
  
  ggsave(plot = p, 
         filename = fp, 
         device = "png")
  
}


##### join images using Magick #####
## list file names and read in using lapply
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 10 frames per second
img_animated <- image_animate(img_joined, fps = 10)

## view animated image (open in external viewer to see entire image)
img_animated
# save animated image
image_write(image = img_animated,
            path = "~/heatmap.gif")




