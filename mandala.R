library(ggplot2); library(magrittr)

#for example
# mandala(4,10,2.9,palette = topo.colors(3))
mandala <- function(iter=4, # Number of iterations (depth)
                    points=c(3,6,9,12), # Number of points - length can be 1 or number of iterations
                    radius=1.6, # Factor of expansion/compression
                    palette=NA, # Colour palette to use or NA (for example, topo.colors(5))
                    border_poly = FALSE,
                    distance_colouring = TRUE, # Colours based on distance from center or poly area
                    gradient_colouring = FALSE, # Discrene or gradient colouring fills 
                    border = "black"){ # Display border polygons
  #helper function from tile.list to a data frame
  unwrap <- function(lst){
    x <- lst$x
    y <- lst$y
    seg <- data.frame(cbind(x, y)) #make a data frame of the polygon segments
    seg$grp <- lst$ptNum
    seg$centx <- lst$pt[1] #polygon center
    seg$centy <- lst$pt[2]
    seg$border <- any(lst$bp) #TRUE when this segement is part of the border
    seg$area <- lst$area
    return(seg)
  }
  # Initial center
  df=data.frame(x=0, y=0)
  my_points <- rep(points, length.out = iter) #points becomes length of iter
  # Iterate over centers again and again
  for (k in 1:iter){
    angles=seq(0, 2*pi*(1-1/my_points[k]), length.out = my_points[k])+pi/2 #Angles of points from center - can vary by iter
    temp=data.frame()
    for (i in 1:nrow(df)){
      data.frame(x=df[i,"x"]+radius^(k-1)*cos(angles), 
                 y=df[i,"y"]+radius^(k-1)*sin(angles)) %>% rbind(temp) -> temp
    }
    df=temp
  }
  data <- df %>%
    dplyr::select(x,y) %>% 
    deldir::deldir(sort=TRUE) %>% # Obtain Voronoi regions
    deldir::tile.list() %>% #create a list of polygons 
    lapply(unwrap)
  mydf<-do.call("rbind", data) %>% #make a data frame
    dplyr::mutate(dist = centx^2 + centy^2, #calculate distance from center to assist colouring
                  col = dist,
                  area = area)
  #mandala(iter = 3, points = 14, radius = 1.4, gradient_colouring = FALSE, distance_colouring = FALSE, palette = palette)
  if(!gradient_colouring){
    mydf$col <- as.factor(mydf$col)
    mydf$area <- as.factor(mydf$area)
  } 
  if(!border_poly){mydf <- dplyr::filter(mydf, border == FALSE)} #exclude border polygons based on parameter
  
  gg <- ggplot(mydf, aes(x=x, y=y, group=grp)) +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_fixed() + 
    theme(legend.position  = "none",
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.ticks       = element_blank(),
          panel.grid       = element_blank(),
          axis.title       = element_blank(),
          axis.text        = element_blank())
  if(distance_colouring){
    gg <- gg + geom_polygon(aes(fill = col), colour = border, size=0.1)
    fill_vals <- length(unique(as.numeric(mydf$col)))
  } else {
    gg <- gg + geom_polygon(aes(fill = area), colour = border, size=0.1)
    fill_vals <- length(unique(as.numeric(mydf$area)))
  }
  if(gradient_colouring){
    plot <- gg + scale_fill_gradientn(colors=sample(palette, length(palette)))
  } else {
    plot <- gg + scale_fill_manual(values = rep_len(palette, fill_vals))
  }
  return(plot)
}
#for example,
# mandala_save(iter=4,points=c(3,6,9,12),radius=4,palette=topo.colors(5),path="images/")
mandala_save <- function(iter, points, radius, palette=NA, border_poly=FALSE, border = "black", path="", cm=10, res=300){
  if(length(points) == 1){
    p_string <- as.character(points)
  } else {
    p_string <- paste0('[', toString(points) ,']')
  }
  for(i in iter){
    for(k in radius){
      #print(sprintf("%sMandela-%03d-%s-%2.1f.png",  path, i, p_string, k, replace=TRUE)) #print the filename to track progress
      png(filename=sprintf("%sMandela-%03d-%s-%2.1f.png",  path, i, p_string, k, replace=TRUE), #filename - prioritize points
          bg="transparent", type="cairo-png", units="cm", height=cm, width = cm, res=res)
      print(mandala(iter = i, points = points, radius = k, palette = palette, border_poly = border_poly, border = border))
      dev.off()
    }
    
  }
  return(TRUE) 
}
#for example,
# mandala_ani(iter=4,points=c(3,6,9,12),radius=4,palette=topo.colors(5),path="images/")
mandala_ani <- function(iter, points, radius, palette=NA, border_poly=FALSE,
                        distance_colouring = TRUE, gradient_colouring = FALSE, border = "black", morph = FALSE,
                        path="", cm=10, res=100, fps = 2, remove_previous = FALSE, my_filename = "ani.gif"){
  if(length(points) == 1){
    p_string <- as.character(points)
  } else {
    p_string <- paste0('[', toString(points) ,']')
  }
  if(min(radius) != max(radius)){
    rad_min <- min(radius)
    rad_max <- max(radius)
    rad_mid <- mean(c(rad_min, rad_max))
    rad_1q <- mean(c(rad_min, rad_mid))
    rad_3q <- mean(c(rad_mid, rad_max))
    my_radius <- c(rad_min, rad_1q, rad_mid, rad_3q, rad_max)
  } else {
    my_radius <- radius
  }
  if(path != ""){
    dir.create(path, showWarnings = FALSE) 
  }
  if(remove_previous){
    file.remove(file.path(path, list.files(path)))
  }
  for(i in iter){
    for(k in my_radius){
      #print(sprintf("%s/Mandela-%03d-%s-%2.1f.png",  path, i, p_string, k, replace=TRUE)) #print the filename to track progress
      png(filename=file.path(path, sprintf("Mandela-%03d-%s-%2.1f.png",  i, p_string, k, replace=TRUE)), #filename - prioritize points
          bg="transparent", type="cairo-png", units="cm", height=cm, width = cm, res=res)
      print(mandala(iter = i, points = points, radius = k, palette = palette, border_poly = border_poly,
                    distance_colouring = distance_colouring, gradient_colouring = gradient_colouring, border = border))
      dev.off()
    }
  }
  im_list <- image_read(file.path(path,dir(path))) 
  if (morph)
    {im_list <- image_morph(im_list,10)}
  ani_obj <- image_animate(im_list, fps = fps)  
  
  
  image_write(ani_obj, file.path(path, my_filename)) #create the animated gif
  
  return(TRUE) 
}
