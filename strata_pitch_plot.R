#Please note, that this is slightly modified and adjusted to @Stratabet data version of @FC_rstats work available here: 
# https://github.com/FCrSTATS/Visualisations/blob/master/3.CreateAPitch.md

library(ggplot2)

grass_colour = "#202020"
line_colour = "#efefef"
background_colour = "#202020"
goal_colour = "#131313"

theme_blankPitch = function(size=12) { 
  theme(
    #axis.line=element_blank(), 
    axis.text.x=element_blank(), 
    axis.text.y=element_blank(), 
    #axis.ticks.y=element_text(size=size),
    #   axis.ticks=element_blank(),
    axis.ticks.length=unit(0, "lines"), 
    #axis.ticks.margin=unit(0, "lines"), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank(), 
    legend.background=element_rect(fill=NA, colour=NA), 
    legend.key=element_rect(colour=background_colour,fill=background_colour), 
    legend.key.size=unit(1.2, "lines"), 
    legend.text=element_text(size=size), 
    legend.title=element_text(size=size, face="bold",hjust=0),
    strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
    panel.background=element_rect(fill=background_colour,colour=background_colour), 
    #       panel.border=element_blank(), 
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.spacing=element_blank(), 
    plot.background=element_blank(), 
    plot.margin=unit(c(0, 0, 0, 0), "lines"), 
    plot.title=element_text(size=size*1.2), 
    strip.text.y=element_text(colour=background_colour,size=size,angle=270),
    strip.text.x=element_text(size=size*1))}

ymin <- 0
ymax <- 420
xmin <- -136
xmax <- 136

left_goalpost_x <- 15
left_goalpost_y <- 0
right_goalpost_x <- -15
right_goalpost_y <- 0
box_6yard_left_x <- 37
box_6yard_left_y <- 22
box_6yard_right_x <- -37
box_6yard_right_y <- 22
penalty_spot_x <- 0
penalty_spot_y <- 44
box_18yard_left_x <- 81
box_18yard_left_y <- 66
box_18yard_right_x <- -81
box_18yard_right_y <- 66
centre_spot_x <- 0
centre_spot_y <- 210


pitch_plot <- function(){ 
  
  create_circle <- function(centre = c(0,0), diameter = 1, npoints = 100){
    r = diameter / 2
    t <- seq(0,2*pi,length.out = npoints)
    x <- centre[1] + r * cos(t)
    y <- centre[2] + r * sin(t)
    return(data.frame(x = x, y = y))
  }
  
  centre_circle <- create_circle(c(centre_spot_x, centre_spot_y), 50, 1000)
  
  D_left <- create_circle(c(penalty_spot_x, ymax - penalty_spot_y), 66.6, 1000)
  D_left <- D_left[which(D_left$y <= ymax - box_18yard_left_y), ]
  
  D_right <- create_circle(c(penalty_spot_x, penalty_spot_y), 66.6, 1000)
  D_right <- D_right[which(D_right$y >= box_18yard_left_y), ]
  
  corner_top_left <- create_circle(c(xmax, ymax), 8, 1000)
  corner_top_left <- corner_top_left[which(corner_top_left$x <= 136 & corner_top_left$y <= 420),]
  
  corner_top_right <- create_circle(c(xmax, ymin), 8, 1000)
  corner_top_right <- corner_top_right[which(corner_top_right$x <= 136 & corner_top_right$y >= 0), ]
  
  corner_bottom_left <- create_circle(c(xmin, ymax), 8, 1000)
  corner_bottom_left <- corner_bottom_left[which(corner_bottom_left$x >= -136 & corner_bottom_left$y <= 420),]
  
  corner_bottom_right <- create_circle(c(xmin, ymin), 8, 1000)
  corner_bottom_right <- corner_bottom_right[which(corner_bottom_right$x >= -136 & corner_bottom_right$y >= 0),]
  
  plt <- ggplot() + coord_fixed() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, colour = line_colour, size = 1) +
  geom_rect(aes(xmin = right_goalpost_x, xmax = left_goalpost_x, ymin = ymin - 5, ymax = ymin), fill = NA, colour = line_colour, size = 1) +
  geom_rect(aes(xmin = right_goalpost_x, xmax = left_goalpost_x, ymin = ymax, ymax = ymax + 5), fill = NA, colour = line_colour, size = 1) +
  geom_rect(aes(xmin = box_6yard_right_x, xmax = box_6yard_left_x, ymin = ymin, ymax = box_6yard_left_y), fill = NA, colour = line_colour, size = 1) +
  geom_rect(aes(xmin = box_6yard_right_x, xmax = box_6yard_left_x, ymin = ymax - box_6yard_left_y, ymax = ymax), fill = NA, colour = line_colour, size = 1) +
  geom_point(aes(x = penalty_spot_x, y = penalty_spot_y), colour = line_colour, size = 1) +
  geom_point(aes(x = penalty_spot_x, y = ymax - penalty_spot_y), colour = line_colour, size = 1) +
  geom_rect(aes(xmin = box_18yard_right_x, xmax = box_18yard_left_x, ymin = ymin, ymax = box_18yard_left_y), fill = NA, colour = line_colour, size = 1) +
  geom_rect(aes(xmin = box_18yard_right_x, xmax = box_18yard_left_x, ymin = ymax - box_18yard_left_y, ymax = ymax), fill = NA, colour = line_colour, size = 1) +
  geom_segment(aes(x = xmin, y = centre_spot_y, xend = xmax, yend = centre_spot_y), colour = line_colour, size = 1) +
  geom_point(aes(x = centre_spot_x, y = centre_spot_y), colour = line_colour, size = 1) +
  geom_path(data = centre_circle, aes(x = x, y = y), colour = line_colour, size = 1) +
  geom_path(data = D_left, aes(x = x, y = y), colour = line_colour, size = 1) +
  geom_path(data = D_right, aes(x = x, y = y), colour = line_colour, size = 1) + 
  geom_path(data = corner_top_left, aes(x = x, y = y), colour = line_colour, size = 1) +
  geom_path(data = corner_top_right, aes(x = x, y = y), colour = line_colour, size = 1) +
  geom_path(data = corner_bottom_left, aes(x = x, y = y), colour = line_colour, size = 1) +
  geom_path(data = corner_bottom_right, aes(x = x, y = y), colour = line_colour, size = 1) +
  coord_flip() +
  scale_y_reverse() +
  theme_blankPitch()
  
  return(plt)
}

pitch_plot()
