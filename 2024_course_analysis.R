library(package = data.table)
library(package = openxlsx)
library(package = ggplot2)

#-------------------------------------------------------------------------------
meters_to_feet <- function(dist_meters,
                           digits = 1L) {
  
  dist_feet = as.numeric(x = dist_meters) * 3.28084
  dist_feet = round(x = dist_feet, digits = digits)
  return(dist_feet)
}

#-------------------------------------------------------------------------------
get_course_elevation_dt <- function(data_dir = file.path(".", "data")) {
  
  course_elevation_dt <-
    read.xlsx(xlsxFile = file.path(data_dir, "2024-course-elevation.xlsx"),
              sheet = "B24v1_profile data")
  
  setDT(x = course_elevation_dt)
  
  course_elevation_dt[, Loop_Distance_km := Elapsed_Distance / 1000]
  
  course_elevation_dt[, Course_km := floor(x = Loop_Distance_km) + 1]
  
  setnames(x = course_elevation_dt, old = "Elevation", new = "Elevation_meters")
  
  course_elevation_dt[, Elevation_feet := 
                        meters_to_feet(dist_meters = Elevation_meters)]
  
  course_elevation_dt[, gain_loss_meters := 
                        Elevation_meters - shift(x = Elevation_meters)]
  
  course_elevation_dt[, gain_loss_feet := 
                        meters_to_feet(dist_meters = gain_loss_meters)]
  
  return(course_elevation_dt)
}

#-------------------------------------------------------------------------------
get_elevation_gain_meters <- function(course_elevation_dt,
                                      digits = 1L) {
  
  elevation_gain_meters <- 
    course_elevation_dt[gain_loss_meters > 0, sum(gain_loss_meters)]
  
  elevation_gain_meters <- 
    round(x = elevation_gain_meters, digits = digits)
  
  return(elevation_gain_meters)
}

#-------------------------------------------------------------------------------
get_elevation_gain_by_km_dt <- function(course_elevation_dt,
                                            digits = 1L) {
  
  elevation_gain_meters_dt <- 
    course_elevation_dt[gain_loss_meters > 0, 
                        .(elevation_gain_meters = sum(gain_loss_meters)),
                        by = "Course_km"]
  
  elevation_gain_meters_dt[, elevation_gain_feet :=
                             meters_to_feet(dist_meters = 
                                              elevation_gain_meters)]
  
  return(elevation_gain_meters_dt)
}

#-------------------------------------------------------------------------------
get_course_elevation_plot <- function(course_elevation_dt,
                                      elevation_gain_meters) {
  
  course_elevation_plot <- ggplot() +
    geom_line(data = course_elevation_dt,
              mapping = aes(x = Loop_Distance_km, y = Elevation_meters),
              color = "blue",
              linewidth = 2) +
    #scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    xlab(label = "Loop Distance (km)") +
    scale_x_continuous(limits = c(0.0, 10.0),
                       breaks = seq(from = 0.0, to = 10.0, by = 1.0),
                       expand = c(0.0, 0.0)) +
    ylab(label = "Elevation (m)") +
    ggtitle(label = "2024 Birkie 10km Course Elevation Plot",
            subtitle = paste0("Elevation gain ",
                              elevation_gain_meters, " m (",
                              meters_to_feet(dist_meters = 
                                               elevation_gain_meters),
                              " ft) per lap")) +
    theme_bw()
  
  return(course_elevation_plot)
}

#-------------------------------------------------------------------------------
get_course_elevation_gain_by_km_plot <- function(course_elevation_dt) {
  
   elevation_gain_by_km_dt <-
     get_elevation_gain_by_km_dt(course_elevation_dt = course_elevation_dt)
  
  course_elevation_gain_by_km_plot <- ggplot() +
    #geom_hline(mapping = aes(yintercept = 5L)) +
    geom_col(data = elevation_gain_by_km_dt,
             mapping = aes(x = Course_km, y = elevation_gain_meters),
             fill = "royalblue1") +
    geom_text(data = elevation_gain_by_km_dt,
              mapping = aes(x = Course_km, y = elevation_gain_meters,
                            label = paste0(round(elevation_gain_meters, 
                                                 digits = 1),
                                           " m (",
                                           round(elevation_gain_feet,
                                                 digits = 1),
                                           " ft)")), 
              vjust = -0.5,
              size = 3) +
    xlab(label = "Course Kilometer") +
    scale_x_continuous(breaks = seq(from = 0.0, to = 10.0, by = 1.0)) +
    ylab(label = "Elevation Gain (m)") +
    ggtitle(label = "2024 Birkie Course Elevation Plot",
            subtitle = "Elevation Gain by Course Kilometer") +
    theme_bw()
  
  return(course_elevation_gain_by_km_plot)
}


#-------------------------------------------------------------------------------
save_course_elevation_plot <- function(course_elevation_plot,
                                       filepath,
                                       width = 11L,
                                       height = 8L,
                                       paper = "USr") {
 
  pdf(file = filepath,
      width = width,
      height = height,
      paper = paper)
  plot(x = course_elevation_plot)
  dev.off()
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
course_elevation_dt <- get_course_elevation_dt()

elevation_gain_meters <- 
  get_elevation_gain_meters(course_elevation_dt = course_elevation_dt)

course_elevation_plot <-
  get_course_elevation_plot(course_elevation_dt = course_elevation_dt,
                            elevation_gain_meters = 
                              elevation_gain_meters)

save_course_elevation_plot(course_elevation_plot = course_elevation_plot,
                           filepath = 
                             file.path(".", 
                                       "2024 Birkie Course Elevation Plot.pdf"))

elevation_gain_by_km_dt <- 
  get_elevation_gain_by_km_dt(course_elevation_dt = course_elevation_dt)

course_elevation_gain_by_km_plot <-
  get_course_elevation_gain_by_km_plot(course_elevation_dt = 
                                         course_elevation_dt)

filepath = file.path(".", "2024 Birkie Course Elevation Gain by km.pdf")

save_course_elevation_plot(course_elevation_plot = 
                             course_elevation_gain_by_km_plot,
                           filepath = filepath)

# course_elevation_filename <- file.path(".", "course_elevation_plot.pdf")
# pdf(file = course_elevation_filename,
#     width = 11, height = 8,
#     paper = "USr")
# plot(x = get_course_elevation_plot(course_elevation_dt = course_elevation_dt,
#                                    net_elevation_gain = net_elevation_gain))
# dev.off()






