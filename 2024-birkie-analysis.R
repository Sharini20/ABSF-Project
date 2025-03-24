library(package = data.table)
library(package = openxlsx)
library(package = lubridate)
library(package = ggplot2)
library(package = magick)
library(package = Polychrome)
library(package = parallel)

#-------------------------------------------------------------------------------
# Hard-coded scenario, which is used to select the correct scenario folder
#-------------------------------------------------------------------------------
get_scenario <- function() {
  
  #return("10k-loop 421-laps")
  #return("10k-loop 321-laps")
  #return("BS race waves1-2-3")
  return("BS race waves1-2")
}

#-------------------------------------------------------------------------------
# Read parameters from the scenario input file
#-------------------------------------------------------------------------------
get_params_dt <- 
  function(data_dir = file.path(".", "scenarios", get_scenario())) {
  
  params_dt <- 
    read.xlsx(xlsxFile = file.path(data_dir, "inputs.xlsx"),
              sheet = "params")
  
  setDT(x = params_dt)
  
  return(params_dt)
}

#-------------------------------------------------------------------------------
# Get the length of the (loop) course in kilometers from the parameters
#-------------------------------------------------------------------------------
get_course_length_km <- function(params_dt = get_params_dt()) {
  
  return(as.numeric(x = params_dt[Param == "course_length_km", Value]))
}

#-------------------------------------------------------------------------------
# Hard-coded definition of course pass points (kilometers from start) which
#  are used to generate pass points plots (number of skiers passing point
#  over time)
#-------------------------------------------------------------------------------
get_pass_points_km <- function() {
  
  return(c(2.5, 5.0, 7.5, 9.9))
}

#-------------------------------------------------------------------------------
# Function to convert meters to feet
#-------------------------------------------------------------------------------
meters_to_feet <- function(dist_meters,
                           digits = 1L) {
  
  dist_feet = as.numeric(x = dist_meters) * 3.28084
  dist_feet = round(x = dist_feet, digits = digits)
  
  return(dist_feet)
}

#-------------------------------------------------------------------------------
# Function to convert feet to meters
#-------------------------------------------------------------------------------
feet_to_meters <- function(dist_feet,
                           digits = 1L) {
  
  dist_meters = as.numeric(x = dist_meters) * 0.3048
  dist_meters = round(x = dist_meters, digits = digits)
  
  return(dist_meters)
}

#-------------------------------------------------------------------------------
# Read in course elevation file from the base data directory
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
# Read in number of laps per race from scenario input workbook
#-------------------------------------------------------------------------------
get_laps_dt <- 
  function(data_dir = file.path(".", "scenarios", get_scenario())) {
    
    laps_dt <-
      read.xlsx(xlsxFile = file.path(data_dir, "inputs.xlsx"),
                sheet = "laps")
    
    setDT(x = laps_dt)
    
    return(laps_dt)
  }

#-------------------------------------------------------------------------------
# Generate a list of colors, one per wave, to use in plots
#-------------------------------------------------------------------------------
get_wave_colors <- function() {
  
  #colors <- rep(scales::hue_pal()(8), times = 4)
  colors <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), times = 4)
  
  wave_colors <- c("01 W35 BS" = colors[[1]],
                   "02 ELITE F BS" = colors[[2]],
                   "03 ELITE M BS" = colors[[3]],
                   "04 W1 BS" = colors[[4]],
                   "05 W2 BS" = colors[[5]],
                   "06 W70 BS" = colors[[6]],
                   "07 W3 BS" = colors[[7]],
                   "08 W4 BS" = colors[[8]],
                   "09 W5 BS" = colors[[9]],
                   "10 W6 BS" = colors[[10]],
                   "11 W7 BS" = colors[[11]],
                   "12 W8 BS" = colors[[12]],
                   "13 W9 BS" = colors[[13]],
                   "14 W10 BS" = colors[[14]],
                   "00 W35 BC" = colors[[15]],
                   "01 ELITE F BC" = colors[[16]],
                   "02 ELITE M BC" = colors[[17]],
                   "03 W1 BC" = colors[[18]],
                   "04 W2 BC" = colors[[19]],
                   "05 W70 BC" = colors[[20]],
                   "06 W3 BC" = colors[[21]],
                   "07 W4 BC" = colors[[22]],
                   "08 W5 BC" = colors[[23]],
                   "09 W6 BC" = colors[[24]],
                   "01 ELITE M KS" = colors[[25]],
                   "02 U20 M KS" = colors[[26]],
                   "03 ELITE F KS" = colors[[1]],
                   "04 U20 F KS" = colors[[2]],
                   "05 ELITE M KC" = colors[[3]],
                   "06 ELITE F KC" = colors[[4]],
                   "07 U20 M KC" = colors[[5]],
                   "08 U20 F KC" = colors[[6]],
                   "09 W1 KC" = colors[[7]],
                   "10 W1 KS" = colors[[8]],
                   "11 W70 KS" = colors[[9]],
                   "12 W70 KC" = colors[[10]],
                   "13 W2 KC" = colors[[11]],
                   "14 W2 KS" = colors[[12]],
                   "15 W3 KS" = colors[[13]],
                   "16 W4 KS" = colors[[14]],
                   "17 W5 KS" = colors[[15]],
                   "18 W3 KC" = colors[[16]],
                   "19 W4 KC" = colors[[17]],
                   "20 U20 M PH" = colors[[18]],
                   "21 U20 F PH" = colors[[19]],
                   "22 W1 PH" = colors[[20]],
                   "BIRKIE SKATE OT" = colors[[21]],
                   "BIRKIE CLASSIC OT" = colors[[22]],
                   "KORTE SKATE OT" = colors[[23]],
                   "KORTE CLASSIC OT" = colors[[24]],
                   "PRINCE HAAKON OT" = colors[[25]]
  )
  
  return(wave_colors)
}

#-------------------------------------------------------------------------------
# Read in and combine data from multiple Excel worksheets within a workbook
#-------------------------------------------------------------------------------

get_excel_sheets_data_dt <- function(file_dir = ".", file_name) {
  
  sheet_names <- getSheetNames(file = file.path(file_dir, file_name))
  
  sheet_data_list <-
    lapply(X = sheet_names, FUN = function(sheet_name) {
      return(read.xlsx(xlsxFile = file.path(file_dir, file_name),
                       sheet = sheet_name))
    })
  
  excel_sheets_data_dt <- rbindlist(l = sheet_data_list)
  
  setDT(x = excel_sheets_data_dt)
  
  return(excel_sheets_data_dt)
}

#-------------------------------------------------------------------------------
# Read in a prior year's results (especially race, wave, and pace in seconds
#  per kilomerer from the base data directory to be randomly selected to
#  simulate this year's race
#-------------------------------------------------------------------------------
get_results_dt <- function(data_dir = file.path(".", "data")) {
  
  results_dt <- get_excel_sheets_data_dt(file_dir = data_dir,
                                         file_name = "2023-results.xlsx")
  
  results_dt <- results_dt[Ovr != "DNS" & Ovr != "DNF" 
                           & Ovr != "a.k." & Ovr != "DSQ"]
  
  results_dt[, pace_secs_per_km := period_to_seconds(x = ms(Pace))]
  
  results_dt[, Wave := ifelse(test = Wave == "Elite Male",
                              yes = "ELITE M",
                              no = Wave)]
  
  results_dt[, Wave := ifelse(test = Wave == "Elite Female",
                              yes = "ELITE F",
                              no = Wave)]
  
  return(results_dt)
}

#-------------------------------------------------------------------------------
# Read wave data (race group, race, wave, wave label, size, and start datetime) 
#  from scenario input workbook
#-------------------------------------------------------------------------------
get_waves_dt <- 
  function(data_dir = file.path(".", "scenarios", get_scenario())) {
  
  waves_dt <- 
    read.xlsx(xlsxFile = file.path(data_dir, "inputs.xlsx"),
              sheet = "waves")
  
  setDT(x = waves_dt)
  
  waves_dt[, Wave := as.character(x = Wave)]
  
  waves_dt[, Start_time := format(x = convertToDateTime(x = Start_time),
                                  format = "%H:%M")]
  
  waves_dt[, start_datetime := make_datetime(year = Start_year,
                                             month = Start_month,
                                             day = Start_day,
                                             hour = Start_hour,
                                             min = Start_min,
                                             tz = "America/Chicago")]
  
  return(waves_dt)
  }

#-------------------------------------------------------------------------------
# Get a list of race groups from the wave info
#-------------------------------------------------------------------------------
get_race_group_list <- function(waves_dt) {
  
  race_group_list <- unique(x = waves_dt[, Race_group])
  
  return(race_group_list)
}

#-------------------------------------------------------------------------------
# Add wave info to the results data table
#-------------------------------------------------------------------------------
add_wave_info_to_results <- function(results_dt, waves_dt) {
  
  merge_waves_dt <- waves_dt[, .(Race, Race_group, 
                                 Wave, Wave_label, 
                                 start_datetime)]
  
  merged_dt <- merge.data.table(x = results_dt,
                                y = merge_waves_dt,
                                by = c("Race", "Wave"),
                                all.x = TRUE)
  
  return(merged_dt)
}

#-------------------------------------------------------------------------------
# Add lap info to the results data table
#-------------------------------------------------------------------------------
add_laps_to_results <- function(results_dt, laps_dt) {
  
  merged_dt <- merge.data.table(x = results_dt,
                                y = laps_dt,
                                by = "Race",
                                all.x = TRUE)
  
  return(merged_dt)
}

#-------------------------------------------------------------------------------
# Compute the elapsed distance at a specified time (end time) for a skier with
#  a specified pace and start time
#-------------------------------------------------------------------------------
get_skier_elapsed_dist <- function(start_time = hms("08:00:00"),
                                   pace_secs_per_km = seconds(x = 150),
                                   end_time = hms("09:02:30")) {

  elapsed_time <- end_time - start_time
  elapsed_distance <- elapsed_time / pace_secs_per_km * 1000
  
  return(elapsed_distance)
}

#-------------------------------------------------------------------------------
# Get a list of POSIXct times between specified start and end times and at
#  specified increments
#-------------------------------------------------------------------------------
get_time_list <- function(start_time,
                           end_time,
                           time_increment_mins = 1L) {

  time_list <- seq(start_time, end_time, by = paste0(time_increment_mins,                                                     ' mins'))
  
  return(time_list)
}

#-------------------------------------------------------------------------------
# Randomly generate skiers from a prior year (results_dt) to flesh out this
#  year's waves with specified sizes
#-------------------------------------------------------------------------------
get_all_racers_dt <- function(waves_dt, results_dt) {
  
  # Randomly sample skiers for each wave up to size
  wave_racers_list <- lapply(X = 1:nrow(waves_dt), FUN = function(i) {
      race <- waves_dt[i, Race]
      wave <- waves_dt[i, Wave]
      model_wave <- waves_dt[i, Model_wave]
      wave_label <- waves_dt[i, Wave_label]
      wave_size <- waves_dt[i, Size]
      
      racers_dt <- results_dt[Race == race & Wave == model_wave]
      racers_dt[, Wave := wave]
      racers_dt[, Wave_label := wave_label]
      
      # Randomly sample results rows with replacement
      row_sample_list <- sample(x = nrow(x = racers_dt),
                                size = wave_size,
                                replace = TRUE)
      
      racer_list <- lapply(X = row_sample_list, FUN = function(i) {
        return(racers_dt[i])
      })
      
      this_wave_racers_dt <- rbindlist(l = racer_list)
      
      return(this_wave_racers_dt)
    })
  
  all_racers_dt <- rbindlist(l = wave_racers_list)
  
  return(all_racers_dt)
}

#-------------------------------------------------------------------------------
# Compute staggered start times to throttle skier volumes as waves start
#-------------------------------------------------------------------------------
get_staggered_start_datetimes <- function(all_racers_dt) {
  
  all_racers_by_start_datetime_list <-
    split(x = all_racers_dt, by = "start_datetime")
  
  start_datetime_list <-
    lapply(X = all_racers_by_start_datetime_list, 
           FUN = function(start_racers_dt) {
      setorder(x = start_racers_dt, pace_secs_per_km)
      
             start_racers_dt[, racer_num := .I]
             # integer division
             start_racers_dt[, stagger := racer_num %/% 65L]
             start_racers_dt[, stagger_start_datetime := 
                               start_datetime + dminutes(x = stagger)]
             
             return(start_racers_dt)
    })
  
  updated_all_racers_dt <- rbindlist(l = start_datetime_list)
  
  return(updated_all_racers_dt)
}

#-------------------------------------------------------------------------------
# Get a data table of finish times (lap one finish times if return_lap_time is
#  TRUE) for all skiers in this year's race.
#-------------------------------------------------------------------------------
get_finish_times_dt <- function(all_racers_dt,
                                course_length_km = get_course_length_km(),
                                round_time_to = "1 minute",
                                return_lap_time = FALSE) {
  
  finish_times_list <-
    lapply(X = 1:nrow(x = all_racers_dt), FUN = function(i) {
      racer_dt <- all_racers_dt[i]
      
      race <- racer_dt[, Race]
      race_group <- racer_dt[, Race_group]
      wave <- racer_dt[, Wave]
      wave_label <- racer_dt[, Wave_label]
      # Return finish time (all laps), or just first lap finish time
      laps <- ifelse(test = return_lap_time == FALSE,
                     yes = racer_dt[, laps],
                     no = 1L)
      pace_secs_per_km <- racer_dt[, pace_secs_per_km]
      #start_datetime <- racer_dt[, start_datetime]
      start_datetime <- racer_dt[, stagger_start_datetime]

      finish_time <- start_datetime + pace_secs_per_km * course_length_km * laps
          
      racer_finish_dt <-
        data.table(Race = race,
                   Race_group = race_group,
                   Wave = wave,
                   Wave_label = wave_label,
                   Finish_time = finish_time)
          
      return(racer_finish_dt)
    })
  
  return(rbindlist(l = finish_times_list))
}

#-------------------------------------------------------------------------------
# Get finish times (or first lap finish times if return_lap_time is TRUE)
#  boxplots by wave within a specified race group.
#-------------------------------------------------------------------------------
get_finish_times_boxplot <- function(race_group,
                                     waves_dt,
                                     finish_times_dt,
                                     return_lap_time = FALSE) {
  plot_wave_start_datetimes_dt <- 
    waves_dt[Race_group == race_group, .(start_datetime)]
  
  min_time <- plot_wave_start_datetimes_dt[, min(start_datetime)]
  max_time <- finish_times_dt[Race_group == race_group, max(Finish_time)]
  
  finish_lap_string <- ifelse(test = return_lap_time == FALSE,
                              yes = "Projected finish time boxplots for the ",
                              no = "Projected lap one time boxplots for the ")
  
  finish_times_boxplot <- ggplot() +
    theme_bw() +
    ylim(min_time, max_time) +
    geom_boxplot(data = finish_times_dt[Race_group == race_group],
                 mapping = aes(x = Wave_label,
                               y = Finish_time)) +
    geom_hline(data = plot_wave_start_datetimes_dt,
               mapping = aes(yintercept = start_datetime),
               color = "red") +
    xlab(label = "Wave") +
    theme(axis.text.x=element_text(angle = 45 ,hjust = 1)) +
    ylab(label = ifelse(test = return_lap_time == FALSE,
                        yes = "Finish Time",
                        no = "First Lap Time")) +
    ggtitle(label = paste0(finish_lap_string,
                           race_group, " race"),
            subtitle = "Red lines are wave start times")
  
  return(finish_times_boxplot)
}

#-------------------------------------------------------------------------------
# Generate and save finish time (lap one finish time if return_lap_time is TRUE)
#  boxplots across waves for each race group.
#-------------------------------------------------------------------------------
make_finish_times_boxplots <- 
  function(all_racers_dt,
           race_group_list,
           course_length_km = get_course_length_km(),
           round_time_to = "1 minute",
           return_lap_time = FALSE) {

  finish_times_dt <- 
    get_finish_times_dt(all_racers_dt = all_racers_dt,
                        course_length_km = course_length_km,
                        round_time_to = round_time_to,
                        return_lap_time = return_lap_time)
  
  for(race_group in race_group_list) {
    finish_lap_string <- ifelse(test = return_lap_time == FALSE,
                                yes = " finish times boxplot.pdf",
                                no = " lap one times boxplot.pdf")
    
    boxplot_filename <- file.path(".", "scenarios", get_scenario(), 
                                  paste0(race_group, finish_lap_string))
    pdf(file = boxplot_filename,
        width = 11, height = 8,
        paper = "USr")
    
    plot(x = get_finish_times_boxplot(race_group = race_group,
                                      waves_dt = waves_dt,
                                      finish_times_dt = finish_times_dt,
                                      return_lap_time = return_lap_time))
    dev.off()
  }
}

#-------------------------------------------------------------------------------
# Generate a data table showing the number of skiers passing each pass point
#  over time; this data shows the number of skiers an observer would see
#  at a pass point over time
#-------------------------------------------------------------------------------
get_pass_point_times_summary_dt <- 
  function(all_racers_dt,
           course_length_km = get_course_length_km(),
           pass_points_km = get_pass_points_km(),
           round_time_to = "1 minute") {
    
    # Set up parallel environment
    num_cores <- detectCores()
    cl <- makeCluster(spec = num_cores - 1)
    
    clusterEvalQ(cl = cl, {
      library(package = lubridate)
      library(package = data.table)
    })
    
    clusterExport(cl = cl,
                  varlist = c("all_racers_dt",
                              "course_length_km",
                              "round_time_to"),
                  envir = environment())
  
  all_pass_points_times_list <-
    lapply(X = 1:length(x = pass_points_km), FUN = function(i) {
      pass_point_km <- pass_points_km[i]
      
      pass_point_times_list <-
        #lapply(X = 1:nrow(x = all_racers_dt), FUN = function(i) {
        parLapply(cl = cl, X = 1:nrow(x = all_racers_dt), fun = function(i) {
          racer_dt <- all_racers_dt[i]
          
          race <- racer_dt[, Race]
          race_group <- racer_dt[, Race_group]
          wave <- racer_dt[, Wave]
          wave_label <- racer_dt[, Wave_label]
          laps <- racer_dt[, laps]
          pace_secs_per_km <- racer_dt[, pace_secs_per_km]
          #start_datetime <- racer_dt[, start_datetime]
          start_datetime <- racer_dt[, stagger_start_datetime]
          
          racer_pass_point_time_list <-
            lapply(X = 1:laps - 1, FUN = function(laps_completed) {
              elapsed_dist_km <- laps_completed * course_length_km + 
                pass_point_km
              
              pass_point_time <- 
                start_datetime + pace_secs_per_km * elapsed_dist_km
              pass_point_time <- round_date(x = pass_point_time,
                                            unit = round_time_to) 
              
              racer_pass_point_dt <-
                data.table(Race = race,
                           Race_group = race_group,
                           Wave = wave,
                           Wave_label = wave_label,
                           Elapsed_dist_km = elapsed_dist_km,
                           Laps_completed = laps_completed,
                           Pass_point_km = pass_point_km,
                           Pass_point_time = pass_point_time)
              
              return(racer_pass_point_dt)
            })
          
          racer_pass_points_dt <- rbindlist(l = racer_pass_point_time_list)
          
          return(racer_pass_points_dt)
        })
      
      return(rbindlist(l = pass_point_times_list))
    })
  
  stopCluster(cl = cl)
  
  all_pass_point_times_dt <- rbindlist(l = all_pass_points_times_list)
            
  pass_point_times_summary_dt <- 
    all_pass_point_times_dt[, .(num_racers = .N),
                            by = c("Race_group", 
                                   "Wave_label", 
                                   "Pass_point_km",
                                   "Pass_point_time")]
  
  return(pass_point_times_summary_dt)
}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
get_pass_point_times_summary_plot <- function(race_group,
                                              pass_point_times_summary_dt) {
  
  race_group_pass_point_summary_dt <- 
    pass_point_times_summary_dt[Race_group == race_group]
  
  pass_point_times_summary_plot <-
    ggplot() +
    geom_area(data = race_group_pass_point_summary_dt,
              mapping = aes(x = Pass_point_time, 
                            y = num_racers,
                            fill = Wave_label)) +
    scale_fill_manual(name = "Wave",
                      values = get_wave_colors(),
                      na.value = "gray") +
    xlab(label = "Skier passing time") +
    ylab(label = "Number of skiers") +
    ggtitle(label = paste0("Skiers passing km point"),
            subtitle = race_group) +
    theme_bw() +
    facet_wrap(facets = vars(Pass_point_km),
               nrow = 2,
               ncol = 2)
  
  return(pass_point_times_summary_plot)  
}

#-------------------------------------------------------------------------------
# Generate and save summary plots, one for each pass point. Each plot shows
#  the number of skiers an observer at a pass point along the course would
#  see across time.
#-------------------------------------------------------------------------------
make_pass_point_summary_plots <- function(race_group_list) {
  
  pass_point_times_summary_dt <- 
    get_pass_point_times_summary_dt(all_racers_dt = all_racers_dt)
  
  for(race_group in race_group_list) {
    plot_filename <- file.path(".", "scenarios", get_scenario(), 
                                  paste0(race_group, 
                                         " pass times summary plot.pdf"))
    pdf(file = plot_filename,
        width = 11, height = 8,
        paper = "USr")
    
    plot(x = get_pass_point_times_summary_plot(race_group = race_group,
                                               pass_point_times_summary_dt =
                                                 pass_point_times_summary_dt))
    dev.off()
  }
}

#-------------------------------------------------------------------------------
# Get the earliest start time across waves in a specified race group
#-------------------------------------------------------------------------------
get_earliest_start_time <- function(race_group, waves_dt) {
 
   earliest_start_time <- 
     waves_dt[Race_group == race_group, min(start_datetime)]
  
  return(earliest_start_time)
}

#-------------------------------------------------------------------------------
# Get a summary that shows the position on the (loop) course of each skier 
#  in a specified race group at a specified time
#-------------------------------------------------------------------------------
get_race_summary_at_datetime_dt <- function(time_index = 1,
                                            race_group = "BIRKIE SKATE",
                                            all_racers_dt,
                                            course_length_km = 
                                              get_course_length_km(),
                                            this_datetime) {
  
  # racers in this race group who have already started
  race_group_racers_dt <- all_racers_dt[Race_group == race_group]
  
  this_datetime <- ymd_hms(this_datetime)
  
  if(nrow(race_group_racers_dt) == 0) {
    # return(data.table(Race_group = character(),
    #                   Race = character(),
    #                   Wave_label = character(),
    #                   start_datetime = POSIXct(),
    #                   this_datetime = POSIXct(),
    #                   pace_secs_per_km = numeric(),
    #                   elapsed_time = numeric(),
    #                   cum_dist = numeric()))
    # return(data.table(num_racers = numeric(),
    #                   Race_group = character(),
    #                   Race = character(),
    #                   Wave_label = character(),
    #                   #this_datetime = POSIXct(),
    #                   this_datetime = numeric(),
    #                   loop_dist = numeric()))
    
    # Returns an empty data table
    # return(data.table(num_racers = 1,
    #                   Race_group = "X",
    #                   Race = "X",
    #                   wave_label = "X",
    #                   this_datetime = this_datetime,
    #                   loop_distance = 1.0)[Race_group == "Y"])
    return(data.table())
  }
  
  # Set up parallel environment
  num_cores <- detectCores()
  cl <- makeCluster(spec = num_cores - 1)
  
  clusterEvalQ(cl = cl, {
    library(package = lubridate)
    library(package = data.table)
  })
  
  clusterExport(cl = cl,
                varlist = c("race_group_racers_dt",
                            "course_length_km"),
                envir = environment())
  
  racer_position_list <- 
    #lapply(X = 1:nrow(x = race_group_racers_dt), FUN = function(i) {
    parLapply(cl = cl, X = 1:nrow(x = race_group_racers_dt), fun = function(i) {
      race <- race_group_racers_dt[i, Race]
      wave_label <- race_group_racers_dt[i, Wave_label]
      #start_datetime <- ymd_hms(race_group_racers_dt[i, start_datetime])
      start_datetime <- ymd_hms(race_group_racers_dt[i, stagger_start_datetime])
      pace_secs_per_km <- race_group_racers_dt[i, pace_secs_per_km]
      laps <- race_group_racers_dt[i, laps]
      
      elapsed_time <- as.double(this_datetime - start_datetime,
                                units = "secs")
      
      # Skier has not yet started
      if(elapsed_time < 0) {
        return(data.table())
      }
      
      cum_dist <- elapsed_time / pace_secs_per_km
      #cum_dist <- round(x = cum_dist, digits = 1)
      cum_dist <- floor(x = cum_dist * 10) / 10
      loop_dist <- cum_dist %% course_length_km
      
      if(loop_dist > 9.9 | loop_dist < 0.1) {
        cat(paste0("Loop distance is ", loop_dist, "\n"))
      }
      
      # skier has finished or just started but not yet on course
      if(cum_dist > laps * course_length_km | 
         cum_dist <= 0) {
        # return(data.table(Race_group = character(),
        #                   Race = character(),
        #                   Wave_label = character(),
        #                   start_datetime = POSIXct(),
        #                   #start_datetime = numeric(),
        #                   this_datetime = POSIXct(),
        #                   #this_datetime = numeric(),
        #                   pace_secs_per_km = numeric(),
        #                   elapsed_time = numeric(),
        #                   cum_dist = numeric(),
        #                   loop_dist = numeric()))
        
        # Returns an empty data table
        # return(data.table(Race_group = race_group,
        #                   Race = race,
        #                   Wave_label = wave_label,
        #                   start_datetime = start_datetime,
        #                   #start_datetime = numeric(),
        #                   this_datetime = this_datetime,
        #                   #this_datetime = numeric(),
        #                   pace_secs_per_km = pace_secs_per_km,
        #                   elapsed_time = elapsed_time,
        #                   cum_dist = cum_dist,
        #                   loop_dist = loop_dist)[Race_group == "Y"])
        
        return(data.table())
      }
      
      return(data.table(Race_group = race_group,
                        Race = race,
                        Wave_label = wave_label,
                        start_datetime = start_datetime,
                        this_datetime = this_datetime,
                        pace_secs_per_km = pace_secs_per_km,
                        elapsed_time = elapsed_time,
                        cum_dist = cum_dist,
                        loop_dist = loop_dist))
    })
  
  stopCluster(cl = cl)
  
  racer_position_dt <- rbindlist(l = racer_position_list)
  
  racer_position_dt <- racer_position_dt[! is.na(x = Race_group)]
  
  race_summary_at_datetime_dt <- 
    racer_position_dt[, .(num_skiers = .N), 
                      by = c("Race_group", "Race", "Wave_label", 
                             "this_datetime", "loop_dist")]
  
  setorder(x = race_summary_at_datetime_dt, loop_dist)
  
  fwrite(x = race_summary_at_datetime_dt,
         file = file.path(".", "scenarios", get_scenario(), 
                          paste0("race_summary.csv")),
         append = TRUE)
  
  return(race_summary_at_datetime_dt)
}

#-------------------------------------------------------------------------------
# Generate and return an amination that shows the position of skiers from a
#  specified race group, with each frame for a specified time
#-------------------------------------------------------------------------------
get_race_animation_image <- function(race_group,
                                     all_racers_dt,
                                     course_elevation_dt,
                                     start_time,
                                     end_time,
                                     time_increment_mins = 1L) {
  
  total_num_group_skiers <- nrow(x = all_racers_dt[Race_group == race_group])
  
  img <- image_graph(width = 800, height = 600, res = 96)
  
  time_list <- get_time_list(start_time = start_time,
                             end_time = end_time,
                             time_increment_mins = time_increment_mins)
  
  race_summary_plot_list <- 
    lapply(X = 1:length(x = time_list), FUN = function(i) {
      this_datetime <- time_list[[i]][1]
      cat(paste0('The time is ', this_datetime, "\n"))
      
      race_summary_at_datetime_dt <-
        get_race_summary_at_datetime_dt(time_index = i,
                                        race_group = race_group,
                                        all_racers_dt = all_racers_dt,
                                        this_datetime = this_datetime)
      
      # Don't display more than 100 skiers per 100m bucket
      max_skiers_to_display <- 75L
      #max_skiers_to_display <- 100L
      
      race_summary_at_datetime_dt[, num_skiers_to_display := 
                                    ifelse(test = num_skiers >
                                             max_skiers_to_display,
                                           yes = max_skiers_to_display,
                                           no = num_skiers)]
      
      skiers_on_course <- race_summary_at_datetime_dt[, sum(num_skiers)]
      
      plot <- ggplot() +
        geom_col(data = race_summary_at_datetime_dt,
                 mapping = aes(x = loop_dist,
                               y = num_skiers_to_display,
                               fill = Wave_label),
                 just = 1.0,  # Place column to the right of axis breaks
                 width = 1.0) +#,
                 #position = position_dodge(preserve = 'single')) +
        geom_line(data = course_elevation_dt,
                  mapping = aes(x = Loop_Distance_km,
                                y = Elevation_meters / 10L),
                  color = "gray") +
        scale_fill_manual(name = "Wave",
                           values = get_wave_colors(),
                           na.value = "gray") +
        xlab(label = "Loop distance km") +
        scale_x_continuous(limits = c(0L, 10L),
                           breaks = seq(from = 0L, to = 10L, by = 1L),
                           expand = c(0.0, 0.0)) +
        ylab(label = "Number of skiers") +
        ylim(0L, max_skiers_to_display) +
        ggtitle(label = race_group,
                subtitle = paste0(DescTools::StrPad(x = skiers_on_course, 
                                                    width = 5),
                                  " of ", total_num_group_skiers,
                                  " skiers on course at ", 
                                  this_datetime)) +
        # Number of skiers in 10m with 2 lanes assuming 10 feet,
        #  5 feet forward and 5 feet backward for each skier
        #  (course capacity)
        geom_hline(mapping = aes(yintercept = 65), 
                   color = "red", 
                   linewidth = 2.0) +
        theme(panel.grid.minor = element_blank()) +
        theme_bw()
      
      print(x = plot)
    })
  
  dev.off()
  
  animation <- image_animate(img, fps = 0.5, optimize = TRUE)
  
  return(animation)
}

#-------------------------------------------------------------------------------
# Generate and save a race animation for each race group
#-------------------------------------------------------------------------------
make_race_animation_by_race_group <- function(race_group_list,
                                              all_racers_dt,
                                              course_elevation_dt,
                                              scenario = get_scenario(),
                                              duration_hours = 2L,
                                              time_increment = 1L) {
  
  for(race_group in race_group_list) {
    start_time <- get_earliest_start_time(race_group = race_group, 
                                          waves_dt = waves_dt)
    # Add one minute so that animation starts with skiers on the course
    start_time <- start_time + dminutes(x = 1L)
    
    # Set end time after start time based on duration
    end_time <- start_time + dhours(x = duration_hours)
    
    race_animation_image <- 
      get_race_animation_image(race_group = race_group,
                               all_racers_dt = all_racers_dt,
                               course_elevation_dt = course_elevation_dt,
                               start_time = start_time,
                               end_time = end_time,
                               time_increment_mins = 1L)
    
    print(race_animation_image)
    
    image_write(image = race_animation_image, 
                path = file.path(".", "scenarios", get_scenario(),
                                 paste0(race_group, " - ", 
                                        scenario, " animation.gif")))
  }
}


#-------------------------------------------------------------------------------
# MAIN
#-------------------------------------------------------------------------------

# Remove .pdf files from any prior run of this scenario
wd <- getwd()
setwd(dir = file.path(".", "scenarios", get_scenario()))
pdf_file_list <- list.files(path = ".", pattern = "*.pdf")
file.remove(pdf_file_list)
setwd(dir = wd)

course_elevation_dt <- get_course_elevation_dt()

waves_dt <- get_waves_dt()

laps_dt <- get_laps_dt()

results_dt <- get_results_dt()

results_dt <- add_wave_info_to_results(results_dt = results_dt,
                                              waves_dt = waves_dt)

results_dt <- add_laps_to_results(results_dt = results_dt, laps_dt = laps_dt)

all_racers_dt <- get_all_racers_dt(waves_dt = waves_dt,
                                   results_dt = results_dt)

all_racers_dt <- get_staggered_start_datetimes(all_racers_dt = all_racers_dt)

race_group_list <- get_race_group_list(waves_dt = waves_dt)

make_pass_point_summary_plots(race_group_list = race_group_list)

course_length_km <- get_course_length_km()

# Start and finish times
make_finish_times_boxplots(all_racers_dt = all_racers_dt,
                           race_group_list = race_group_list,
                           course_length_km = course_length_km,
                           round_time_to = "1 minute",
                           return_lap_time = FALSE)

# Start and first lap times
make_finish_times_boxplots(all_racers_dt = all_racers_dt,
                           race_group_list = race_group_list,
                           course_length_km = course_length_km,
                           round_time_to = "1 minute",
                           return_lap_time = TRUE)

make_race_animation_by_race_group(race_group_list = race_group_list,
                                  all_racers_dt = all_racers_dt,
                                  course_elevation_dt =
                                    course_elevation_dt,
                                  duration_hours = 2L,
                                  time_increment = 1L)

#-------------------------------------------------------------------------------
# TODO
# 1. Fix animation warnings(position_dodge()` requires non-overlapping x intervals)
# 2. Fix missing values warning (Removed 2 rows containing missing values (`geom_col()`))
#     COMPLETE: 2/11/2024
#     Set ylim(0, 100); some wave start numbers were > 100; to fix, set num_skiers to a max of 100
# 3. Adjust wave 100m numbers to at most 100, filling in backwards to slow wave start
#    COMPLETE 2/12/2024; staggered start times (65 skiers per minute)
# 4. Name animation files to include scenario
#    COMPLETE: 2/11/2024
# 5. Model varying pace per km based on elevation
# 6. Model congestion
#-------------------------------------------------------------------------------


# Line 647: return doesn't match summary returned later; why this statement?

