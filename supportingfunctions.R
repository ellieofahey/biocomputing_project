library(ggplot2)

convert_files <- function(directory) {
  # Converts all files in a specified directory into .csv files
  
  # Start with a list of all of the .txt files in the specified directory
  txt_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
  
  # Iterate through the .txt files
  for (file in txt_files) {
    # Get the name of the .txt file to keep it the same for the .csv file
    output_name <- sub("\\.txt$", ".csv", file)
    data <- read.table(file, header = TRUE, sep="\t")
    write.csv(data, file=output_name, row.names = FALSE)
  }
}

compile_csv <- function(directory){
  # Write a function to compile data from all .csv files in a directory into a single file

  compiled_data <- list()
  
  # In order to fill in the country column, need to first identify which rows will have 'x' or 'y'
  
  country_directories <- list.dirs(directory, recursive = FALSE)
  
  for (country_directory in country_directories) {
    if (grepl("X", country_directory)) {
      country <- "x"
    } else if (grepl("Y", country_directory)) {
      country <- "y"
    }
  
  csv_files <- list.files(country_directory, pattern="\\.csv$",full.names = TRUE)
  
  for (file in csv_files) {
    data <- read.csv(file, header=TRUE)
    
    # Get the day of the year from the filenames
    file_name <- basename(file)
    date <- sub("screen_(\\d{3})\\.txt", "\\1", file_name)
    
    # Add two columns with Country and DayofYear
    data$Country <- country
    data$DayofYear <- as.integer(date)
    
    compiled_data[[length(compiled_data) + 1]] <- data
  }
}
  compiled_data <- do.call(rbind, compiled_data)
  return(compiled_data)
  # User should be able to choose whether they want to ...
  # Remove rows with NA's
  # Include NA's in the data with a warning 
  # Include NA's in the data without a warning 
}

summarize_data <- function(compiled_data_file) {
  # Summarize the compiled data in terms of ...
  
  data <- read_csv(compiled_data_file)
  
  # If one or more markers are 1 for each row then the patient was infected
  markers_to_check <- c("marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10")
  infected_tally <- 0
  
  for (i in 1:nrow(data)) {
    if (any(data[i, markers_to_check] == 1)) {
      infected_tally <- infected_tally + 1
    }
  }
  
  # Calculations for the summary list
  num_screens_run <- max(data$dayofYear) - min(data$dayofYear)
  percent_infected <- ((infected_tally / nrow(data)) * 100)
  percent_male <- ((sum(data[["gender"]] == "male") / nrow(data)) * 100)
  percent_female <- ((sum(data[["gender"]] == "female") / nrow(data)) * 100)
  
  summary_list <- list(
  # Number of screens run
    num_screens_run = num_screens_run,
  
    # Percent of patients screened that were infected
    percent_infected = percent_infected,
      
    # Percent of patients that identify as male and female
    percent_male = percent_male,
    percent_female = percent_male

) 
  
  # Age distribution of patients as a plot
  print(ggplot(data, aes(x = age)) + 
    geom_histogram(aes(y= ..density..), binwidth = 5, fill = "skyblue", color = "blue", alpha = 0.7) +
    geom_density(color = "gray", size = 1) +
    labs(title = "Age Distribution of Patients from Countries X and Y") +
    theme_minimal())
  
  return(summary_list)
}

# ---------- SAMPLE USES ----------

summarize_data('allData.csv')

convert_files("/Users/ellie/git/Biocomp/Rproject2024/countryX")
convert_files("/Users/ellie/git/Biocomp/Rproject2024/countryY")

compile_csv("/Users/ellie/git/Biocomp/Rproject2024")
