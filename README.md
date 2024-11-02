# README: Behavior and Modifier Analysis of Observational Data in R

## Overview

This project is designed to perform post-processing on observational data captured in CSV files, focusing specifically on analyzing and aggregating behaviors and modifiers across different observational phases. By utilizing R and the Tidyverse suite of packages, we can efficiently read, manipulate, and summarize the data to gain insights into behavioral patterns. The analysis is aimed at researchers and data analysts interested in understanding the dynamics of animal behavior in various observational settings.

## Project Structure

The project consists of the following main components:

1. **Data Preparation**: Reading the CSV file and cleaning the data.
2. **Phase Duration Calculation**: Calculating the duration of each observation phase.
3. **Behavior Filtering**: Filtering and aggregating behavioral data relevant to the analysis.
4. **Start-Stop Pairing**: Identifying and pairing behavior start and stop events for each observation.
5. **Percentage Calculation**: Calculating the percentage of behavior durations in relation to phase durations.
6. **Output Generation**: Writing the summarized results to a new CSV file for further analysis or reporting.

## Prerequisites

Before running the code, ensure you have the following:

- R installed on your computer.
- RStudio or any other R IDE.
- The `tidyverse`, `dplyr`, `tidyr`, and `lubridate` packages installed. You can install these packages using the following command in R:

```r
install.packages("tidyverse")
```

## Data Input

The project begins by importing a CSV file named `Trial_events_G.csv`. The data structure should include the following relevant columns:

- `Observation_id`: Identifier for each observation.
- `Phase`: Different phases of observation.
- `Behaviour`: The type of behavior observed.
- `Modifier`: Modifying factors related to the behavior.
- `Behaviour_type`: Indicates whether the record is a start or stop event.
- `Time_stamp`: Timestamp for each event in `mm:ss` format.

Make sure the file path in the script corresponds to the actual location of your CSV file.

## Code Explanation

The analysis is performed within an R Markdown document named `Mostafa_Trial_Final`. Here’s a breakdown of the key sections of the code:

### 1. **Setup and Libraries**

```r
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
```

This section loads the necessary libraries for data manipulation and analysis. The Tidyverse includes a collection of packages that work in harmony to provide a streamlined workflow for data science tasks.

### 2. **Reading the Data**

```r
Trial_events_G <- read.csv ("E:/Individual recognition/Individual Recognition R Project/Trial_events_G.csv")
```

This line reads the CSV file into a dataframe called `Trial_events_G`.

### 3. **Time Conversion Function**

```r
convert_to_seconds <- function(time_str) {
  if (is.na(time_str)) return(NA)
  parts <- unlist(strsplit(time_str, ":"))
  if (length(parts) != 2) return(NA)
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}
```

A helper function `convert_to_seconds` is defined to convert time from `mm:ss` format into total seconds. This is crucial for calculating durations accurately.

### 4. **Calculating Phase Durations**

```r
Phase_table <- Trial_events_G %>%
  group_by(Observation_id, Phase) %>%
  summarise(
    Start_timestamp = min(Time_stamp),
    End_timestamp = max(Time_stamp),
    Duration = convert_to_seconds(End_timestamp) - convert_to_seconds(Start_timestamp)
  )
```

This section groups the data by `Observation_id` and `Phase`, then calculates the start timestamp, end timestamp, and duration of each phase. The results are stored in `Phase_table`.

### 5. **Filtering Behavioral Data**

```r
Trial_events_G_Filtered <- Trial_events_G %>%
  group_by(Observation_id, Behaviour, Modifier) %>%
  filter(Behaviour_type != "POINT" & Modifier != "NA")
```

In this step, we filter out behaviors of type "POINT" and any modifiers that are not available (`NA`), allowing for a more focused analysis on relevant behaviors.

### 6. **Start-Stop Pairing**

```r
start_stop_df <- Trial_events_G_Filtered %>%
  filter(Behaviour_type %in% c("START", "STOP")) %>%
  group_by(Observation_id, Behaviour, Modifier) %>%
  mutate(event_id = cumsum(Behaviour_type == "START")) %>%
  filter(Behaviour_type == "START" | (Behaviour_type) == "STOP") %>%
  group_by(Observation_id, Behaviour, Modifier, event_id) %>%
  summarise(
    Start_timestamp = first(Time_stamp[Behaviour_type == "START"]),
    End_timestamp = first(Time_stamp[(Behaviour_type) == "STOP"]),
    .groups = 'drop'
  )
```

Here, we pair each "START" event with its corresponding "STOP" event using a cumulative sum to create an event ID. The result is a new dataframe, `start_stop_df`, that contains start and end timestamps for each behavior-modifier combination.

### 7. **Calculating Percentages**

```r
output_df <- data.frame(
  Observation_id = character(),
  Phase = character(),
  Behaviour = character(),
  Modifier = character(),
  Percentage = character()
)

for (i in 1:nrow(start_stop_df)) {
  obs_id <- start_stop_df$Observation_id[i]
  behaviour <- start_stop_df$Behaviour[i]
  modifier <- start_stop_df$Modifier[i]
  bm_start_time <- start_stop_df$Start_timestamp[i]
  bm_end_time <- start_stop_df$End_timestamp[i]
  bm_duration <- convert_to_seconds(bm_end_time) - convert_to_seconds(bm_start_time)
  percentage <- 0
  
  new_row <- data.frame(
    Observation_id = obs_id,
    Behaviour = behaviour,
    Modifier = modifier,
    Phase = "",
    Percentage = ""
  )
  
  filtered_phase_table <- Phase_table %>% filter(Observation_id == obs_id)
  
  for (j in 1:nrow(filtered_phase_table)) {
    phase_start_time <- filtered_phase_table$Start_timestamp[j]
    phase_end_time <- filtered_phase_table$End_timestamp[j]
    phase_duration <- filtered_phase_table$Duration[j]
    new_row$Phase <- filtered_phase_table$Phase[j];

    if (bm_start_time >= phase_start_time & bm_end_time <= phase_end_time) {
      percentage = (bm_duration / phase_duration) * 100
    } else if (bm_start_time >= phase_start_time & bm_start_time <= phase_end_time & bm_end_time >= phase_end_time) {
      percentage = ((convert_to_seconds(phase_end_time) - convert_to_seconds(bm_start_time)) / phase_duration) * 100
    } else if (bm_start_time <= phase_start_time & bm_end_time <= phase_end_time & bm_end_time > phase_start_time) {
      percentage = ((convert_to_seconds(bm_end_time) - convert_to_seconds(phase_start_time)) / phase_duration) * 100
    } else if (bm_start_time <= phase_start_time & bm_end_time >= phase_end_time) {
      percentage = 100
    } else {
      next
    }
    
    new_row$Percentage <- percentage
    output_df <- rbind(output_df, new_row)
    percentage = ""
  }
}
```

In this section, we iterate through the `start_stop_df` dataframe and calculate the percentage of each behavior's duration relative to the corresponding phase durations. The results are stored in `output_df`.

### 8. **Writing Output to CSV**

```r
write_csv(output_df, "<Path>/Output.csv")
```

The final output is written to a CSV file named `Output.csv`, containing aggregated percentages for behaviors across different phases.

### 9. **Aggregating Output Data**

```r
aggregated_outpt <- output_df %>%
  group_by(Observation_id, Behaviour, Modifier, Phase) %>%
  summarise(Total_Percentage = sum(Percentage), .groups = 'drop')
```

Finally, we aggregate the results to sum the percentages for each combination of observation, behavior, modifier, and phase. The aggregated data is stored in `aggregated_outpt`.

## Future Enhancements

In future iterations of this project, consider the following enhancements:

- **Data Visualization**: Incorporate graphical representations of the behavior and modifier analysis for better understanding and communication of results.
- **Error Handling**: Improve error handling in the functions to manage unexpected data formats or missing values gracefully.
- **Modularization**: Refactor the code into reusable functions for greater modularity and ease of use in different projects.

By following these enhancements, the analysis can be made more robust and user-friendly, paving the way for deeper insights into animal behavior.

## Contributions
Contributions to this project are welcome! If you have suggestions for improvements, additional features, or enhancements, please feel free to fork the repository and submit a pull request. Issues and feedback can also be submitted via the project's GitHub page.

## License
This project is licensed under the Apache License. You are free to use, modify, and distribute the code as long as appropriate credit is given.

## Acknowledgments
We acknowledge the contributions of the community and researchers who provided input and feedback during the development of this project. Special thanks to the R community for the invaluable resources and packages that made this analysis possible.
And for Mostafa Mansour for driving this and the long sessions debugging and achieving the desired results .. also sharing the music.
