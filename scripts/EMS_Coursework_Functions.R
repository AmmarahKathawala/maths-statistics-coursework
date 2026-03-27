#EMS_Coursework_Functions.R

#Purpose:Functions used for the Mountain Climb Markov-chain game.
#-------------------------------------------------------------------------------
#Function for Question 1:
#Simulates a single move in the Mountain Climb game
#Output: 
#   pos: updated position after one dice roll 

play_mc <- function(
    #Input parameters follow those in the brief
    start_pos = 0,
    max_pos = 70,
    slippery_squares = c(9, 18, 27, 36, 45, 54, 63),
    climbing_squares = c(16, 32, 48, 64),
    slip_back_range = 1:3,
    climb_up_amount = 5) {
  
  #Roll one fair die
  die <- sample.int(6, 1)
  
  #Update position based on die roll output
  pos <- start_pos + die
  
  #Apply board rules if still below summit
  if (pos < max_pos) {
    
    #Slide backwards on slippery squares
    if (pos %in% slippery_squares) {
      pos <- max(0, pos - sample(slip_back_range, 1))
    }
    
    #Climb upwards on climbing squares
    if (pos %in% climbing_squares) {
      pos <- pos + climb_up_amount
    }
  }
  
  #Cap position at summit
  pos <- min(pos, max_pos)
  
  #Return updated position after applying Mountain Climb rules
  return(pos)
}

#-------------------------------------------------------------------------------
#Function for Question 2:
#Simulates a full Mountain Climb game until reaching the summit
#Output:
#   pos_history: vector of positions after each turn
#   turns_taken: total number of turns required to reach the summit

play_mc_full <- function(
    #Input parameters use sensible default values
    start_pos = 0,
    max_pos = 70,
    max_turns = 1000,
    slippery_squares = c(9, 18, 27, 36, 45, 54, 63),
    climbing_squares = c(16, 32, 48, 64),
    slip_back_range = 1:3,
    climb_up_amount = 5) {
  
  #Initialise player position and record history
  pos <- start_pos
  pos_history <- start_pos
  turns <- 0
  
  #Play until summit is reached/turn cap is hit
  for (t in seq_len(max_turns)) {
    if (pos >= max_pos) break
    
    #Perform one move with play_mc() function
    pos <- play_mc(
      start_pos = pos,
      max_pos = max_pos,
      slippery_squares = slippery_squares,
      climbing_squares = climbing_squares,
      slip_back_range = slip_back_range,
      climb_up_amount = climb_up_amount
    )
    
    #Record updated position
    pos_history <- c(pos_history, pos)
    turns <- turns + 1
  }
  
  #Return list of all positions and number of turns taken
  return(list(pos_history = pos_history, turns_taken = turns))
}

#-------------------------------------------------------------------------------
#Function for Question 3:
#Simulates multiple games of Mountain Climb and summarises the results
#Output: A list containing:
#   turns_vector: numeric vector with number of turns taken in each simulation
#   turns_stats: named vector with summary statistics (mean, median, sd)
#   all_histories: list of position-history vectors for each simulation

#Simulates multiple games of Mountain Climb and summarises the results

simulate_climbs <- function(
    #Input parameters use sensible default values
    n_simulations = 100,
    start_pos = 0,
    max_pos = 70,
    max_turns = 1000,
    slippery_squares = c(9, 18, 27, 36, 45, 54, 63),
    climbing_squares = c(16, 32, 48, 64),
    slip_back_range = 1:3,
    climb_up_amount = 5) {
  
  #Make vector for number of turns and a list for histories
  turns_vector <- numeric(n_simulations)
  all_histories <- vector("list", n_simulations)
  
  #Loop over simulations
  for (i in seq_len(n_simulations)) {
    #Simulate a full game using play_mc_full()
    game_res <- play_mc_full(
      start_pos = start_pos,
      max_pos = max_pos,
      max_turns = max_turns,
      slippery_squares = slippery_squares,
      climbing_squares = climbing_squares,
      slip_back_range = slip_back_range,
      climb_up_amount = climb_up_amount
    )
    
    #Store the number of turns and the history
    turns_vector[i] <- game_res$turns_taken
    all_histories[[i]] <- game_res$pos_history
  }
  
  #Summary statistics of the turns taken
  turns_stats <- c(
    mean = mean(turns_vector),
    median = median(turns_vector),
    sd = sd(turns_vector)
  )
  
  #Return list of results
  return(list(
    turns_vector = turns_vector,
    turns_stats = turns_stats,
    all_histories = all_histories
  ))
}

#-------------------------------------------------------------------------------
#Function for Question 4:
#Simulates multiple Mountain Climb games and returns the position reached after a fixed number of turns
#Output: A list containing
#   pos_vector: numeric vector with the position reached after n_turns in each simulation
#   pos_stats: named vector with summary statistics (mean, var, sd)
#   all_histories 

simulate_pos_at <- function(
    #Input parameters use sensible default values
    n_simulations = 100,
    n_turns = 10,
    start_pos = 0,
    max_pos = 70,
    max_turns = 1000,
    slippery_squares = c(9, 18, 27, 36, 45, 54, 63),
    climbing_squares = c(16, 32, 48, 64),
    slip_back_range = 1:3,
    climb_up_amount = 5) {
  
  #Make vector for positions and list for histories
  pos_vector <- numeric(n_simulations)
  all_histories <- vector("list", n_simulations)
  
  #Loop over simulations
  for (i in seq_len(n_simulations)) {
    #Start each game at the starting position
    pos <- start_pos
    pos_history <- start_pos
    
    #Simulate the specified number of turns
    for (t in seq_len(n_turns)) {
      #Perform one move with play_mc() function
      pos <- play_mc(
        start_pos = pos,
        max_pos = max_pos,
        slippery_squares = slippery_squares,
        climbing_squares = climbing_squares,
        slip_back_range = slip_back_range,
        climb_up_amount = climb_up_amount
      )
      
      pos_history <- c(pos_history, pos)
    }
    
    #Store the position after n_turns and list the history
    pos_vector[i] <- pos
    all_histories[[i]] <- pos_history
  }
  
  #Summary statistics of the positions reached
  pos_stats <- c(
    mean = mean(pos_vector),
    var = var(pos_vector),
    sd = sd(pos_vector)
  )
  
  #Return results
  return(list(
    pos_vector = pos_vector,
    pos_stats = pos_stats,
    all_histories = all_histories
  ))
}

#Purpose:Functions used for the Data Exploration section.
#-------------------------------------------------------------------------------
#Function for Question 6
#Updates BMI values and removes PCA-based outliers
#Output:
#   data_clean: cleaned data frame with corrected BMI and outliers removed

update_bmi <- function(
    data,
    bmi_min = 10,
    bmi_max = 50,
    num_cols = c("age","bai","bmi","body_fat","density","weight","height"),
    sd_cutoff = 3) {
  
  #Recalculate BMI from weight and height (height in cm)
  bmi_recalc <- data$weight / ((data$height / 100)^2)
  
  #Update BMI where original values are implausible
  data$bmi <- ifelse(data$bmi < bmi_min | data$bmi > bmi_max,
                     bmi_recalc,
                     data$bmi)
  
  #Subset rows with complete numeric data for PCA
  cc_idx  <- which(complete.cases(data[, num_cols]))
  num_mat <- data[cc_idx, num_cols]
  
  #Run PCA on numeric variables (centre and scale)
  pca_obj <- prcomp(num_mat, center = TRUE, scale. = TRUE)
  scores  <- pca_obj$x          
  
  #Identify outliers using PC1 and PC2 (> sd_cutoff SD from centre)
  pc1_sd <- sd(scores[, 1])
  pc2_sd <- sd(scores[, 2])
  
  out_loc <- which(abs(scores[, 1]) > sd_cutoff * pc1_sd |
                     abs(scores[, 2]) > sd_cutoff * pc2_sd)
  
  #Map PCA outlier indices back to original row numbers
  out_rows <- cc_idx[out_loc]
  
  #Remove outliers and return cleaned data
  data_clean <- data[-out_rows, ]
  return(data_clean)
}
