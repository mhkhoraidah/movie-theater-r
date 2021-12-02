# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack
snacks <- snacks<- rbind(c("Popcorn", 5), 
                         c("Coca-Cola", 7), 
                         c("Nachos Chips", 11),
                         c("Snakers", 4))
# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch
# 4. Discount Student
# 5. Create function
# 6. theaters types (VIP, St)
# 7. Random number of seats will start form seats/2:seats
# 8. Scheduling movie function


#'''
# Object: theater
#          -type
#          -seats
#          -adult_cost
#          -child_cost
#'''




# Cost for adults and children
ticket_cost <- 12
ticket_cost_child <- 7 

# List 7 of your favorite movies
movies <- rbind(c("Limitless", "PG-13", 3), 
                c("Taken", "PG-13", 1), 
                c("The Old Guard", "R", ),
                c("Mr. & Mrs. Smith", "PG-13"),
                c("Focus", "+16"),
                c("Raya and the Last Dragon", "PG"),
                c("Rush Hour 3", "PG-13"))

show_time = c("13:30", "18:00", "22:40")

theaters <-  6 # How many screens does the theater have? (assume 1 per movie)
seats <-  120 # How many seats does each theater hold
week_days <- rep(0, 7)  # Store totals for each day
  
    # iterate through the week
    for (day_num in 1:length(week_days)) {
      # Keep track of total revenue for the day
      daily_revenue <- 0
      
      # iterate through the amount of screens on a particular day
      for (theater_number in 1:theaters) {
        
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(1:seats, 1)
        avalibale_seats <- seats - visitors_adults
        visitors_children <- 0
        
        # conditional statements for movies that may be PG-13 and children are not allowed to watch
        if(movies[theater_number,2] == "PG" || movies[theater_number,2] == "PG-13") {
          visitors_children <- sample(1:avalibale_seats,1) # this is should be the the rest of available seats or less
          avalibale_seats <- avalibale_seats - visitors_children
        } 
        cat("\nThe theater #", theater_number, "\n\tMovie : ", movies[theater_number], "(", movies[theater_number, 2] , ")", "\n\tAduilt booked: ", visitors_adults,
            "\n\tChildren booked: ", visitors_children, "\n\tAbalibale seats: ", avalibale_seats, "/", seats ,"\n")
        if(movies[theater_number,2] != "PG" && movies[theater_number,2] != "PG-13") {
          cat("**children are not allowed because movie is ", movies[theater_number,2], "**\n")
        }

        
        # Calculate the revenue for adults and children
        movie_revenue <- (visitors_adults * ticket_cost) + (visitors_children * ticket_cost_child)
        
        # Calculate revenue, and add to running total for the day
        week_days[day_num] <- week_days[day_num] + movie_revenue
        
        # ------> Need to add Snacks revenue
        # Calculate snacks revenue
        
      }
      # Save total to the corresponding day
      cat("------------------------------------------\n")
      cat("Daliy total revenue: $", week_days[day_num], "\n")
      cat("------------------------------------------\n")
      
    }
    
    # Make a barchart showing total revenue per day
    day_name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    barplot(week_days, names.arg = day_name, horiz = FALSE)

    # Make any other chart
    pie(week_days, label = day_name, main = "Revenue")
    
    # Which day had the highest revenue? 
    cat("The highest revenue is $", max(week_days), "in", day_name[which(week_days == max(week_days))])
