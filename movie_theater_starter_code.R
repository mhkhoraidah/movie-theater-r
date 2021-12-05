#Theaters<-read.csv('Data governance bootcamp\\R\\Week 6\\day5\\movie-theater-r\\Theaters.csv',stringsAsFactors = FALSE)
#print(Theaters)



# 1. Create snacks that the customers can buy and randomize who buys which snack

###################################### Snacks ##########################################

#Create snacks dataframe to store the sales
snacksDF<-data.frame(Branch=factor(),Theater=integer(), Small.Popcorn=integer(), 
                     Medium.Popcorn=integer(), Large.Popcorn=integer(),
                     Icecream=integer(),Soft.Drink=integer(), Frozen=integer())
snacksDF<-data.frame(Branch=0,Theater=0,Small.Popcorn=0L, 
                     Medium.Popcorn=0L, Large.Popcorn=0L,
                     Icecream=0L,Soft.drink=0L, Frozen=0L)

################################## Snacks Function #####################################

# the function takes snacks data frame to store the sales on it, Branch name, theater number
# and takes number of visitors.It is return the total revenue of the day for a theater.
snacksF<-function(snacksDF, BranchName, TheaterN, visitorsN){
  #snacks prices
  prices<-c(6, 8, 10, 8, 7, 9)
  #create a vector of sold snacks
  snack<-sample(x = 1:visitorsN, size = 6, replace = TRUE)
  #calculate the sales of snacks for a screen
  snacksSales<- t(snack)%*%prices
  snacksSales<-snacksSales[1]
  #bind it with the branch and theater
  snack<-c(BranchName, TheaterN,snack)
  #print(snack)
  #add it to the dataframe
  snacksDF<-rbind.data.frame(snacksDF,snack)
  #Assign it to golbal var
  snacksDF<<-snacksDF
  return (snacksSales)
}

###############################################################################################

# Movie List
# DATA:         Movie Name  | Age Rating  | Number of Shows each day
movies <- rbind(c("Limitless", "PG-13", 3), 
                c("Taken", "PG-13", 1), 
                c("The Old Guard", "R", 2),
                c("Mr. & Mrs. Smith", "PG-13", 3),
                c("Focus", "+16", 2),
                c("Raya and the Last Dragon", "PG", 4),
                c("Rush Hour 3", "PG-13", 3))
# Branchs and size of the each branch based on the number of screens have
#  DATA:       Branch Location  | VIP | Standard | MAX
branchs <-  rbind(c("Riyadh", 2, 5, 2), 
                  c("Dammam", 1, 3, 1), 
                  c("Jeddah", 1, 4, 1))
# Theater Types based on capacity and ticket cost
# DATA:         Type Name  | Seats  | Adult Cost  | Child Cost
theatersType <- rbind(c("VIP",      60,  40, 30),
                      c("Standard", 120, 20, 15),
                      c("MAX",      180, 25, 20))

# This function generate a theaters data set for multiple branch by passing branchs and theatersTypes
generateDataSet <- function(branchs, theatersType){
  # Initialize the dataset columns to empty state
  branch_col <- NULL
  theater_num_col <- NULL
  type_col <- NULL
  seats_col <- NULL
  adult_cost_col <- NULL
  child_cost_col <- NULL
  movie_name_col <- NULL
  movie_rating_age_col <- NULL
  show_number_col <- NULL
  
  
  # Loop over branchs and theatersType to create dataset
  for(row in 1:nrow(branchs)) {
    
    # Count theater number for each branch in total including all types of theater (VIP, Standard, Max)
    countTheater <- 0
    
    for(col in 2:ncol(branchs)) {
      # total screen for each theater type and covert the string digit to integer number
      screensCount <- as.numeric(branchs[row, col])
      # calculate the total of all screens for each branch
      countTheater <- countTheater + screensCount
      
      # Loading the data for each column
      branch_col     <- c(branch_col, rep(branchs[row], screensCount))
      type_col       <- c(type_col, rep(theatersType[col-1], screensCount))
      seats_col      <- c(seats_col, rep(theatersType[col+2], screensCount))
      adult_cost_col <- c(adult_cost_col, rep(theatersType[col+5], screensCount))
      child_cost_col <- c(child_cost_col, rep(theatersType[col+8], screensCount))
    }
    
    # Create random movies assign for each theater
    for(movie in 1:countTheater) {
      # Get a random movie form Movies List in the top
      random_movie_index <- sample(1:nrow(movies),1)
      
      # Loading and assign random movies to each theater
      movie_name_col <- c(movie_name_col, movies[random_movie_index,1])
      movie_rating_age_col <- c(movie_rating_age_col, movies[random_movie_index,2])
      show_number_col <- c(show_number_col, sample(1:4, 1))
    }
    
    # List of theaters number for all branch
    theater_num_col <- c(theater_num_col, c(1:countTheater))
  }
  
  # create the data frame 
  dataset = data.frame(Branch         = branch_col,
                       Theater.Number = theater_num_col,
                       Type           = type_col,
                       Seats          = seats_col,
                       Adult.Cost     = adult_cost_col,
                       Child.Cost     = child_cost_col,
                       Movie_Name      = movie_name_col,
                       Age.Rating = movie_rating_age_col,
                       Show_Number      = show_number_col,
                       stringsAsFactors = FALSE)
  
  return(dataset)
}

# Call generateDataSet function
Theaters<-generateDataSet(branchs, theatersType)



##################################### Theaters Function ######################################
# It takes theater dataframe and returns the week revenues for each branch as a dataframe
runTheaters<-function(theatersDF, moviesDF){
  
  #Get branches names from the dataset
  uniqueBranch <- unique(theatersDF[c('Branch')])
  uniqueBranch<-uniqueBranch[,1]
  branchesRevenue<-data.frame(uniqueBranch,stringsAsFactors = FALSE)
  
  #create revenues dataset, 7 days for each branch
  for (i in 1:7){
    d <- rep(0, length(uniqueBranch))
    branchesRevenue<- cbind(branchesRevenue,d)
  }
  
  #iterate through theaters
  for (i in 1:nrow(theatersDF)){
    
    # iterate through the week
    for (day_num in 1:7) {
      # Keep track of total revenue for the day
      daily_revenue <- 0
      
      #get theater info
      seats<- as.numeric(theatersDF[i, ]['Seats'])
      branchName<- as.character(theatersDF[i, ]['Branch'])
      theaterN<- as.numeric(theatersDF[i, ]['Theater.Number'])
      AdultCost<- as.numeric(theatersDF[i, ]['Adult.Cost'])
      ChildCost<- as.numeric(theatersDF[i, ]['Child.Cost'])
      AgeRating<- as.character(theatersDF[i,]['Age.Rating'])
      show_number<- as.numeric(theatersDF[i,]['Show_Number'])
      
      #The sample starts by 1 or half of the space if it is weekend
      startSeat=1L
      if (day_num %in% (5:7)){
        startSeat= seats %/% 2
      }
      
      for (o in 1:show_number){
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(startSeat:seats, 1)
        # student discount
        end <- round(visitors_adults/2)
        students<- sample(1:end, 1)
        studentsRevenue<- studentDiscount(AdultCost, students)
        
        visitors_adults<-visitors_adults - students
        avalibale_seats <- seats - visitors_adults
        visitors_children <- 0
        
        # conditional statements for movies that may be PG-13 and children are not allowed to watch
        if( AgeRating == "PG" || AgeRating == "PG-13") {
          visitors_children <- sample(1:avalibale_seats,1) # this is should be the the rest of available seats or less
          avalibale_seats <- avalibale_seats - visitors_children
        } 
        
        # Calculate the revenue for adults and children
        movie_revenue <- (visitors_adults * AdultCost) + (visitors_children * ChildCost) + studentsRevenue
        
        # Calculate snacks revenue
        snakcsRevenue<-snacksF(snacksDF, branchName, theaterN, seats-avalibale_seats)
        # Calculate the total revenue of the day, and add it to the revenues datafram by matching
        # the branch name and the day index
        x<-which(uniqueBranch == branchName )
        branchesRevenue[x,day_num+1] <- branchesRevenue[x ,day_num+1]  + movie_revenue + snakcsRevenue
        
      }
      
      
    }
    
  }
  #Rename the columns to correct names
  names(branchesRevenue)[2]<-"Sunday"
  names(branchesRevenue)[3]<-"Monday"
  names(branchesRevenue)[4]<-"Tuesday"
  names(branchesRevenue)[5]<-"Wednesday"
  names(branchesRevenue)[6]<-"Thursday"
  names(branchesRevenue)[7]<-"Friday"
  names(branchesRevenue)[8]<-"Saturday"
  
  cat("---------------------------------------------------------------------\n")
  cat("\t The Total revenues For The Theaters", "\n")
  print((branchesRevenue))
  cat("---------------------------------------------------------------------\n")
  return (branchesRevenue)
}

#################################### print ###################################################
# Display the max revenue for each branch 
printMaxRevenue<-function(RevenuesDF){
  # Transpose the dataset
  newDF=t(RevenuesDF)
  
  for (i in 1: ncol(newDF)){
    cat("------------------------------------------\n")
    cat("Maximum total revenue of the week for", newDF[1,i],": $", max(newDF[2:nrow(newDF),i]), "\n")
    cat("------------------------------------------\n")
  }
  
}
################################################ dicsount function #####################################

# Student Discount %20 and return the total discount
studentDiscount <- function(ticket_cost, studentsvisitors) {
  discounted_ticket <- ticket_cost * 0.2
  return(studentsvisitors *discounted_ticket)
}


data<-runTheaters(Theaters, movies)
printMaxRevenue(data)
View(Theaters)
View(data)


#-------------------------------------------------------------------------------
# first the names
branchname <- data$uniqueBranch

# transpose all but the first column (name)
data <- as.data.frame(t(data[,-1]))
colnames(data) <- branchname
data$myfactor <- factor(row.names(data))
#str(data) # Check the column types

# Make chart
# Pie Chart with Percentages
slices <- c(sum(data$Riyadh), sum(data$Dammam), sum(data$Jeddah))

lbls <- c('Riyadh', 'Dammam', 'Jeddah')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col= c("#8b0000" ,"#696969" ,"#ffcc33"),
    main=paste("Total Revenue of ALL Branches"))




# third plot
bName<-'Riyadh'
slices1 <- c(sum(data$Sunday),sum(data$Monday),sum(data$Tuesday),sum(data$Tuesday),sum(data$Wednesday),sum(data$Thursday),sum(data$Saturday))

lbls1 <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Satudray')
pct1 <- round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels
barplot(data$Riyadh, main=paste("Total Revenue per Day for Riyadh Branch"),
    xlab="Day", names.arg=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Satudray'))

# forth plot
bName2<-'Dammam'
slices2 <- c(sum(data$Sunday),sum(data$Monday),sum(data$Tuesday),sum(data$Tuesday),sum(data$Wednesday),sum(data$Thursday),sum(data$Saturday))

lbls2 <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Satudray')
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels
barplot(data$Dammam, main=paste("Total Revenue per Day for Dammam Branch"),
        xlab="Day", names.arg=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Satudray'))

# Fifth plot
bName3<-'Jeddah'
slices3 <- c(sum(data$Sunday),sum(data$Monday),sum(data$Tuesday),sum(data$Tuesday),sum(data$Wednesday),sum(data$Thursday),sum(data$Saturday))

lbls3 <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Satudray')
pct3 <- round(slices3/sum(slices3)*100)
lbls3 <- paste(lbls3, pct3) # add percents to labels
lbls3 <- paste(lbls3,"%",sep="") # ad % to labels
barplot(data$Jeddah, main=paste("Total Revenue per Day for Jeddah Branch"),
        xlab="Day", names.arg=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Satudray'))



# Make any other chart
# Pie Chart with Percentages

slices2 <- c(sum(as.integer(snacksDF$Small.Popcorn) ), sum(as.integer(snacksDF$Medium.Popcorn) ),
             sum(as.integer(snacksDF$Large.Popcorn) ),sum(as.integer(snacksDF$Icecream) ),
             sum(as.integer(snacksDF$Soft.drink) ), sum(as.integer(snacksDF$Frozen) ) )

lbls2 <- c('Small Popcorn', 'Medium Popcorn', 'Large Popcorn', 'Icecream', 'Soft Drink', 'Frozen')
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels
pie(slices2,labels = lbls2, col=c("#d3d3d3","#808080","#f8de7e","#daa520","#b22222","#800000"),
    main=paste("Sales Percentages of snacks on", bName, 'Branch'))

