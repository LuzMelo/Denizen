library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

d2 <-read.csv("C:/Users/luzya/OneDrive/Escritorio/PhD program/Spring 2024/Data Science Internship/Dinner Series #3 - All Data sorted by attendees.csv")
restaurant_info <- read.csv("C:/Users/luzya/OneDrive/Escritorio/PhD program/Spring 2024/Data Science Internship/restaurants_info.csv")
# <- read.csv("/Users/skyejung/Desktop/denizen/Dinner Series #3 - RSVP List.csv")
#only yes for dinners
d2 <- d2[d2$X.1 == "I will attend",]

#editing phone numbers to match so we can join data

# d2$X.2 <- gsub("^.{0,2}", "", d2$X)
names(d2)[names(d2) == 'X'] <- 'phone'
# d2$phone <- gsub("[^0-9]", "", d2$phone)

# allData <- plyr::join(d2, rsvps, by = "phone") 
# allData <- allData[!is.na(allData$X.3),]

allData <- d2

# transform the checklist variables into 0s and 1s
cols_to_transform <-c(82:113)
allData[cols_to_transform] <- lapply(allData[cols_to_transform], function(x) ifelse(x == "" | is.na(x), 0, 1))

# Transform the multiple-choice commitment-focused questions into numbers
allData$howOftenCanYouHang <-dplyr::recode(allData$How.often._.can._.you.hang.out.with.friends.in.this.stage.of.your.life., "Once per month"=.25, "Bi-weekly" = .5, "Once a week"=1,"Two-Three time a week"=2.5, "Daily"=7, .default =.5)

allData$howOftenDoYouWantToHang <- dplyr::recode(allData$How.often._.do.you.want.to._.hang.out.with.friends.in.this.stage.of.your.life., "Once per month"=.25, "Bi-weekly" = .5, "Once a week"=1,"Two-Three time a week"=2.5, "Daily"=7, .default =.5)

allData$whenHang <-dplyr::recode(allData$When.do.you.prefer.hanging.out., "Weekends"=1, "During the Week" = 0,"Either works for me"=.5, .default =.5)

allData$spend <- dplyr::recode(allData$How.much.are.you.willing.to.spend.when.out.with.friends.,"Less than $25"= 1, "$25 to $50"=2, "$50 to $100"=3, "$100 to $200"=4)

# scale the now-numeric variables that aren't 0s and 1s
allData$howOftenCanYouHangS <-(allData$howOftenCanYouHang-mean(allData$howOftenCanYouHang,na.rm=T))/sd(allData$howOftenCanYouHang,na.rm=T)

allData$howOftenDoYouWantToHangS <-(allData$howOftenDoYouWantToHang-mean(allData$howOftenDoYouWantToHang,na.rm=T))/sd(allData$howOftenDoYouWantToHang,na.rm=T)

allData$spendS <-(allData$spend-mean(allData$spend,na.rm=T))/sd(allData$spend,na.rm=T)

# replace the missing values with average responses (aka, 0s)
allData <- allData %>% replace_na(list(howOftenCanYouHangS= 0, howOftenDoYouWantToHangS=0, spendS= 0))

allData$dob <- as.numeric(strtrim(allData$What.is.your.date.of.birth, 4))

allData$age <- scale(2023-allData$dob)

allData$wealthMatter <- scale(as.numeric(allData$Does.wealth.matter.to.me.))

allData$careerPriority <- scale(as.numeric(allData$My.career.is.my.top.priority))

# getting the humor things together, without the others and NAs

humorCols <- c("Dave.Chappelle..",  "Richard.Pryor..",   "Tig.Notaro..",      "Eddie.Murphy.",    
               "George.Carlin..",   "Sarah.Silverman..", "Gabriel.Iglesias..", "John.Mulaney.." , 
               "Marc.Maron..",       "Wanda.Sykes.." , "Weird.Al.Yankovic..", "Charlie.Chaplin..",
               "Bill.Burr.."  ,       "Amy.Schumer.."  ,    
               "Louis.C.K..."  ,      "Anthony.Jeselnik.." , "Ricky.Gervais..", "The.Hangover", "The.Dictator"   ,                      
              "Ted"     , "Horrible.Bosses"  ,                    
               "Vacation" ,   "Superbad"      ,                       
                "Bridesmaids"     ,                      "Airplane."   ,                         
              "Anchorman..The.Legend.of.Ron.Burgundy", "Step.Brothers"  ,                      
               "Zoolander"      ,                       "Old.School"   ,                        
                "The.40.Year.Old.Virgin"  ,              "Monty.Python.and.the.Holy.Grail"   ,   
               "Groundhog.Day"         ,                "Dumb.and.Dumber"   ,                   
                "Tropic.Thunder"      ,                  "Ghostbusters"  ,                       
                "Office.Space"       ,                   "Shaun.of.the.Dead" ,                   
               "Borat" )  

newCols <- paste0(humorCols, "C")

for (col in humorCols) {
  if(col %in% names(allData)) {
    newCol <- paste0(col, "C")
    allData[[newCol]] <- ifelse(allData[[col]] == "", 0, 1)
  } else {
    warning(paste("Column", col, "does not exist in allData. Skipping..."))
  }
}


allData <- allData[-28,]


################################################################################
names(allData)[names(allData) == "How.much.are.you.willing.to.spend.when.out.with.friends."] <- 'price_point'

price_point_mapping <- c('Less than $25' = 1,
                         '$25 to $50' = 2,
                         '$50 to $100' = 3,
                         '$100 to $200' = 4,
                         '$200 or more' = 5)

# Apply the mapping to the price_point column to create a new numeric_price column
allData$price_point <- as.integer(sapply(allData$price_point, function(x) price_point_mapping[x]))

# Sort allData by price point
allData <- allData[order(allData$price_point),]

# Print total number of participants
total_participants <- nrow(allData)
cat("Total number of participants:", total_participants, "\n\n")

# Show the distribution of participants across price points
cat("Distribution of participants by price range:\n")
price_distribution <- table(allData$price_point)
print(price_distribution)
cat("\n")

# Ask the user for the number of participants in each restaurant
restaurant_info$DesiredParticipants <- numeric(nrow(restaurant_info))
remaining_participants <- total_participants

for (i in seq_len(nrow(restaurant_info))) {
  repeat {
    cat("The restaurant", restaurant_info$Restaurant[i], "has a price point of", 
        restaurant_info$Price_point[i], "\n")
    cat("Enter the number of participants for", restaurant_info$Restaurant[i],
        "[Capacity:", restaurant_info$Capacity[i], ", Remaining participants:", 
        remaining_participants, "]: ")
    desired_count <- as.integer(readline())
    
    if (is.na(desired_count) || desired_count < 0) {
      cat("Please enter a valid number.\n")
    } else if (desired_count > remaining_participants) {
      cat("Not enough remaining participants. Please enter a smaller number.\n")
    } else if (desired_count > restaurant_info$Capacity[i]) {
      cat("The number exceeds the capacity of the restaurant. Please enter a smaller number.\n")
    } else {
      restaurant_info$DesiredParticipants[i] <- desired_count
      remaining_participants <- remaining_participants - desired_count
      cat("You have assigned", desired_count, "participants to", restaurant_info$Restaurant[i], "\n")
      cat("Participants remaining: ", remaining_participants, "\n\n")
      break
    }
  }
}

# Create a new column in allData for the restaurant assignment
allData$Restaurant <- NA
allData$Address <- NA

# Assign participants to restaurants and their addresses based on the ordered list
start_index <- 1
for (i in seq_len(nrow(restaurant_info))) {
  end_index <- min(start_index + restaurant_info$DesiredParticipants[i] - 1, total_participants)
  allData$Restaurant[start_index:end_index] <- restaurant_info$Restaurant[i]
  allData$Address[start_index:end_index] <- restaurant_info$Address[i]
  start_index <- end_index + 1
}

dK2 <- allData[,c(82:113, 300:302, 304:306)] #with age, career, wealth
dK2humor <- allData[,c(82:113, 300:302,304:332)] #with age, career, wealth, humor

dK2humor$Restaurant <- allData$Restaurant

############################# GET EVEN CLUSTERS ###############################
# Set seed for reproducibility
set.seed(2023)

get_even_clusters <- function(X, cluster_size) {
  # Determine the required number of clusters
  n_clusters <- ceiling(nrow(X) / cluster_size)
  
  # Perform initial clustering to find approximate centers
  set.seed(42) # Ensure reproducibility
  km_result <- kmeans(X, centers = n_clusters)
  
  # Initialize vector for custom cluster assignments
  custom_assignments <- rep(NA, nrow(X))
  
  # Initialize a count of members for each cluster
  cluster_members_count <- rep(0, n_clusters)
  
  # Assign data points to clusters, respecting cluster size limits
  for (i in 1:nrow(X)) {
    distances <- numeric(n_clusters)
    for (j in 1:n_clusters) {
      # Calculate Euclidean distance from data point i to cluster center j
      distances[j] <- sqrt(sum((X[i, ] - km_result$centers[j, ])^2))
    }
    
    # Attempt to assign to nearest cluster with availability
    for (j in order(distances)) {
      if (cluster_members_count[j] < cluster_size) {
        custom_assignments[i] <- j
        cluster_members_count[j] <- cluster_members_count[j] + 1
        break
      }
    }
  }
  
  # Handle any unassigned data points
  unassigned_indices <- which(is.na(custom_assignments))
  for (i in unassigned_indices) {
    # Find a cluster with available space
    least_populated_cluster <- which.min(cluster_members_count)
    custom_assignments[i] <- least_populated_cluster
    cluster_members_count[least_populated_cluster] <- cluster_members_count[least_populated_cluster] + 1
  }
  
  return(custom_assignments)
}

###############################################################################
# Ask the user for the number of participants per table for each restaurant
# Add a 'Group' column to store the group assignment for each participant
allData$Group <- NA

for (i in seq_len(nrow(restaurant_info))) {
  repeat {
    cat("Enter the number of participants per table for", restaurant_info$Restaurant[i],
        "[Max per table:", restaurant_info$Max.per.table[i], "]: ")
    cluster_size <- as.integer(readline())
    
    if (is.na(cluster_size) || cluster_size <= 0) {
      cat("Please enter a valid number greater than 0.\n")
    } else if (cluster_size > restaurant_info$Max.per.table[i]) {
      cat("The number exceeds the maximum amount of participants allowed per table. Please enter a smaller number.\n")
    } else {
      # Apply the get_even_clusters function for participants assigned to this restaurant
      participants_in_restaurant <- dK2humor[dK2humor$Restaurant == restaurant_info$Restaurant[i],]
      
      if (nrow(participants_in_restaurant) > 0) {
        # Apply even k-means clustering
        cluster_assignments <- get_even_clusters(participants_in_restaurant[, -ncol(participants_in_restaurant)], cluster_size)
        
        # Update the Group for each participant
        allData$Group[allData$Restaurant == restaurant_info$Restaurant[i]] <- cluster_assignments
      }
      
      # Move on to the next restaurant
      break
    }
  }
}
#############################################################################

dOutput3 <- allData[,c("phone", "Please.enter.your.first.name","Please.enter.your.last.name", "Please.enter.your.email","What.s.your.gender.identity", "dob","price_point","I.often.take.the.initiative.to.keep.the.conversation.flowing.when.meeting.new.people.", "Restaurant", "Address", "Group")]

dOutput3$dob <- 2023 - dOutput3$dob 

#psych::describeBy(dOutput3[5:9], group = "weightedKmeansHumorCluster8")

write.csv(dOutput3, "C:/Users/luzya/OneDrive/Escritorio/PhD program/Spring 2024/Data Science Internship/cluster_assignments_240222_v3.csv")
write.csv(allData, "all_data_240222_v3.csv")

