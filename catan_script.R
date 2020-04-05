### CreateRandom()
# this function creates a random game board in form of a dataframe
CreateRandom <- function() {
  
  # declare the total number of tiles in the game and generate IDs for individual tiles
  Tile_No <- 19
  Tile_ID <- c(LETTERS[1:Tile_No])
  
  ## generate a vector of randomized resources
  Resource_list <- c(rep("Wheat", 4), rep("Sheep", 4), rep("Wood", 4), rep("Brick", 3), rep("Ore", 3), "Desert")
  Resources <- sample(Resource_list, Tile_No, replace=F)
  
  ## generate a vector of randomized numbers on tiles
  # order of numbers in 'Numbers_list' taken from the game rules
  Numbers_list <- c(5, 2, 6, 3, 8, 10, 9, 12, 11, 4, 8, 10, 9, 4, 5, 6, 3, 11)
  
  # order of letters in 'Tile_order_n' lists taken from the game rules
  # numbers are put on to tiles in this order
  Tile_order_1 <- c("A", "D", "H", "M", "Q", "R", "S", "P", "L", "G", "C", "B", "E", "I", "N", "O", "K", "F", "J")
  Tile_order_2 <- c("H", "M", "Q", "R", "S", "P", "L", "G", "C", "B", "A", "D", "I", "N", "O", "K", "F", "E", "J")
  Tile_order_3 <- c("Q", "R", "S", "P", "L", "G", "C", "B", "A", "D", "H", "M", "N", "O", "K", "F", "E", "I", "J")
  Tile_order_4 <- c("S", "P", "L", "G", "C", "B", "A", "D", "H", "M", "Q", "R", "O", "K", "F", "E", "I", "N", "J")
  Tile_order_5 <- c("L", "G", "C", "B", "A", "D", "H", "M", "Q", "R", "S", "P", "K", "F", "E", "I", "N", "O", "J")
  Tile_order_6 <- c("C", "B", "A", "D", "H", "M", "Q", "R", "S", "P", "L", "G", "F", "E", "I", "N", "O", "K", "J")
  
  # choose the variant for this game
  choose_list <- list(Tile_order_1, Tile_order_2, Tile_order_3, Tile_order_4, Tile_order_5, Tile_order_6)
  choose_value <- sample(1:6, 1, replace=F)
  Tile_order <- choose_list[[choose_value]]
  
  # create a new vector 'Numbers'
  # assign the correct numbers to Tile_IDs according to Tile_order, save them to 'Numbers'
  # assign 0 to the Desert tile
  Desert_index <- which(Resources=="Desert")
  Numbers_list <- append(Numbers_list, 0, which(Tile_order == Tile_ID[Desert_index])-1)
  
  Numbers <- vector("numeric", 19)
  for (x in 1:length(Tile_ID)) {
    for (i in 1:length(Tile_order)) {
      if  (Tile_ID[x] == Tile_order[i]) {
        Numbers[x] <- Numbers_list[i]
      }
    }
  }
  
  # create a new vector 'Points' which holds the production points of the corresponding tiles
  # Production Points are corresponding to the number of possibilities that this particular number lands after throwing two dice
  Points <- vector("numeric", 19)
  for (i in 1:length(Numbers)) {
    Points[i] <- (6-abs(Numbers[i]-7))
  }
  Points[which(Points == -1)] <- 0
  
  Board <- data.frame(Tile_ID, Resources, Numbers, Points)
  return(Board)
}

### CreateBoard()
# this function creates a board based on user input
CreateBoard <- function(resources_input, numbers_input) {
  
  #check the user input
  if (is.numeric(resources_input) == F | is.numeric(numbers_input) == F) {
    print("Sorry! Your input must have a form of a numeric vector.")
  }
  
  if (length(resources_input) != 19) {
    print("Sorry! You must type in exactly 19 values representing resources on your gameboard.")
  }
  
  if (length(numbers_input) != 19) {
    print("Sorry! You must type in exactly 19 values representing numbers on the tiles of your gameboard.")
  }
  
  if (any(numbers_input > 12)) {
    print("Numbers on the tiles of your gameboard must range from 2 to 12.")
  }
  
  # declare the total number of tiles in the game and generate IDs for individual tiles
  Tile_No <- 19
  Tile_ID <- c(LETTERS[1:Tile_No])
  
  # convert numbers from the numeric vector 'resources_input' into resource names and save them in a character vector
  Resources <- vector("character", Tile_No)
  reference_numbers <- c(0:5)
  reference_resources <- c("Desert", "Wood", "Sheep", "Wheat", "Brick", "Ore")
  
  for (i in 1:length(resources_input)) {
    for (n in 1:length(reference_numbers)) {
      if (resources_input[i] == reference_numbers[n]) {
        Resources[i] <- reference_resources[n]
      }
    }
  }
  
  Numbers <- numbers_input
  
  # create a new vector 'Points' which holds the production points of the corresponding tiles
  # Production Points are corresponding to the number of possibilities that this particular number lands after throwing two dice
  Points <- vector("numeric", 19)
  for (i in 1:length(Numbers)) {
    Points[i] <- (6-abs(Numbers[i]-7))
  }
  Points[which(Points == -1)] <- 0
  
  Board <- data.frame(Tile_ID, Resources, Numbers, Points)
  return(Board)
}

### Evaluate()
# this function finds the resources with the highest and lowest value of production points and prints them in a console message
Evaluate <- function(dframe=Board) {
  Categories <- c("Wheat", "Sheep", "Wood", "Brick", "Ore")
  
  #create a matrix which will hold production point values for the corresponding resources
  prod_mat <- matrix(0, 5, 1, dimnames = list(Categories))
  
  # sum up the production point values for individual resources from the dataframe 'Points' column
  # save the result into the 'prod_mat' matrix
  ## poznamka: uvedomuji si, ze nasledujici kus kodu se zbytecne opakuje a dal by se zkratit tak na 10 radku nejakou elegantni
  ## smyckou, ale vzdycky, kdyz jsem se o to pokusil, tak to dopadlo velice spatne
  positions_wheat <- which(dframe$Resources == "Wheat")
  positions_sheep <- which(dframe$Resources == "Sheep")
  positions_wood <- which(dframe$Resources == "Wood")
  positions_brick <- which(dframe$Resources == "Brick")
  positions_ore <- which(dframe$Resources == "Ore")
  
  value_wheat <- 0
  for (i in positions_wheat) {
    value_wheat <- value_wheat + sum(dframe$Points[i])
  }
  prod_mat[1,1] <- value_wheat
  
  value_sheep <- 0
  for (i in positions_sheep) {
    value_sheep <- value_sheep + sum(dframe$Points[i])
  }
  prod_mat[2,1] <- value_sheep
  
  value_wood <- 0
  for (i in positions_wood) {
    value_wood <- value_wood + sum(dframe$Points[i])
  }
  prod_mat[3,1] <- value_wood
  
  value_brick <- 0
  for (i in positions_brick) {
    value_brick <- value_brick + sum(dframe$Points[i])
  }
  prod_mat[4,1] <- value_brick
  
  value_ore <- 0
  for (i in positions_ore) {
    value_ore <- value_ore + sum(dframe$Points[i])
  }
  prod_mat[5,1] <- value_ore
  
  # find the resources with the most points in the 'prod_mat' matrix and save their names in a new character vector 'max_names'
  # collapse the resource names into a character string 'max_string',
  # if there is more than one resource, separate them with 'and'
  address_max <- which(prod_mat == max(prod_mat))
  max_names <- vector("character", length(address_max))
  for (i in 1:length(address_max)) {
    max_names[i] <- Categories[address_max[i]]
  }
  max_string <- paste(max_names, collapse = ' and ')
  
  # abide by the grammar rules
  if (length(address_max) == 1){
    verb_max <- "is"
  } else {
    verb_max <- "are"
  }
  
  if (length(address_max) == 1){
    noun_max <- "resource"
  } else {
    noun_max <- "resources"
  }
  
  # finally, save the 'max_string', correct singular or plural words and the text to be printed into a new character variable
  Line1 <- paste(max_string, verb_max, "the most abundant", noun_max, "with", max(prod_mat), "production points.")
  
  # find the resources with the least points in the 'prod_mat' matrix and save their names in a new character vector 'min_names'
  # collapse the resource names into a character string 'min_string',
  # if there is more than one resource, separate them with 'and'
  address_min <- which(prod_mat == min(prod_mat))
  min_names <- vector("character", length(address_min))
  for (i in 1:length(address_min)) {
    min_names[i] <- Categories[address_min[i]]
  }
  min_string <- paste(min_names, collapse = ' and ')
  
  # abide by the grammar rules
  if (length(address_min) == 1){
    verb_min <- "is"
  } else {
    verb_min <- "are"
  }
  
  if (length(address_min) == 1){
    noun_min <- "resource"
  } else {
    noun_min <- "resources"
  }
  
  # finally, save the 'min_string', correct singular or plural words and the text to be printed into a new character variable
  Line2 <- paste(min_string, verb_min, "the most scarce", noun_min,"with only", min(prod_mat), "production points.")
  Line3 <- "Keep an eye on those!"
  
  cat(paste(Line1, Line2, Line3, sep = "\n"))
}

### FindSpots()
# this function finds the best initial placements on the gameboard
FindSpots <- function(dframe=Board) {
  vertices <- c("ABE", "BCF", "DEI", "EFJ", "FGK", "HIM", "IJN", "JKO", "KLP", "DIH", "GKL", "MNQ", "NOR", "OPS", "NQR", "ORS", "MIN", "JNO", "OKP", "EIJ", "FJK", "ADE", "BEF", "CFG")
  numbers <- vector("character", length(vertices))
  resources <- vector("character", length(vertices))
  points <- vector("numeric", length(vertices))
  has_ore <- vector("logical", length(vertices))
  
  spots <- data.frame(vertices, numbers, resources, points, has_ore, stringsAsFactors=FALSE)
  
  # fill the columns in 'spots', take data from 'dframe' (Board)
  for (n in 1:length(vertices)) {
    lttrs <- unlist(strsplit(vertices[n], ""))
    
    row_no <- vector("numeric", length(lttrs))
    for (i in 1:length(lttrs)) {
      row_no[i] <- which(dframe$Tile_ID == lttrs[i])
    }
    
    spots$numbers[n] <- paste(dframe$Numbers[row_no[1]], dframe$Numbers[row_no[2]], dframe$Numbers[row_no[3]], sep = "-")
    spots$resources[n] <- paste(dframe$Resources[row_no[1]], dframe$Resources[row_no[2]], dframe$Resources[row_no[3]], sep = "-")
    spots$points[n] <- sum(dframe$Points[row_no[1]], dframe$Points[row_no[2]], dframe$Points[row_no[3]])
    
    if ((dframe$Resources[row_no[1]] == "Ore" | dframe$Resources[row_no[2]] == "Ore" | dframe$Resources[row_no[3]] == "Ore")) {
      spots$has_ore[n] <- T
    }
  }
  
  m.spots <- head(spots[order(spots$points, spots$has_ore, decreasing = T),], length(which(spots$points >= 10)))
  best_vertices <- m.spots[,2:4]
  names(best_vertices) <- c("Numbers", "Resources", "Points")
  
  cat("The best initial placements for your villages are:", "\n")
  print(best_vertices, row.names = F)
}

### FindComplement()
# this function finds a suitable initial placement for player's second village based on the resources they covered with their first
# it finds such a location that complements all the 5 resources if you they to employ 'the generalist' strategy
FindComplement <- function(arg1, arg2, arg3, dframe=Board) {
  Categories <- c("Wheat", "Sheep", "Wood", "Brick", "Ore")
  
  #find what resources does player still need
  first_resources <- c(arg1, arg2, arg3)
  second_resources <- setdiff(Categories, first_resources)
  
  # create a dataframe of vertices, same as in FindSpots() minus the 'has_ore' column
  vertices <- c("ABE", "BCF", "DEI", "EFJ", "FGK", "HIM", "IJN", "JKO", "KLP", "DIH", "GKL", "MNQ", "NOR", "OPS", "NQR", "ORS", "MIN", "JNO", "OKP", "EIJ", "FJK", "ADE", "BEF", "CFG")
  numbers <- vector("character", length(vertices))
  resources <- vector("character", length(vertices))
  points <- vector("numeric", length(vertices))
  
  second_placement <- data.frame(vertices, numbers, resources, points, stringsAsFactors=FALSE)
  
  # fill the columns in 'second_placement', take data from 'dframe' (Board)
  for (n in 1:length(vertices)) {
    lttrs <- unlist(strsplit(vertices[n], ""))
    
    row_no <- vector("numeric", length(lttrs))
    for (i in 1:length(lttrs)) {
      row_no[i] <- which(dframe$Tile_ID == lttrs[i])
    }
    
    second_placement$numbers[n] <- paste(dframe$Numbers[row_no[1]], dframe$Numbers[row_no[2]], dframe$Numbers[row_no[3]], sep = "-")
    second_placement$resources[n] <- paste(dframe$Resources[row_no[1]], dframe$Resources[row_no[2]], dframe$Resources[row_no[3]], sep = "-")
    second_placement$points[n] <- sum(dframe$Points[row_no[1]], dframe$Points[row_no[2]], dframe$Points[row_no[3]])
  }
  
  # get the resource triads of vertices from the dataframe
  # convert them from strings back to character vectors which are saved in a list
  three_resources <- vector("list", length(second_placement$resources))
  for (i in 1:length(second_placement$resources)) {
    three_resources[i] <- strsplit(second_placement$resources[i], "-")
  }
  
  # find the numbers of rows in the 'second_placement' dataframe which contain the suitable vertices
  address <- vector("numeric")
  for (i in 1:length(three_resources)) {
    if (sum(second_resources %in% unlist(three_resources[i])) == length(second_resources)) {
      address[i] <- i
    }
  }
  true_address <- as.numeric(na.omit(address))
  
  # construct a new dataframe which contains only the suitable vertices
  new_numbers <- vector("character", length(true_address))
  new_resources <- vector("character", length(true_address))
  new_points <- vector("numeric", length(true_address))
  for (i in 1:length(true_address)) {
    new_numbers[i] <- second_placement$numbers[true_address[i]]
    new_resources[i] <- second_placement$resources[true_address[i]]
    new_points[i] <- second_placement$points[true_address[i]]
  }
  new_second_placement <- data.frame(new_numbers, new_resources, new_points)
  new_second_placement <- new_second_placement[order(new_second_placement$new_points, decreasing = T),]
  names(new_second_placement) <- c("Numbers", "Resources", "Points")
  
  if (length(address) == 0) {
    print("Sorry! I could not find any placements that would give you all 5 resources.")
  } else {
    cat("Here are the spots that would give you all 5 resources:", "\n")
    print(new_second_placement, row.names = F, colnames = F)
  }
}