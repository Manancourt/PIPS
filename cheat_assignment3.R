


cheat_assignment3 <- function() {
  
  text <- "###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###            ASSIGNMENT 3-1   BEAU MANANCOURT   26-01-2023            ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  # Load packages
  library(ggplot2)
  library(ggpubr)
  library(ggstatsplot)
  library(plotly)
  library(gganimate)
  library(gifski)
  library(cranlogs)
  library(quantmod)
  library(devtools)
  library(htmltools)
  library(memer)
  
  
  ##################################################################
  ##                            Q3.1.1                            ##
  ##################################################################
  
  # Simulate grades from normal distribution
  set.seed(6)
  grades <- rnorm(65, mean = 7.5, sd = 1)
  df1 <- data.frame(grades)
  hist(grades)
  
  
  
  ##################################################################
  ##                            Q3.1.2                            ##
  ##################################################################
  
  # Read data
  data2 <- read.csv('schiphol_data.csv')
  df2 <- data.frame(data2)
  head(df2)
  
  # Scatterplot
  plot(df2$DATE, df2$TMAX)
  
  
  
  ##################################################################
  ##                            Q3.1.3                            ##
  ##################################################################
  
  # Read data
  data3 <- read.csv('titanic_train.csv')
  df3 <- data.frame(data3)
  
  # Recreate barchart
  p3 <- ggplot(df3, aes(x = Sex)) + 
    geom_bar(aes(fill = factor(Survived)))+
    labs(fill = 'How did it go?') +
    scale_fill_discrete(labels=c('dead', 'alive'))
  p3
  
  
  
  ##################################################################
  ##                            Q3.1.4                            ##
  ##################################################################
  
  #' --------------------------------------------------------------┐ 
  #' ANSWER:                                                       |
  #' --------------------------------------------------------------┤
  #' 'Void' is the worst theme because it strips away most         |
  #' information needed to effectively interpret a plot.           |                                   
  #' --------------------------------------------------------------┘
  
  # Creating different plots + themes:
  
  # Linedraw
  p3 + theme_linedraw()
  
  # Light
  p3 + theme_light()
  
  # Dark
  p3 + theme_dark()
  
  # Minimal
  p3 + theme_minimal()
  
  # Classic
  p3 + theme_classic()
  
  # Void 
  p3 + theme_void() + 
    labs(title = 'HELLO ITS THE THEME POLICE UR UNDER ARREST')
  
  
  
  ##################################################################
  ##                            Q3.1.5                            ##
  ##################################################################
  
  # Original plot
  plot(ToothGrowth$supp, ToothGrowth$len)
  
  # Edit 1: Informative labels / title
  plot(ToothGrowth$supp, ToothGrowth$len, 
       xlab = 'Supplement Type', ylab = 'Length (mm.)', 
       main = 'Guinea Pig Odontoblast Length Per \n Vitamin C Delivery Method')
  
  # Edit 2: Different groups, different colors
  plot(ToothGrowth$supp, ToothGrowth$len, 
       xlab = 'Supplement Type', ylab = 'Length (mm.)',
       main = 'Guinea Pig Odontoblast Length Per \n Vitamin C Delivery Method',
       col = c('#B7ADCF', '#4F646F'))
  
  # Edit 3: Add legend
  plot(ToothGrowth$supp, ToothGrowth$len, 
       xlab = 'Supplement Type', ylab = 'Length (mm.)',
       main = 'Guinea Pig Odontoblast Length Per \n Vitamin C Delivery Method',
       col = c('#B7ADCF', '#4F646F'))
  legend('topleft', legend = c('Orange Juice', 'Ascorbic Acid'), 
         fill =  c('#B7ADCF', '#4F646F'), title = 'Delivery Method',
         cex = 0.9, horiz = TRUE)
  
  
  
  ##################################################################
  ##                            Q3.1.6                            ##
  ##################################################################
  
  # Load data / create data frame
  df6 <- data.frame(ChickWeight)
  
  # Compute max weight
  max_weight <- numeric()
  for(i in 1:max(as.numeric(factor(df6$Chick)))) {
    temp_chick <- subset(df6$weight, df6$Chick == i)
    max_weight[i] <- max(temp_chick)
  }
  
  # Create data frame for ggplot
  chick_i <- c(1, 20, 3, 40, 5)
  chick <- factor(chick_i, levels = c('1', '2', '3', '40', '5'))
  relevant_weight <- c(max_weight[chick_i])
  df6b <- data.frame(chick = chick, max_weight = relevant_weight)
  
  # Recreate barchart
  p6 <- ggplot(df6b, aes(x = chick, y = max_weight)) +
    geom_bar(stat = 'identity')
  
  p6
  
  
  
  ##################################################################
  ##                            Q3.1.7                            ##
  ##################################################################
  
  # Load data / create data frame
  df7 <- data.frame(ChickWeight)
  
  # Recreate plot
  p7 <- ggplot(df7, aes(x = Time, y = weight)) +
    stat_smooth(method = 'lm', formula = y ~ x)
  p7
  
  
  
  ##################################################################
  ##                            Q3.1.8                            ##
  ##################################################################
  
  # Create data frame for time-series plot
  df8 <- data.frame(subset(df6, 
                           Chick == 1 | Chick == 20 | Chick == 3 | Chick == 40 | Chick == 5))
  
  # Create factors for time-series plot (needed for right order in legend)
  df8$Chick <- factor(df8$Chick, levels = c('1', '20', '3', '40', '5'))
  
  # Barchart from Q3.1.6
  p8a <- ggplot(df6b, aes(x = chick, y = max_weight)) +
    geom_bar(stat = 'identity')
  
  
  # Time-series plot
  p8b <- ggplot(df8, aes(x = Time, y = weight, color = Chick)) +
    geom_line(size = 0.7) +
    scale_color_manual(values = c('1' = '#673173', '20' = '#3b528b', 
                                  '3' = '#51a6a3', '40' = '#77ce7c', 
                                  '5' = '#fae843')) 
  
  # Display plots together
  ggpubr::ggarrange(p8a, p8b, ncol = 2, nrow = 1)
  
  
  
  ##################################################################
  ##                            Q3.1.9                            ##
  ##################################################################
  
  # Load data
  df9 <- ToothGrowth
  
  # Show plot
  ggstatsplot::ggbetweenstats(data = ToothGrowth, 
                              x = supp, 
                              y = len, 
                              plot.type = 'box')
  
  # Administering vitamin C through OJ leads to longer teeth!
  
  #################################################################
  ##                           Q3.1.10                           ##
  #################################################################
  
  # Load data
  data10 <- read.csv('Body Measurements _ original_CSV.csv')
  head(data10)
  
  # 3D scatterplot
  p10 <- plot_ly(data10, x = ~TotalHeight, y = ~LegLength, z = ~ShoulderToWaist,
                 type = 'scatter3d', mode = 'markers' , opacity = 0.5)
  p10
  
  
  
  #################################################################
  ##                           Q3.1.11                           ##
  #################################################################
  
  # Download data and create data frame
  data11 <- cran_downloads(packages = c('caret', 'tidymodels'), 
                           from = '2013-01-01', to = '2022-12-31')
  df11 <- data.frame(data11)
  df11$package <- factor(data11$package, levels = c('caret', 'tidymodels'))
  
  # Create time-series plot
  p11 <- ggplot(df11, aes(x = date, y = count, color = package)) +
    geom_line(size = 0.8) +
    scale_color_manual(values = c(caret = '#f8766d', 
                                  tidymodels = '#10c3c8')) +
    ylab('Package Downloads') +
    ggtitle('Package popularity over time') +
    theme_bw()
  p11
  
  # Create animation
  p11 + transition_reveal(date)
  
  
  
  #################################################################
  ##                           Q3.1.12                           ##
  #################################################################
  
  # Get stock data
  data12 <- getSymbols('VIST',
                       from = '2022/01/01',
                       to = '2022/12/31',
                       periodicity = 'daily', auto.assign = FALSE)
  
  # Plot time-series data
  chart_Series(data12)
  
  #' --------------------------------------------------------------┐ 
  #' ANSWER:                                                       |
  #' --------------------------------------------------------------┤
  #' Vista Energy does gas / oil exploration                       |
  #' --------------------------------------------------------------┘
  
  
  
  #################################################################
  ##                           Q3.1.13                           ##
  #################################################################
  
  plotstock <- function(symbol = 'VIST', year = 2021, file_name = 'plot',
                        width = 500, height = 500) {
    # Require package
    require(quantmod)
    
    # Variables to specify year
    my_year <- as.character(year)
    my_from <- as.character(paste(my_year, '/01/01', sep = ''))
    my_to   <- as.character(paste(my_year, '/12/31', sep = ''))
    
    # Get stock data
    stonks <- getSymbols(symbol, from = my_from, to = my_to, 
                         periodicity = 'daily', auto.assign = FALSE)
    
    # Save and present plot
    my_file_name = paste(file_name, '.png' sep = '')
    png(file = my_file_name, width = width, height = height)
    my_plot <- chart_Series(stonks, name = symbol)
    my_plot
    dev.off()
    print(my_plot)
  }
  
  plotstock(symbol = 'AAPL', year = 2019, file_name = 'test5')
  
  
  
  #################################################################
  ##                           Q3.1.14                           ##
  #################################################################
  
  #' --------------------------------------------------------------┐ 
  #' IMPROVING Q3.1.1:                                             |
  #' --------------------------------------------------------------┤
  #' I recreated the plot in ggplot2 for nicer visuals. I added    |
  #' informative labels and a title. I also used a theme and added |
  #' some color. Lastly, I added a vertical line to display the    | 
  #' mean of the grade distribution.                               | 
  #' --------------------------------------------------------------┘
  
  # Simulate grades from normal distribution
  set.seed(6)
  grades <- rnorm(65, mean = 7.5, sd = 1)
  df14a <- data.frame(grades)
  
  # Histogram
  p14a <- ggplot(df14a, aes(x = grades)) + 
    geom_histogram(color = 'black', fill = 'lightcyan3', 
                   binwidth = 0.4, size = 0.8) +
    geom_vline(aes(xintercept = mean(grades)), color = 'magenta4', 
               linetype = 'dashed', size = 1.1) + 
    ggtitle('Grades - Programming Exam 2023' + 
    xlab('Grade') + 
    ylab('Count') +
    theme_bw()
  p14a
  
  
  
  #' --------------------------------------------------------------┐ 
  #' IMPROVING Q3.1.2:                                             |
  #' --------------------------------------------------------------┤
  #' I recreated the plot in ggplot2 for nicer visuals. I added    |
  #' informative labels and a title. I also created a custom theme.|
  #' Lastly, I included information about a third variable by      |
  #' varying the color of the data points to display minimum       |
  #' temperature.                                                  |
  #' --------------------------------------------------------------┘
  
  # Read data
  data14b <- read.csv('schiphol_data.csv')
  df14b <- data.frame(data2)
  
  # Make custom theme
  theme14b <- theme(plot.title = element_text(face = 'bold', size = 14),
                    axis.title.x = element_text(face = 'bold', size = 12),
                    axis.title.y = element_text(face = 'bold', size = 12))
  
  # Create plot
  p14b <- ggplot(df14b, aes(x = DATE, y = TMAX)) + 
    geom_point(aes(color = TMIN), size = 2) +
    ggtitle('Schiphol Temperature (Celsius) per Year') + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    xlab('Year') + 
    ylab('Maximum Temperature') +
    theme14b
  p14b
  
  
  
  #################################################################
  ##                           Q3.1.15                           ##
  #################################################################
  
  # Original code
  a <- 'dog'
  b = 'cat'
  v = function(x, y){
    xx <- strsplit(x, '', )[[1]]
    yy <- strsplit(y, '', )[[1]]
    lxx = length(xx)
    lyy = length(yy)
    v <- lxx == lyy
    if(v) return(T)
  }
  v(a, b)
  
  # Adding stuff to make sense of the function
  a <- 'dog'
  b = 'cat'
  v = function(x, y){
    xx <- strsplit(x, '', )[[1]]
    print(xx) # Splits 'x' string into separate letters
    
    yy <- strsplit(y, '', )[[1]]
    print(yy) # Splits 'y' string into separate letters
    
    lxx = length(xx)
    print(lxx) # Amount of letters in input string
    
    lyy = length(yy)
    print(lyy) # Amount of letters in input string
    
    v <- lxx == lyy
    print(v) # Boolean, TRUE if both input strings are of equal length
    
    if(v) return(T) # Return TRUE if 'v' is TRUE
  }
  v(a, b)
  
  
  
  #' --------------------------------------------------------------┐ 
  #' ANSWER:                                                       |
  #' --------------------------------------------------------------┤
  #' The code defines a function that checks whether two strings
  #' contain the same number of letters and returns 
  #' 
  #' 1. The function's name is not informative. We should name it  |
  #' such that it is clear what purpose the function serves.       |
  #'                                                               |
  #' 2. The variable names inside the function are also            |
  #' uninformative. These too should be named such that it is      |
  #' clear what values are stored in them.                         |
  #'                                                               |
  #' 3. The logical comparison and if-statement towards the end    |
  #' are redundant and we can make the code shorter and more       |
  #' readable by omitting the 'v' variable.                        |
  #'                                                               |
  #' 4. There are no comments indicating what's going on inside    |
  #' of the function.                                              |
  #'                                                               |
  #' 5. Inconsistent use of assignment operators.                  |
  #'                                                               |
  #' --------------------------------------------------------------┘
  
  
  # Improved version (with some additional fixes):
  
  # Function that checks whether two words contain same amount of letters
  equalWordLength <- function(word1 = 'slay', word2 = 'king') {
    # Check if inputs are strings
    if (!is.character(word1)) {
      stop('argument 'word1' must be of class character') 
    } else if (!is.character(word2)) {
      stop('argument 'word2' must be of class character')
      
      # If inputs are strings, following code is executed
    } else {
      # Split input strings into separate letters
      letters1 <- strsplit(word1, split = '')[[1]]
      letters2 <- strsplit(word2, split = '')[[1]]
      
      # Store number of letters per word
      n_letters1 <- length(letters1)
      n_letters2 <- length(letters2)
      
      # Return TRUE if words are same length
      if (n_letters1 == n_letters2) {
        return(TRUE)
        # Return FALSE if words are different length
      } else {
        return(FALSE)
      }
    }
  }
  
  # Test function
  equalWordLength()                   # Should return TRUE
  equalWordLength('cat', 'dog')       # Should return TRUE
  equalWordLength('carsick', 'bongo') # Should return FALSE
  equalWordLength(2, 'doobie')        # Should produce error for word1
  equalWordLength('brothers', 5)      # Should produce error for word2
  
  
  
  #################################################################
  ##                           Q3.1.16                           ##
  #################################################################
  
  # The correct answer
  rep(1:3) * matrix(1:9, nrow = 3, byrow = TRUE)
  
  # The weird this I came up with earlier and don't wanna get rid off
  matrix(c(c(1:3), c(7:12)[c(F,T)], c(21:27)[c(T,F,F)]), 
         nrow = 3, ncol = 3, byrow = TRUE)
  
  
  
  #################################################################
  ##                           Q3.1.17                           ##
  #################################################################
  
  #' --------------------------------------------------------------┐ 
  #' ANSWER:                                                       |
  #' --------------------------------------------------------------┤
  #'  Ctrl + Shift + A formats code to make it look neat.          |
  #'  Similarly, Ctrl + I fixes only indenting.                    | 
  #' --------------------------------------------------------------┘
  
  
  
  #################################################################
  ##                           Q3.1.18                           ##
  #################################################################
  
  meme_get('ExpandingBrain') %>%
    meme_text_brain('Making memes in paint',
                    'Making memes in photoshop',
                    'Making memes in R',
                    'Making memes in assembly')
  
  
  
  "
  cat(text)
}


cheat_assignment3()
