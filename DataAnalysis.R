#Aim is to perform data analysis on movie ratings by critics and audience as well as movie budgets for the years 2007-2011 for movie rating consultants for an article on movie reviews
colnames(MovieRatings)
[1] "Film"                      "Genre"                     "Rotten Tomatoes Ratings %" "Audience Ratings %"       
[5] "Budget (million $)"        "Year of release" 
getwd() #getting woring directory
[1] "C:/Users/54321/OneDrive/Documents"
setwd("C:/Users/54321/OneDrive/Documents") #setting working directory to load the data
colnames(MovieRatings) <-c("Film","Genre","CriticRating","AudienceRating","BudgetMillions","Year") # changing the column names as suitable
head(MovieRatings) # starting 6 data values
tail(MovieRatings) #last 6 data values
str(MovieRatings) # structure of the data set
#===========================================output of str======================
#'data.frame':	562 obs. of  6 variables:
# $ Film          : Factor w/ 562 levels "(500) Days of Summer ",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Genre         : Factor w/ 7 levels "Action","Adventure",..: 3 2 1 2 3 1 3 5 3 3 ...
# $ CriticRating  : int  87 9 30 93 55 39 40 50 43 93 ...
# $ AudienceRating: int  81 44 52 84 70 63 71 57 48 93 ...
# $ BudgetMillions: int  8 105 20 18 20 200 30 32 28 8 ...
# $ Year          : Factor w/ 5 levels "2007","2008",..: 3 2 3 4 3 3 2 1 5 5 ...
#==============================================================================
summary(MovieRatings) #total summary of the data
#=========================================summary output========================
#Film           Genre      CriticRating  AudienceRating  BudgetMillions       Year     
# (500) Days of Summer :  1   Action   :154   Min.   : 0.0   Min.   : 0.00   Min.   :  0.0   Min.   :2007  
# 10,000 B.C.          :  1   Adventure: 29   1st Qu.:25.0   1st Qu.:47.00   1st Qu.: 20.0   1st Qu.:2008  
# 12 Rounds            :  1   Comedy   :172   Median :46.0   Median :58.00   Median : 35.0   Median :2009  
# 127 Hours            :  1   Drama    :101   Mean   :47.4   Mean   :58.83   Mean   : 50.1   Mean   :2009  
# 17 Again             :  1   Horror   : 49   3rd Qu.:70.0   3rd Qu.:72.00   3rd Qu.: 65.0   3rd Qu.:2010  
# 2012                 :  1   Romance  : 21   Max.   :97.0   Max.   :96.00   Max.   :300.0   Max.   :2011  
# (Other)              :556   Thriller : 36 
#===============================================================================     
#as year is considered as numeric variable , we need to convert it as a categorical variable for evaluation
MovieRatings$Year<-factor(MovieRatings$Year)
summary(MovieRatings)
#    Film              Genre            CriticRating  AudienceRating  BudgetMillions    Year    
#Length:562         Length:562         Min.   : 0.0   Min.   : 0.00   Min.   :  0.0   2007: 79  
#Class :character   Class :character   1st Qu.:25.0   1st Qu.:47.00   1st Qu.: 20.0   2008:125  
#Mode  :character   Mode  :character   Median :46.0   Median :58.00   Median : 35.0   2009:116  
#                                      Mean   :47.4   Mean   :58.83   Mean   : 50.1   2010:119  
#                                      3rd Qu.:70.0   3rd Qu.:72.00   3rd Qu.: 65.0   2011:123  
#                                      Max.   :97.0   Max.   :96.00   Max.   :300.0 
#==============================================================================

#Scatter plot for audience v/s critic rating for every year
plots <-ggplot(data = MovieRatings,aes(x = CriticRating,y=AudienceRating,color = Year))
plots + geom_point() + geom_smooth(fill = NA) + ggtitle("Scatter plot for Critic Rating V/S Audience rating")+ 
  xlab("Critic Ratings")+
  ylab("Audience Ratings")+
  theme(axis.title.x = element_text(color="red",size = 35),
        axis.title.y = element_text(color="pink",size = 35),
        legend.title = element_text(color="pink",size = 15))

# please refer to ScatterPlotForYear.png from the uploaded file.from the plot we can see how both the ratings are interrelated for a pariticular Year

#==============================================================
#Scatter plot for audience v/s critic rating for every Genre
plots <-ggplot(data = MovieRatings,aes(x = CriticRating,y=AudienceRating,color = Genre))
plots + geom_point() + geom_smooth(fill = NA) + ggtitle("Scatter plot for Critic Rating V/S Audience rating")+ 
  xlab("Critic Ratings")+
  ylab("Audience Ratings")+
  theme(axis.title.x = element_text(color="green",size = 30),
        axis.title.y = element_text(color="brown",size = 30),
        legend.title = element_text(color="brown",size = 15))
# please refer to ScatterPlotGenre.png from the uploaded file.from the plot we ca see that for the Romance Genre, there is a sudden spike at the start

#===============================================Scatter plot for audience v/s critic for all year and Genre(plot 10)==============================================
plots <-ggplot(data = MovieRatings,aes(x = CriticRating,y=AudienceRating,color = Year))
plots + geom_point(size = 1)+
  geom_smooth()+
  facet_grid(Genre~Year)+
  coord_cartesian(ylim = c(0,100))+
xlab("Critic Ratings")+
  ylab("Audience Ratings")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="black",size = 20),
        legend.title = element_text(color="black",size = 12))

#please refer to ScatterPlotForGenre.png. in this image the each genre for each year is displayed for better viewing
#===============================================Box ploting=====================
#box plot for audience rating for each Genre
plots2 <-ggplot(data = MovieRatings)
plots2 +  geom_jitter(aes(x=Genre,y=AudienceRating,color=Genre))+geom_boxplot(aes(x = Genre,y=AudienceRating,color = Genre),size = 1.2,,alpha = 0.5)+ 
ggtitle("Box plot for Audience rating for each Genre")+ 
  xlab("Genre")+
  ylab("Audience Ratings")+
  theme(axis.title.x = element_text(color="red",size = 20),
        axis.title.y = element_text(color="purple",size = 20),
        legend.title = element_text(color="purple",size = 15))
#please refer to AudienceRatingBoxplot.png. the box plots are one of the best method for visualization. the line in the boox plot are considerd as mean of that 

##box plot for critic ratings for each Genre    
plots2 +  geom_jitter(aes(x=Genre,y=CriticRating,color=Genre))+geom_boxplot(aes(x = Genre,y=CriticRating,color = Genre),size = 1.2,,alpha = 0.5)+ 
ggtitle("Box plot for critic ratings for each Genre")+ 
  xlab("Genre")+
  ylab("Critic Ratings")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="brown",size = 20),
        legend.title = element_text(color="brown",size = 15))
#please refer to CriticRatingBoxplot, just as similar to the audience ratings .the critic ratings are also high for action and also low ratings for the Horror genre.

#=============================================Histogram plots============================

#Histogram plot for Audience ratings 
plots3 <-ggplot(data = MovieRatings,aes(x= AudienceRating))
plots3 + geom_histogram(binwidth = 10,color = "black",fill = "pink")+
  ggtitle("Histogram for Audience ratings v/s number of movies")+ 
    xlab("Audience Ratings")+
  ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="black",size = 20),
        plot.title = element_text(color="brown",size = 15))
#Please refer to HistAudience.png.

#Histogram plot for Critic ratings
plots4 <-ggplot(data = MovieRatings,aes(x= CriticRating))
plots4 + geom_histogram(binwidth = 10,color = "black",fill = "green")+
  ggtitle("Histogram for Critic ratings v/s number of movies")+ 
  xlab("Critic Ratings")+
  ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="black",size = 20),
        plot.title = element_text(color="brown",size = 15))
# Please refer to HistCritics.png.

# histogram plots for budget for each genre
plots1 <-ggplot(data = MovieRatings,aes(x= BudgetMillions))
plots1 + geom_histogram(binwidth = 10,aes(fill = Genre),color = "black")+
facet_grid(Genre~.,scales = "free")+
  ggtitle("Histogram for budget in millions of each genre")+ 
  xlab("Budget in Millions")+
ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="black",size = 20),
        plot.title = element_text(color="brown",size = 15))
#Please refer to HistGenre.png.

# histogram plots for budget for each Year
plots1 <-ggplot(data = MovieRatings,aes(x= BudgetMillions))
plots1 + geom_histogram(binwidth = 10,aes(fill = Year),color = "black")+
facet_grid(Year~.,scales = "free")+
  ggtitle("Histogram for budget in millions of each genre")+ 
  xlab("Budget in Millions")+
ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="black",size = 20),
        plot.title = element_text(color="black",size = 15))
#Please refer to HistYear.png.

#histogram plot for Genre and year together
plots1 <-ggplot(data = MovieRatings,aes(x= BudgetMillions))
plots1 + geom_histogram(binwidth = 10,aes(fill = Year),color = "black")+
facet_grid(Year~Genre,scales = "free")+
  ggtitle("Histogram for budget in millions of each genre")+ 
  xlab("Budget in Millions")+
ylab("Total number of Movies")+
  theme(axis.title.x = element_text(color="black",size = 20),
        axis.title.y = element_text(color="black",size = 20),
        plot.title = element_text(color="brown",size = 15))
#Please refer to HistGenreYear.png.