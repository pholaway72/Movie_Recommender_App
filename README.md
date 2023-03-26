# Movie Recommender RShiny App

## [App Link](https://pholaway72.shinyapps.io/projectapp/)

The purpose of this project was to develop a movie recommender app using RShiny. The [data](https://github.com/pholaway72/Movie_Recommender_App/tree/main/data) used for this app contains movie reviews for 3,883 movies from 6,040 uniqure MovieLen users who joined MovieLen in 2000. There are two different methods implemented in this app for users to get movie recommendations.

1. Genre Input
2. User-Based Collaborative Filtering (UBCF)

---

## Recommender Systems

### 1. Genre Input

For this method movies with the top 10 highest average ratings are returned within each genre. There were two criteria used in this method. The first was that a movie must be within the top 25 most reviewed movies. The thought process was that the more reviews a movie had, the more watched it was, which means it would be more popular among viewers. The second was that the movie has to have a top 10 highest average rating from that subset of 25 most reviewed. The cleaned data the app used for this method can be found [here](https://github.com/pholaway72/Movie_Recommender_App/blob/main/ProjectApp/GenreRecommend.txt).

### 2. UBCF

This method returns movie recommendations by taking in a user's input and comparing it other user's input and returning movies that are similar in the reviews. This method is implemented in the app using the `recommenderlab` package.

Further details about the process for these methods can be found in the [`.Rmd` file](https://github.com/pholaway72/Movie_Recommender_App/blob/main/project4.Rmd) or by downloading the [HTML Documentation](https://github.com/pholaway72/Movie_Recommender_App/blob/main/project4.html).

---

### How To Use The App

**Genre Input:** Select your genre of interest, and then app will return the top 10 highest rated movies from that genre using the methodology outlined above.

**Rating Input:** Rank the movies presented to you that you have seen from 1-5 stars, and our app will return movies based on your recommendations using the UBCF method.
