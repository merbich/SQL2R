### Przetwarzanie danych w językach R i Python 2024L
### Praca domowa nr. 1 / Homework Assignment no. 1
###
### WAŻNE
### Ten plik powinien zawierać tylko rozwiązania zadań w postaci
### definicji funkcji, załączenia niezbędnych bibliotek
### i komentarzy do kodu.
###
### Raport powinien zawierać:
### * source() tego pliku,
### * odczytanie danych,
### * dołączenie bibliotek,
### * pomiary czasu wykonania (z mikrobenchmarkiem),
### * porównanie równoważności wyników,
### * interpretację zapytań.
install.packages(c("sqldf", "dplyr", "data.table", "compare"))


library(dplyr)
library(sqldf)

# -----------------------------------------------------------------------------#
# Task 1
# -----------------------------------------------------------------------------#

sqldf_1 <- function(Posts, Users) {
  # Input the solution here
  #
  sqldf("
        SELECT Location, COUNT(*) AS Count
        FROM (
          SELECT Posts.OwnerUserId, Users.Id, Users.Location
          FROM Users
          JOIN Posts ON Users.Id = Posts.OwnerUserId
        )
        WHERE Location NOT IN ('')
        GROUP BY Location
        ORDER BY Count DESC
        LIMIT 10
        ")
}

base_1 <- function(Posts, Users) {
  # Input the solution here
  #
}

dplyr_1 <- function(Posts, Users) {
  # Input the solution here
  #
}

data.table_1 <- function(Posts, Users) {
  # Input the solution here
  #
}

# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

sqldf_2 <- function(Posts, PostLinks) {
  # Input the solution here
  #
  sqldf("
      SELECT Posts.Title, RelatedTab.NumLinks
      FROM
      (
        SELECT RelatedPostId AS PostId, COUNT(*) AS NumLinks
        FROM PostLinks
        GROUP BY RelatedPostId
      ) AS RelatedTab
      JOIN Posts ON RelatedTab.PostId=Posts.Id
      WHERE Posts.PostTypeId=1
      ORDER BY NumLinks DESC
      ")
}

base_2 <- function(Posts, PostLinks) {
  # Input the solution here
  #
}

dplyr_2 <- function(Posts, PostLinks) {
  # Input the solution here
  #
}

data.table_2 <- function(Posts, PostLinks) {
  # Input the solution here
  #
}

# -----------------------------------------------------------------------------#
# Task 3
# -----------------------------------------------------------------------------#

sqldf_3 <- function(Comments, Posts, Users) {
  # Input the solution here
  #
  sqldf("
      SELECT Title, CommentCount, ViewCount, CommentsTotalScore,
        DisplayName, Reputation, Location
      FROM (
        SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount,
          Posts.ViewCount, CmtTotScr.CommentsTotalScore
        FROM (
          SELECT PostId, SUM(Score) AS CommentsTotalScore
            FROM Comments
          GROUP BY PostId
        ) AS CmtTotScr
        JOIN Posts ON Posts.Id = CmtTotScr.PostId
          WHERE Posts.PostTypeId=1
      ) AS PostsBestComments
      JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
      ORDER BY CommentsTotalScore DESC
      LIMIT 10
      ")
}

base_3 <- function(Comments, Posts, Users) {
  # Input the solution here
  #
}

dplyr_3 <- function(Comments, Posts, Users) {
  # Input the solution here
  #
}

data.table_3 <- function(Comments, Posts, Users) {
  # Input the solution here
  #
}

# -----------------------------------------------------------------------------#
# Task 4
# -----------------------------------------------------------------------------#

sqldf_4 <- function(Posts, Users) {
  # Input the solution here
  #
  sqldf("
      SELECT DisplayName, QuestionsNumber, AnswersNumber, Location,
      Reputation, UpVotes, DownVotes
      FROM (
        SELECT *
        FROM (
          SELECT COUNT(*) as AnswersNumber, OwnerUserId
          FROM Posts
          WHERE PostTypeId = 2
          GROUP BY OwnerUserId
        ) AS Answers
        JOIN
        (
          SELECT COUNT(*) as QuestionsNumber, OwnerUserId
          FROM Posts
          WHERE PostTypeId = 1
          GROUP BY OwnerUserId
        ) AS Questions
        ON Answers.OwnerUserId = Questions.OwnerUserId
        WHERE AnswersNumber > QuestionsNumber
        ORDER BY AnswersNumber DESC
        LIMIT 5
      ) AS PostsCounts
      JOIN Users
      ON PostsCounts.OwnerUserId = Users.Id
      ")
}

base_4 <- function(Posts, Users) {
  # Input the solution here
  #
}

dplyr_4 <- function(Posts, Users) {
  # Input the solution here
  #
}

data.table_4 <- function(Posts, Users) {
  # Input the solution here
  #
}

# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

sqldf_5 <- function(Posts, Users) {
  # Input the solution here
  #
  sqldf("
      SELECT
        Users.AccountId,
        Users.DisplayName,
        Users.Location,
        AVG(PostAuth.AnswersCount) as AverageAnswersCount
      FROM
      (
        SELECT
          AnsCount.AnswersCount,
          Posts.Id,
          Posts.OwnerUserId
        FROM (
          SELECT Posts.ParentId, COUNT(*) AS AnswersCount
          FROM Posts
          WHERE Posts.PostTypeId = 2
          GROUP BY Posts.ParentId
        ) AS AnsCount
        JOIN Posts ON Posts.Id = AnsCount.ParentId
      ) AS PostAuth
      JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
      GROUP BY OwnerUserId
      ORDER BY AverageAnswersCount DESC
      LIMIT 10
      ")
}

base_5 <- function(Posts, Users) {
  # Input the solution here
  #
}

dplyr_5 <- function(Posts, Users) {
  # Input the solution here
  #
}

data.table_5 <- function(Posts, Users) {
  # Input the solution here
  #
}
