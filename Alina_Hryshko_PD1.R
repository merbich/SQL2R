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
  # JOIN Posts and Users
  x1 <- merge(Users, Posts, by.x = "Id", by.y = "OwnerUserId")
  # rows WHERE Location NOT IN ''
  x1 <- x1[x1$Location != "", ]
  # GROUP BY Location
  x1 <- aggregate(x = x1$Location, by = x1["Location"], FUN = length)
  # Set name for column
  colnames(x1)[2] <- "Count"
  # Sort by Count column
  x1 <- x1[order(x1$Count, decreasing = TRUE), ]
  rownames(x1) <- NULL
  # LIMIT 10
  x1 <- head(x1, 10)
  x1
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
  # GROUP BY RelatedPostId
  RelatedTab <- aggregate(x = PostLinks$RelatedPostId, 
                          by = PostLinks["RelatedPostId"], FUN = length)
  # SELECT RelatedPostId AS PostId, COUNT(*) AS NumLinks
  colnames(RelatedTab) <- c("PostId", "NumLinks")
  # WHERE Posts.PostTypeId=1
  ChoosenPosts <- Posts[Posts$PostTypeId == 1, ]
  # JOIN Posts ON RelatedTab.PostId=Posts.Id
  x <- merge(ChoosenPosts, RelatedTab, by.x = "Id", by.y = "PostId")
  # ORDER BY NumLinks DESC
  x <- x[order(x$NumLinks, decreasing = TRUE), ]
  # SELECT Posts.Title, RelatedTab.NumLinks
  x <- x[, c("Title", "NumLinks")]
  rownames(x) <- NULL
  x
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
  # GROUP BY PostId
  CmtTotScr <- aggregate(x = Comments$Score, by = Comments["PostId"], FUN = sum)
  # SELECT PostId, SUM(Score) AS CommentsTotalScore
  colnames(CmtTotScr) <- c("PostId", "CommentsTotalScore")
  # WHERE Posts.PostTypeId=1
  ChoosenPosts <- Posts[Posts$PostTypeId == 1, ]
  # JOIN Posts ON Posts.Id = CmtTotScr.PostId
  PostBestComments <- merge(
                            CmtTotScr, ChoosenPosts, 
                            by.x = "PostId", by.y = "Id")
  # SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount,
  # Posts.ViewCount, CmtTotScr.CommentsTotalScore
  PostBestComments <- PostBestComments[,
                                       c("OwnerUserId", "Title",
                                         "CommentCount", "ViewCount",
                                         "CommentsTotalScore")]
  # JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
  x <- merge(Users, PostBestComments, by.x = "Id", by.y = "OwnerUserId")
  # ORDER BY CommentsTotalScore DESC
  x <- x[order(x$CommentsTotalScore, decreasing = TRUE), ]
  # SELECT Title, CommentCount, ViewCount, CommentsTotalScore,
  # DisplayName, Reputation, Location
  x <- x[,
         c("Title", "CommentCount",
           "ViewCount", "CommentsTotalScore",
           "DisplayName", "Reputation", "Location")]
  rownames(x) <- NULL
  # LIMIT 10
  head(x, 10)
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
  # WHERE PostTypeId = 1
  ChoosenPosts <- Posts[Posts$PostTypeId == 1, ]
  # GROUP BY OwnerUserId
  Questions <- aggregate(
                        x = ChoosenPosts$OwnerUserId,
                        by = ChoosenPosts["OwnerUserId"],
                        FUN = length)
  # SELECT COUNT(*) as QuestionsNumber, OwnerUserId
  colnames(Questions) <- c("OwnerUserId", "QuestionsNumber")

  # WHERE PostTypeId = 2
  ChoosenPosts2 <- Posts[Posts$PostTypeId == 2, ]
  # GROUP BY OwnerUserId
  Answers <- aggregate(
                       x = ChoosenPosts2$OwnerUserId,
                       by = ChoosenPosts2["OwnerUserId"],
                       FUN = length)
  # SELECT COUNT(*) as AnswersNumber, OwnerUserId
  colnames(Answers) <- c("OwnerUserId", "AnswersNumber")

  # JOIN ON Answers.OwnerUserId = Questions.OwnerUserId
  PostCounts <- merge(
                      Answers, Questions,
                      by.x = "OwnerUserId",
                      by.y = "OwnerUserId")
  # WHERE AnswersNumber > QuestionsNumber
  PostCounts <- PostCounts[
                           PostCounts$AnswersNumber
                           >
                             PostCounts$QuestionsNumber, ]
  # ORDER BY AnswersNumber DESC
  PostCounts <- PostCounts[order(PostCounts$AnswersNumber, decreasing = TRUE), ]
  # LIMIT 5
  PostCounts <- head(PostCounts, 5)

  # JOIN Users ON PostsCounts.OwnerUserId = Users.Id
  x <- merge(PostCounts, Users, by.x = "OwnerUserId", by.y = "Id")
  # ORDER BY AnswersNumber DESC (again after JOIN)
  x <- x[order(x$AnswersNumber, decreasing = TRUE), ]
  # SELECT DisplayName, QuestionsNumber, AnswersNumber, Location,
  # Reputation, UpVotes, DownVotes
  x <- x[,
         c("DisplayName", "QuestionsNumber",
           "AnswersNumber", "Location",
           "Reputation", "UpVotes", "DownVotes")]
  rownames(x) <- NULL
  x
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
  # WHERE Posts.PostTypeId = 2
  ChoosenPosts <- Posts[Posts$PostTypeId == 2, ]
  # GROUP BY Posts.ParentId
  AnsCount <- aggregate(
                        x = ChoosenPosts$ParentId,
                        by = ChoosenPosts["ParentId"],
                        FUN = length)
  # SELECT Posts.ParentId, COUNT(*) AS AnswersCount
  colnames(AnsCount) <- c("ParentId", "AnswersCount")

  # JOIN Posts ON Posts.Id = AnsCount.ParentId
  PostAuth <- merge(Posts, AnsCount, by.x = "Id", by.y = "ParentId")
  # SELECT AnsCount.AnswersCount, Posts.Id, Posts.OwnerUserId
  PostAuth <- PostAuth[, c("AnswersCount", "Id", "OwnerUserId")]
  # GROUP BY OwnerUserId (AVG of AnswersCount)
  PostAuth <- aggregate(
                        x = PostAuth$AnswersCount,
                        by = PostAuth["OwnerUserId"],
                        FUN = mean)

  # JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
  x1 <- merge(PostAuth, Users, by.x = "OwnerUserId", by.y = "AccountId")
  # SELECT Users.AccountId, Users.DisplayName, Users.Location,
  # AVG(PostAuth.AnswersCount) as AverageAnswersCount
  x1 <- x1[, c("OwnerUserId", "DisplayName", "Location", "x")]
  colnames(x1) <- c(
                    "AccountId", "DisplayName",
                    "Location", "AverageAnswersCount")
  # ORDER BY AverageAnswersCount DESC
  x1 <- x1[order(x1$AverageAnswersCount, decreasing = TRUE), ]
  rownames(x1) <- NULL
  # LIMIT 10
  head(x1, 10)
}

dplyr_5 <- function(Posts, Users) {
  # Input the solution here
  #
}

data.table_5 <- function(Posts, Users) {
  # Input the solution here
  #
}
