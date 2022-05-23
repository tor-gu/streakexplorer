# What is this?
The *Baseball Streak Explorer* is a way to explore and visualize hot and cold streaks of Major League Baseball teams, from 1948 to 2021.

## The basic idea -- how streaks are compared
How do you compare a brief but intense 13-2 hot streak against a 39-16 run of sustained excellence?

Compared to a winning percentage of $.600$, the longer 39-16 streak is more impressive. On the other hand, compared to a a winning percentage of $.700$, the shorter streak in more impressive:


| Actual Record | $.600$ Pace |Excess Wins vs $.600$|$.700$ Pace|Excess Wins vs $.700$|
|:-----------:|:-----------:|:----------|:-----------:|:---------:|
|39-16        |33 - 22      |6          |38.5 - 16.5  |.5         |
|13-2         |9-6          |4          |10.5 - 4.5   |2.5        |


In this application, we measure streaks against varying winning percentage from $0$ to $1$ -- represented by the $x$-axis -- and rank the streaks on the $y$-axis. At the right-side of the graph, shorter, more intense streaks are most highly ranked, while at the left-side of the graph, full seasons dominate.

The scoring formula uses a parameter $p$, which we call the streak *intensity*. The value of $p$ ranges from $0$ to $1$, and the streak score is:
$$ Score_p = W + T/2 - (W+L+T)p $$
where $W$, $L$ and $T$ are the number of wins, losses and ties, respectively.  The score measures how much better (or worse) a team is doing compared to a team with a winning percentage of $p$ by counting the number of excess wins, as in the table above. 

So, for example, at $p = .5$, the score is proportional to what is normally called "games above $.500$". As $p$ tends to $1$, pure winning streaks dominate, and at $p = 0$, the score becomes just the total number of wins (plus half the number of ties), so full seasons score the highest.

For cold streaks, the $x$-axis is reversed, since the lowest scoring streaks at $p=1$ are full seasons, while pure losing streaks get the lowest scores as $p$ tends to $0$.

## Which streaks are ranked?
Not every streak in ranked at every intensity. 

For hot streaks, at each intensity level, a streak is given a ranking only if it its *score is maximal among all sub- and super-streaks*.  For example, at intensity  $p=.55$, the highest ranked streak is Cleveland's 1954 run from games 10 to 151, when they went 106-34-2.  This streak is included in a 108-36-2 super-streak (which scores higher at intensities $p < .5$) and a 53-12-2 sub-streak (which scores higher at intensities $p > .6875$). But at $p=0.55$, the 106-34-2 streak has the highest score and is the only one ranked.  

Similarly, for cold streaks, at each intensity level, a streak is given a ranking only if it is *minimal* among all sub- and super-streaks.

#### Streak Splitting
As you follow a streak line to to the left, the streak may be replaced by a longer, less intense super-streak. Conversely, as you follow a line to the right, the streak may be replaced by a shorter, more intense sub-streak.  In some cases, as you move to the right, a streak may be replaced by *two or more* non-overlapping sub-streaks, each more intense than the super-streak. In this case, the line will split.

## How are double-headers handled?
In general, mid-season standings are calculated based on the results at the end of the day. ("The end of the day" does not mean midnight. It means when all of the day's games are completed or suspended.) However, when a streak starts or ends in the middle of a double-header, the calculation is slightly more complicated.

When a streak *starts* on the *second* game of a double-header, the "Standings Before" is the standings at the start of the day, plus the results of the first game of the double-header (for both the team and its opponent). When a streak *ends* after the *first* game of a double-header, the "Standings After" is the standings at the start of the day, plus the results of the first game.

## How are incomplete games handled?
For the purpose of calculating streak wins and losses (and scores), it is the *completion* of the game that counts.  

For the purpose of calculating runs-scored and runs-allowed during a streak, the  runs scored during partial games are included in the totals if they fall within the bounds of the streak.

#### Example
Suppose a team plays 8 games, including one suspended and completed game, as follows:

|Index|Game|Runs Scored|Runs Allowed|Result|Comment
|:---:|:--:|:---------:|:----------:|:----:|:-----:|
|1    |1   |4          |5           |Loss  |       |          
|2    |2   |3          |2           |Win   |       |          
|3    |3   |4          |2           |      |Game suspended in the 9th |
|4    |4   |4          |3           |Win   |       |          
|5    |5   |1          |5           |Loss  |       |          
|6    |6   |3          |0           |Win   |       |          
|7    |3   |0          |1           |Win   |Completion of index 3, final score 4-3 |
|8    |7   |4          |1           |Win   |       |          
|9    |8   |2          |5           |Loss  |       |          

From index 2 to 4, the team has a *two* game winning streak -- the suspended game at index 3 does not count, even though the 
team eventually won the game.  From index  6 to 8, the team has a *three* game winning streak -- the completion of the earlier game counts towards the streak. However, the runs scored at index 3 are incorporated in first streak, while only the runs scored at index 7 are included in the second streak.  If we also include the longer hot-streak starting at index 2 and ending at index 8, we have these three streaks:

|Streak|Starting Index|Ending Index|Record|Runs Scored|Runs Allowed|
|:----:|:------------:|:----------:|:----:|:---------:|:----------:|
|1     |2             |4           |2-0   |11         |7           |
|2     |6             |6           |3-0   |7          |2           |
|3     |2             |8           |5-1   |19         |14          |


## How are ties handled?
Ties count as one half of a win when computing winning percentages and displaying standings. 

## What is the scale of the $y$-axis in the graph?
The scale of the $y$ axis is the streak *rank*, among streaks from 1948 to 2021 at the same intensity level.  For hot streaks, the streaks are ranked by highest score, and for cold streaks, by lowest score.

Note that, when ranking streaks at a given intensity, sub- and super-streaks with lower scores are excluded. So, for example, at $p=.55$, the highest scoring streak is 1954 Cleveland's 106-34-2. The next highest scoring streak is a super-streak of this streak, and so is not ranked (at $p=.55$). After that is a 115-44 run by the 2001 Mariners, so that is the streak that is ranked second at $p=.55$.

|Season        |Record  |Score ($p=.55$)|Rank|Comment|
|:------------:|:------:|:---------:|:--:|:-----:|
|1954 Cleveland|106-34-2|$29.9$     |1   ||
|1954 Cleveland|108-36-2|$28.7$     |    |Super-streak|
|2001 Seattle  |115-44  |$27.55$    |2   ||

## What is the scale of the $x$-axis in the graph?
The $x$-axis is a non-linear function of the intensity $p$. We use the [$\beta$ distribution](https://en.wikipedia.org/wiki/Beta_distribution) to move the more interesting intensities -- for hot streaks, around $p=.6$ and for cold streaks around, $p=.4$ towards the center.

More precisely, for hot streaks, the $x$ value and the intensity $p$ are related by$$x=I_p(7,5)$$ where $I_p(7,5)$ is the cumulative distribution function for the $\beta$ distribution, $\beta(7,5)$, which has a mode at $p=.6$. 

For cold streaks, the $x$ value and intensity are related by $$x=1-I_p(11/3,5)$$ where $I_p(11/3, 5)$ is the cumulative distribution function for the $\beta$ distribution, $\beta(11/3,5)$, which has a mode at $p=.4$. 


# How is this site made?
This is an [R/Shiny](https://shiny.rstudio.com/) application. The source code can be found [here](https://github.com/tor-gu/streakexplorer).

The main plot is rendered with [plotly](https://plotly.com/r/), and the tables are [DataTables](https://datatables.net/), using the [DT](https://rstudio.github.io/DT/) package for R.

The baseball data is from [Retrosheet](https://www.retrosheet.org/), as packaged by the [R retrosheet package](https://cran.r-project.org/package=retrosheet).


