# WebDataAnalytics

Presentation Link: https://youtu.be/GI-2u84VgIs

In this project, we set out to understand the impact of broadband adoption vs. broadband access on school performance for public schools in Indiana. The team grabbed public access csvs on school grade performance in Indiana as well as data on the technology each school utilized, specifically focused on 1-to-1 tech programs. Then, we scraped the counties and cities from the census website as well as the percent of broadband access in each city or county utilizing BeautifulSoup in Python. From there, we built a linear regression in R to predict the school grade (from 0 - 100). We also utilized sentiment analysis to get a proxy for enthusiasm for utilizing technology in the classroom. Overall, we were able to achieve an R-squared of over 30%. 

Things I would do differently
1. Calculate the sentiment scores to take into account the length of the text and not just the amount of positive and negative-toned words. 
