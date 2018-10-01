# mls-elo
Calculating Elo ratings for MLS teams

## scraping

Must have selenium-server in the working directory and running in order to scrape the data. You can download selenium server from [here](http://selenium-release.storage.googleapis.com/index.html). 

Not that the scraping code uses port 5556 and the phantomjs browser (which must also be installed). Phantomjs can be downloaded the `webdriver` packages(`webdriver::install_phantomjs()`).