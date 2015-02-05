###Libraries###
library(XML)
library(plyr)
library(stringr)
library(scrapeR)
library(ggplot2)
library(reshape)
library(lubridate)

tourn <- c("http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=5014&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=5014&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=5014&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2012&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=5014&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2011&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=5014&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2010&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=5014&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2009&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2008&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2007&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2006&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2005&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2004&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2003&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=1536&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2002&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=357&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2001&EventId=352&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=580&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=404&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=403&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=410&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=416&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=414&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=520&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=540&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=421&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=422&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=560&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=357&Draw=ms",
           "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2000&EventId=352&Draw=ms")

###Table of all match stats###
stats.order = NULL
for (j in 1:length(tourn))
{
  #Find all links on the page of the draws#
  h1 <- getLinks()
  htmlTreeParse(file = tourn[j],
                handlers = h1, useInternalNodes = TRUE)
  h1$links()
  
  #Subset the links to only links for the match stats#
  files.1 <- h1$links()[str_detect(h1$links(), "/Share/Match-Facts")]
  #head(files.1)
  #files.1
  
  #Remove the beginning and ending of the javascript links
  files.2 <- substr(files.1, 21, 75)
  #head(files.2)
  
  #Add the beginning of the url to get the full web address
  files.3 <- paste("http://www.atpworldtour.com", files.2, sep="")
  #head(files.3)
  
  #Scrape the website for all the match facts for a given tournament#
  fulltable = NULL
  for (i in 1:length(files.3))
  {
    table <- as.data.frame(readHTMLTable(files.3[i]) [[1]])
    matchtable <- table
    
    matchtable$Hello <- 1
    matchtable <- matchtable[c(3,1,2)]
    matchtable <- t(matchtable)
    matchtable <- matchtable[-1,]
    matchtable <- as.data.frame(matchtable)
    matchtable <- matchtable[-c(1,2,3,4,5,6,7,8,10,11,13,15,16,17,19,21,23,25,27,29,31,32,33,
                                35,37,39,41,42,43,45,47,49)]
    names(matchtable)[1] <- "Result"
    names(matchtable)[2] <- "Player"
    names(matchtable)[3] <- "Nationality_1"
    names(matchtable)[4] <- "Aces"
    names(matchtable)[5] <- "Double_Faults"
    names(matchtable)[6] <- "First_Serve"
    names(matchtable)[7] <- "First_Serve_Points_Won"
    names(matchtable)[8] <- "Second_Serve_Points_Won"
    names(matchtable)[9] <- "Break_Points_Saved"
    names(matchtable)[10] <- "Service_Games_Played"
    names(matchtable)[11] <- "First_Serve_Return_Points_Won"
    names(matchtable)[12] <- "Second_Serve_Return_Points_Won"
    names(matchtable)[13] <- "Break_Points_Converted"
    names(matchtable)[14] <- "Return_Games_Played"
    names(matchtable)[15] <- "Total_Service_Points_Won"
    names(matchtable)[16] <- "Total_Return_Points_Won"
    names(matchtable)[17] <- "Total_Points_Won"
    
    year <- substr(tourn, 57, nchar(tourn))
    year <- substr(year, 1, 4)
    year <- as.numeric(year)
    matchtable$Year <- year[j]
    
    matchtable$Tournament <- table$V1[4]
    matchtable$Round <- table$V1[6]
    matchtable$Time <- table$V1[8]
    
    matchtable$Result <- as.character(matchtable$Result)
    
    if(matchtable$Result[[1]] == "Winner"){
      matchtable$Result[[1]] <- "W"
      matchtable$Result[[2]] <- "L"
    } else{
      matchtable$Result[[1]] <- "L"
      matchtable$Result[[2]] <- "W"
    }
    
    matchtable$MatchID <- i
    
    matchtable$Opponent <- matchtable$Player[[2]]
    matchtable$Opponent[[2]] <- matchtable$Player[[1]]
    
    matchtable$Nationality_2 <- matchtable$Nationality_1[[2]]
    matchtable$Nationality_2[[2]] <- matchtable$Nationality_1[[1]]
    
    matchtable <- matchtable[c(22,18,19,20,21,1,2,3,23,24,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
    
    matchtable$First_Serve_Total <- gsub(".*/", '', matchtable$First_Serve)
    matchtable$First_Serve_Total <- gsub(")", '', matchtable$First_Serve_Total)
    matchtable$First_Serve_Per <- gsub("%.*", '', matchtable$First_Serve)
    
    matchtable$First_Serve_Points_Won_Total <- gsub(".*/", '', matchtable$First_Serve_Points_Won)
    matchtable$First_Serve_Points_Won_Total <- gsub(")", '', matchtable$First_Serve_Points_Won_Total)
    matchtable$First_Serve_Points_Won_Per <- gsub("%.*", '', matchtable$First_Serve_Points_Won)
    
    matchtable$Second_Serve_Points_Won_Total <- gsub(".*/", '', matchtable$Second_Serve_Points_Won)
    matchtable$Second_Serve_Points_Won_Total <- gsub(")", '', matchtable$Second_Serve_Points_Won_Total)
    matchtable$Second_Serve_Points_Won_Per <- gsub("%.*", '', matchtable$Second_Serve_Points_Won)
    
    matchtable$Break_Points_Saved_Total <- gsub(".*/", '', matchtable$Break_Points_Saved)
    matchtable$Break_Points_Saved_Total <- gsub(")", '', matchtable$Break_Points_Saved_Total)
    matchtable$Break_Points_Saved_Per <- gsub("%.*", '', matchtable$Break_Points_Saved)
    
    matchtable$First_Serve_Return_Points_Won_Total <- gsub(".*/", '', matchtable$First_Serve_Return_Points_Won)
    matchtable$First_Serve_Return_Points_Won_Total <- gsub(")", '', matchtable$First_Serve_Return_Points_Won_Total)
    matchtable$First_Serve_Return_Points_Won_Per <- gsub("%.*", '', matchtable$First_Serve_Return_Points_Won)
    
    matchtable$Second_Serve_Return_Points_Won_Total <- gsub(".*/", '', matchtable$Second_Serve_Return_Points_Won)
    matchtable$Second_Serve_Return_Points_Won_Total <- gsub(")", '', matchtable$Second_Serve_Return_Points_Won_Total)
    matchtable$Second_Serve_Return_Points_Won_Per <- gsub("%.*", '', matchtable$Second_Serve_Return_Points_Won)
    
    matchtable$Break_Points_Converted_Total <- gsub(".*/", '', matchtable$Break_Points_Converted)
    matchtable$Break_Points_Converted_Total <- gsub(")", '', matchtable$Break_Points_Converted_Total)
    matchtable$Break_Points_Converted_Per <- gsub("%.*", '', matchtable$Break_Points_Converted)
    
    matchtable$Total_Service_Points_Won_Total <- gsub(".*/", '', matchtable$Total_Service_Points_Won)
    matchtable$Total_Service_Points_Won_Total <- gsub(")", '', matchtable$Total_Service_Points_Won_Total)
    matchtable$Total_Service_Points_Won_Per <- gsub("%.*", '', matchtable$Total_Service_Points_Won)
    
    matchtable$Total_Return_Points_Won_Total <- gsub(".*/", '', matchtable$Total_Return_Points_Won)
    matchtable$Total_Return_Points_Won_Total <- gsub(")", '', matchtable$Total_Return_Points_Won_Total)
    matchtable$Total_Return_Points_Won_Per <- gsub("%.*", '', matchtable$Total_Return_Points_Won)
    
    matchtable$Total_Points_Won_Total <- gsub(".*/", '', matchtable$Total_Points_Won)
    matchtable$Total_Points_Won_Total <- gsub(")", '', matchtable$Total_Points_Won_Total)
    matchtable$Total_Points_Won_Per <- gsub("%.*", '', matchtable$Total_Points_Won)
    
    matchtable$Aces <- as.numeric(as.character(matchtable$Aces))
    matchtable$Double_Faults <- as.numeric(as.character(matchtable$Double_Faults))
    matchtable$Service_Games_Played <- as.numeric(as.character(matchtable$Service_Games_Played))
    matchtable$Return_Games_Played <- as.numeric(as.character(matchtable$Return_Games_Played))
    matchtable$First_Serve_Total <- as.numeric(matchtable$First_Serve_Total)
    matchtable$First_Serve_Per <- as.numeric(matchtable$First_Serve_Per)
    matchtable$First_Serve_Points_Won_Total <- as.numeric(matchtable$First_Serve_Points_Won_Total)
    matchtable$First_Serve_Points_Won_Per <- as.numeric(matchtable$First_Serve_Points_Won_Per)
    matchtable$Second_Serve_Points_Won_Total <- as.numeric(matchtable$Second_Serve_Points_Won_Total)
    matchtable$Second_Serve_Points_Won_Per <- as.numeric(matchtable$Second_Serve_Points_Won_Per)
    matchtable$Break_Points_Saved_Total <- as.numeric(matchtable$Break_Points_Saved_Total)
    matchtable$Break_Points_Saved_Per <- as.numeric(matchtable$Break_Points_Saved_Per)
    matchtable$First_Serve_Return_Points_Won_Total <- as.numeric(matchtable$First_Serve_Return_Points_Won_Total)
    matchtable$First_Serve_Return_Points_Won_Per <- as.numeric(matchtable$First_Serve_Return_Points_Won_Per)
    matchtable$Second_Serve_Return_Points_Won_Total <- as.numeric(matchtable$Second_Serve_Return_Points_Won_Total)
    matchtable$Second_Serve_Return_Points_Won_Per <- as.numeric(matchtable$Second_Serve_Return_Points_Won_Per)
    matchtable$Break_Points_Converted_Total <- as.numeric(matchtable$Break_Points_Converted_Total)
    matchtable$Break_Points_Converted_Per <- as.numeric(matchtable$Break_Points_Converted_Per)
    matchtable$Total_Service_Points_Won_Total <- as.numeric(matchtable$Total_Service_Points_Won_Total)
    matchtable$Total_Service_Points_Won_Per <- as.numeric(matchtable$Total_Service_Points_Won_Per)
    matchtable$Total_Return_Points_Won_Total <- as.numeric(matchtable$Total_Return_Points_Won_Total)
    matchtable$Total_Return_Points_Won_Per <- as.numeric(matchtable$Total_Return_Points_Won_Per)
    matchtable$Total_Points_Won_Total <- as.numeric(matchtable$Total_Points_Won_Total)
    matchtable$Total_Points_Won_Per <- as.numeric(matchtable$Total_Points_Won_Per)
    
    matchtable$Aces_Diff <- matchtable$Aces[[1]] - matchtable$Aces[[2]]
    matchtable$Aces_Diff[[2]] <- matchtable$Aces[[2]] - matchtable$Aces[[1]]
    
    matchtable$Double_Faults_Diff <- matchtable$Double_Faults[[1]] - matchtable$Double_Faults[[2]]
    matchtable$Double_Faults_Diff[[2]] <- matchtable$Double_Faults[[2]] - matchtable$Double_Faults[[1]]
    
    matchtable$First_Serve_Per_Diff <- matchtable$First_Serve_Per[[1]] - matchtable$First_Serve_Per[[2]]
    matchtable$First_Serve_Per_Diff[[2]] <- matchtable$First_Serve_Per[[2]] - matchtable$First_Serve_Per[[1]]
    
    matchtable$First_Serve_Points_Won_Per_Diff <- matchtable$First_Serve_Points_Won_Per[[1]] - matchtable$First_Serve_Points_Won_Per[[2]]
    matchtable$First_Serve_Points_Won_Per_Diff[[2]] <- matchtable$First_Serve_Points_Won_Per[[2]] - matchtable$First_Serve_Points_Won_Per[[1]]
    
    matchtable$Second_Serve_Points_Won_Per_Diff <- matchtable$Second_Serve_Points_Won_Per[[1]] - matchtable$Second_Serve_Points_Won_Per[[2]]
    matchtable$Second_Serve_Points_Won_Per_Diff[[2]] <- matchtable$Second_Serve_Points_Won_Per[[2]] - matchtable$Second_Serve_Points_Won_Per[[1]]
    
    matchtable$Break_Points_Saved_Per_Diff <- matchtable$Break_Points_Saved_Per[[1]] - matchtable$Break_Points_Saved_Per[[2]]
    matchtable$Break_Points_Saved_Per_Diff[[2]] <- matchtable$Break_Points_Saved_Per[[2]] - matchtable$Break_Points_Saved_Per[[1]]
    
    matchtable$First_Serve_Return_Points_Won_Per_Diff <- matchtable$First_Serve_Return_Points_Won_Per[[1]] - matchtable$First_Serve_Return_Points_Won_Per[[2]]
    matchtable$First_Serve_Return_Points_Won_Per_Diff[[2]] <- matchtable$First_Serve_Return_Points_Won_Per[[2]] - matchtable$First_Serve_Return_Points_Won_Per[[1]]
    
    matchtable$Second_Serve_Return_Points_Won_Per_Diff <- matchtable$Second_Serve_Return_Points_Won_Per[[1]] - matchtable$Second_Serve_Return_Points_Won_Per[[2]]
    matchtable$Second_Serve_Return_Points_Won_Per_Diff[[2]] <- matchtable$Second_Serve_Return_Points_Won_Per[[2]] - matchtable$Second_Serve_Return_Points_Won_Per[[1]]
    
    matchtable$Break_Points_Converted_Per_Diff <- matchtable$Break_Points_Converted_Per[[1]] - matchtable$Break_Points_Converted_Per[[2]]
    matchtable$Break_Points_Converted_Per_Diff[[2]] <- matchtable$Break_Points_Converted_Per[[2]] - matchtable$Break_Points_Converted_Per[[1]]
    
    matchtable$Total_Service_Points_Won_Per_Diff <- matchtable$Total_Service_Points_Won_Per[[1]] - matchtable$Total_Service_Points_Won_Per[[2]] 
    matchtable$Total_Service_Points_Won_Per_Diff[[2]] <- matchtable$Total_Service_Points_Won_Per[[2]] - matchtable$Total_Service_Points_Won_Per[[1]]
    
    matchtable$Total_Return_Points_Won_Per_Diff <- matchtable$Total_Return_Points_Won_Per[[1]] - matchtable$Total_Return_Points_Won_Per[[2]] 
    matchtable$Total_Return_Points_Won_Per_Diff[[2]] <- matchtable$Total_Return_Points_Won_Per[[2]] - matchtable$Total_Return_Points_Won_Per[[1]]
    
    matchtable$Total_Points_Won_Per_Diff <- matchtable$Total_Points_Won_Per[[1]] - matchtable$Total_Points_Won_Per[[2]] 
    matchtable$Total_Points_Won_Per_Diff[[2]] <- matchtable$Total_Points_Won_Per[[2]] - matchtable$Total_Points_Won_Per[[1]]
    
    matchtable <- matchtable[-c(13,14,15,16,18,19,20,22,23,24)]
    
    matchtable <- matchtable[c(1,2,3,4,5,6,7,8,9,10,11,35,12,36,15,16,37,17,18,38,19,20,39,21,22,40,13,
                               23,24,41,25,26,42,27,28,43,14,29,30,44,31,32,45,33,34,46)]
    
    if(matchtable$Round[[1]] == "R128"){
      matchtable$Round <- 128
    }
    
    if(matchtable$Round[[1]] == "R64"){
      matchtable$Round <- 64
    }
    
    if(matchtable$Round[[1]] == "R32"){
      matchtable$Round <- 32
    }
    
    if(matchtable$Round[[1]] == "R16"){
      matchtable$Round <- 16
    }
    
    if(matchtable$Round[[1]] == "Q"){
      matchtable$Round <- 8
    }
    
    if(matchtable$Round[[1]] == "S"){
      matchtable$Round <- 4
    }
    
    if(matchtable$Round[[1]] == "F"){
      matchtable$Round <- 2
    }
    
    if(matchtable$Player[[2]] == "N/A Bye"){
      matchtable$Player[[2]] <- NA
      matchtable$Nationality_1[[2]] <- NA
      matchtable$Opponent[[1]] <- NA
      matchtable$Nationality_2[[1]] <- NA
      matchtable$Aces <- NA
      matchtable$Aces_Diff <- NA
      matchtable$Double_Faults <- NA
      matchtable$Double_Faults_Diff <- NA
      matchtable$First_Serve_Total <- NA
      matchtable$First_Serve_Per <- NA
      matchtable$First_Serve_Per_Diff <- NA
      matchtable$First_Serve_Points_Won_Total <- NA
      matchtable$First_Serve_Points_Won_Per <- NA
      matchtable$First_Serve_Points_Won_Per_Diff <- NA
      matchtable$Second_Serve_Points_Won_Total <- NA
      matchtable$Second_Serve_Points_Won_Per <- NA
      matchtable$Second_Serve_Points_Won_Per_Diff <- NA
      matchtable$Break_Points_Saved_Total <- NA
      matchtable$Break_Points_Saved_Per <- NA
      matchtable$Break_Points_Saved_Per_Diff <- NA
      matchtable$Service_Games_Played <- NA
      matchtable$First_Serve_Return_Points_Won_Total <- NA
      matchtable$First_Serve_Return_Points_Won_Per <- NA
      matchtable$First_Serve_Return_Points_Won_Per_Diff <- NA
      matchtable$Second_Serve_Return_Points_Won_Total <- NA
      matchtable$Second_Serve_Return_Points_Won_Per <- NA
      matchtable$Second_Serve_Return_Points_Won_Per_Diff <- NA
      matchtable$Break_Points_Converted_Total <- NA
      matchtable$Break_Points_Converted_Per <- NA
      matchtable$Break_Points_Converted_Per_Diff <- NA
      matchtable$Return_Games_Played <- NA
      matchtable$Total_Service_Points_Won_Total <- NA
      matchtable$Total_Service_Points_Won_Per <- NA
      matchtable$Total_Service_Points_Won_Per_Diff <- NA
      matchtable$Total_Return_Points_Won_Total <- NA
      matchtable$Total_Return_Points_Won_Per <- NA
      matchtable$Total_Return_Points_Won_Per_Diff <- NA
      matchtable$Total_Points_Won_Total <- NA
      matchtable$Total_Points_Won_Per <- NA
      matchtable$Total_Points_Won_Per_Diff <- NA
    }
    
    if((matchtable$Tournament[[1]]=="US Open")|(matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Shanghai")|
         (matchtable$Tournament[[1]]=="ATP Masters Series Canada")|(matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Canada")|
         (matchtable$Tournament[[1]]=="ATP Masters Series Cincinnati")|(matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Cincinnati")|
         (matchtable$Tournament[[1]]=="ATP Masters Series Miami")|(matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Miami")|
         (matchtable$Tournament[[1]]=="ATP Masters Series Indian Wells")|(matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Indian Wells")|
         (matchtable$Tournament[[1]]=="ATP Masters Series Stuttgart")|(matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Paris")|
         (matchtable$Tournament[[1]]=="ATP Masters Series Madrid")){
      matchtable$Surface <- "Hard"
    }
    
    if((matchtable$Tournament[[1]]=="ATP Masters Series Paris")&(matchtable$Year[[1]]>=2007)){
      matchtable$Surface <- "Hard"
    }
    
    if((matchtable$Tournament[[1]]=="Wimbledon")){
      matchtable$Surface <- "Grass"
    }
    
    if((matchtable$Tournament[[1]]=="Australian Open")&(matchtable$Year[[1]]<=2007)){
      matchtable$Surface <- "Rebound Ace"
    }
    
    if((matchtable$Tournament[[1]]=="Australian Open")&(matchtable$Year[[1]]>=2008)){
      matchtable$Surface <- "Plexicushion"
    }
    
    if((matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Madrid")|(matchtable$Tournament[[1]]=="ATP Masters Series Monte Carlo")|
         (matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Monte Carlo")|(matchtable$Tournament[[1]]=="ATP Masters Series Rome")|
         (matchtable$Tournament[[1]]=="ATP World Tour Masters 1000 Rome")|(matchtable$Tournament[[1]]=="ATP Masters Series Hamburg")|
         (matchtable$Tournament[[1]]=="Roland Garros")){
      matchtable$Surface <- "Clay"
    }
    
    if((matchtable$Tournament[[1]]=="ATP Masters Series Paris")&(matchtable$Year[[1]]<=2006)){
      matchtable$Surface <- "Carpet"
    }
    
    matchtable <- matchtable[c(1,2,47,3,4:46)]
    
    if(i == 1){
      fulltable <- matchtable
    } else {
      fulltable = rbind(fulltable, matchtable)
    }
  }
  if(j == 1){
    stats.order = fulltable
  } else {
    stats.order = rbind(stats.order, fulltable)
  }
}

row.has.na <- apply(stats.order, 1, function(x){any(is.na(x))})
sum(row.has.na)
row.final.filtered <- stats.order[row.has.na,]

#Edit time outside the loop#
stats.order$Time <- as.character(stats.order$Time)
stats.order$Time <- as.numeric(substr(stats.order$Time, 1, nchar(stats.order$Time)-8))

#Change player to character
stats.order$Player <- as.character(stats.order$Player)
stats.order$Opponent <- as.character(stats.order$Opponent)

#Write to csv#
write.csv(stats.order, "Stats.csv", row.names=FALSE)