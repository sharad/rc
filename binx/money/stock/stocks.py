# Author: Bhavik Shah
# Date Created: Feb 27th, 2009
# Description: Pulls stock quotes from yahoo finance
#  and displays them in conky

# Open up .conkyrc and add the following line
# {execi 120 /path/to/your/script.py goog xom}
# replace 120 by your desired interval in seconds
# replace the path with your path
# Change goog and xom to the stocks you want
# If Value is N/A you probably typed the quote in wrong
# If Open is N/A the stock market is probably closed

# Script is free, do whatever you want with it
# Credit to www.bhaviksblog.com is appreciated =) 

import urllib2
import sys

if sys.argv > 1: #arg must be supplied
	stocks = sys.argv #args are the stocks
	stocks.pop(0) #pop the first entry since its the filename
	print
	print '%5s %8s %9s %9s %9s' % ('Name','Value','Open','Change','Time') #print header
	print '--------------------------------------------' #fancy line

	for i in stocks: #for each stock do this
		stock=i
		
		#This is the url from yahoo...if this changes the script dies, Ill fix it and upload again if it happens.
		csv = urllib2.urlopen('http://download.finance.yahoo.com/d/quotes.csv?s='+ stock +'&f=sl1d1t1c1ohgv&e=.csv')
		data = csv.read() #grab csv 
		tokens = data.split(',') #split into tokens

		name=tokens[0].replace('"','') #get rid of quotes
		value=tokens[1] 
		time=tokens[3].replace('"','')
		change=tokens[4]
		openVal=tokens[5]

		print '%5s' % name,'%8s'% value,'%9s'% openVal,'%9s'% change,'%9s'% time #print to screen with formatting
else:
	print 'You forgot to supply your stock quote as an arg' #go back and give the script your stocks as arguments
	print 'Example: python stocks.py goog'
	


