import csv
import sys
reload(sys)  
sys.setdefaultencoding('UTF8')
import re
#import networkx as nx
#import matplotlib.pyplot as plt
import io

filename = "unicorns.csv"



csvOutFile = "unicorn_investors.csv"
outCsv = open(csvOutFile, 'wb')
fieldnames = ['company','valuation_billions','valuation_millions','date_joined','country','industry','investor','test1','year_founded','region','description','funding','test','funding_millions','us_city']

csv_writer = csv.DictWriter(outCsv, fieldnames=fieldnames)
csv_writer.writeheader()

i = 0
with io.open(filename, encoding='utf-8', errors='ignore') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
    for row in readCSV:
        investors = row[6].split(",")
        
        for investor in investors:
            investor = re.sub("\[", "", investor) 
            investor = re.sub("\]", "", investor) 
            investor = re.sub("\'", "", investor) 
            investor = investor.strip()
            print investor
            new_csv_row = {}
            new_csv_row['investor'] = investor
            new_csv_row['company'] = row[0]
            new_csv_row['valuation_billions'] = row[1]
            new_csv_row['valuation_millions'] = row[2]
            new_csv_row['date_joined'] = row[3]
            new_csv_row['country'] = row[4]
            new_csv_row['industry'] = row[5]
            new_csv_row['test1'] = row[7]
            new_csv_row['year_founded'] = row[8]
            new_csv_row['region'] = row[9]
            new_csv_row['description'] = row[10]
            new_csv_row['funding'] = row[11]
            new_csv_row['test'] = row[12]
            new_csv_row['funding_millions'] = row[13]
            new_csv_row['us_city'] = row[14]
    
            csv_writer.writerow(new_csv_row)
