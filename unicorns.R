# unicorns
if (!require(waffle)) {
  install.packages("waffle", repos="http://cran.us.r-project.org")
}
library(waffle)
if (!require(data.table)) {
  install.packages("data.table", repos="http://cran.us.r-project.org")
}
library(data.table)
if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
library(wordcloud)
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
library(SnowballC)
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
library(SnowballC)
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)

workingDir = '/Users/michaeltauberg/projects/unicorns/'
csvName = "unicorns.csv"
data_name = "unicorns"
setwd(workingDir)

unicorns = read.csv(csvName)

#################################
#####  VALUATION INTRO
#################################

################
## by valuation
##############
unicorns$valuation_millions = strtoi(unicorns$valuation_millions)
unicorns$company = factor(unicorns$company, levels = unicorns$company[order(unicorns$valuation_millions, decreasing=FALSE)])
unicorns = unicorns[order(unicorns$valuation_millions, decreasing=TRUE),]
data_name = "valuations"
top_valuations = unicorns[1:20,]
p = ggplot(top_valuations, aes(x=company, y=valuation_millions, fill=country)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Global Unicorns by Valuation")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=13,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Valuation in millions of usd") 
#p = p + guides(fill=FALSE)
p = p + coord_flip() 
#p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)


#################################
#####  ANALYSIS BY COUNTRY
#################################

################
## country waffle plots
##############
countries = c()
total = nrow(unicorns)
for (country in levels(droplevels(unicorns$country))) {
  country_subset = unicorns[unicorns$country==country,]
  country_total = nrow(country_subset)
  country_total_valuation = sum(country_subset$valuation_millions)
  stats_row = c(country,country_total,total,country_total/total*100,country_total_valuation)
  countries = rbind(countries, stats_row)
}
countries = as.data.table(countries)
countries = as.data.frame(countries)
colnames(countries) = c("country","num_companies","total","percent_countries", "total_value")
countries$percent_countries=as.numeric(as.character(countries$percent_countries))

vals = as.numeric(as.character(countries$num_companies))
names(vals)  =  countries$country
country_colors = c("#89C5DA", "#f5ef42", "#74D944", "#ad2f21", "#3F4921", "#C0717C", "#0055A4", "#FFCE00", 
                            "#673770", "#138808", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                            "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
                            "#e8e9ff", "#3c3b6e")
p = waffle::waffle(vals,
               size = 1, 
               colors = country_colors,
               title = "Global Unicorns by Country")
#p = p + ggthemes::scale_fill_tableau(name=NULL)
p = p + theme(plot.title = element_text(hjust = 0.5,size = 17, face = "bold", colour = "darkred"),
              legend.text = element_text(size = 7))
ggsave(filename = sprintf("./%s_countries.png", data_name) , plot=p, width=9, height=4)

################
## plot by number of companies per country - compare to GDP
##############
data_name = "country_bar"
countries$num_companies = as.numeric(as.character(countries$num_companies))
countries = countries[order(countries$num_companies, decreasing=FALSE),]
countries$country = factor(countries$country, levels = countries$country[order(countries$num_companies, decreasing=FALSE)])
p = ggplot(countries, aes(x=country, y=num_companies)) + geom_bar(stat="identity") 
p = p + xlab("Country") + ylab("Number of Unicorns") 
p = p + ggtitle("Unicorn Companies by Country")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=7, height=5)

################
## plot by valuation of companies per country - compare to GDP
##############
data_name = "valuation_country_bar"
countries$total_value = as.numeric(as.character(countries$total_value))
countries = countries[order(countries$total_value, decreasing=FALSE),]
countries$country = factor(countries$country, levels = countries$country[order(countries$total_value, decreasing=FALSE)])
p = ggplot(countries, aes(x=country, y=total_value)) + geom_bar(stat="identity") 
p = p + xlab("Country") + ylab("Total Unicorn Valuation (millions usd)") 
p = p + ggtitle("Unicorn Companies by Country")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=7, height=5)

#################################
#####  ANALYSIS BY CITY
#################################
cities = c()
for (city in levels(droplevels(unicorns$us_city))) {
  city_subset = unicorns[unicorns$us_city==city,]
  city_total = nrow(city_subset)
  city_total_valuation = sum(city_subset$valuation_millions)
  stats_row = c(city,city_total,city_total_valuation)
  cities = rbind(cities, stats_row)
}
cities = as.data.table(cities)
cities = as.data.frame(cities)
colnames(cities) = c("city","num_companies","total_value")
cities = cities[cities$city != "",]

################
## plot by number of companies per city - compare to GDP
##############
data_name = "city_bar"
cities$num_companies = as.numeric(as.character(cities$num_companies))
cities = cities[order(cities$num_companies, decreasing=FALSE),]
cities$city = factor(cities$city, levels = cities$city[order(cities$num_companies, decreasing=FALSE)])
p = ggplot(cities, aes(x=city, y=num_companies)) + geom_bar(stat="identity") 
p = p + xlab("City") + ylab("Number of Unicorns") 
p = p + ggtitle("Unicorn Companies by US City")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=7, height=5)

################
## plot by valuation of companies per city - compare to GDP
##############
data_name = "valuation_city_bar"
cities$total_value = as.numeric(as.character(cities$total_value))
cities = cities[order(cities$total_value, decreasing=FALSE),]
cities$city = factor(cities$city, levels = cities$city[order(cities$total_value, decreasing=FALSE)])
p = ggplot(cities, aes(x=city, y=total_value)) + geom_bar(stat="identity") 
p = p + xlab("City") + ylab("Total Unicorn Valuation") 
p = p + ggtitle("Unicorn Total Valuation by US City")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=7, height=5)


################################
## ANALYSIS BY INDUSTRY
################################

################
## industry waffle plots
##############
data_name = "industry_waffle"
industries = c()
total = nrow(unicorns)
for (industry in levels(droplevels(unicorns$industry))) {
  industry_subset = unicorns[unicorns$industry==industry,]
  industry_total = nrow(industry_subset)
  stats_row = c(industry,industry_total,total,industry_total/total*100 )
  industries = rbind(industries, stats_row)
}
industries = as.data.table(industries)
industries = as.data.frame(industries)
colnames(industries) = c("industry","num_industries","total","percent_industries")
industries$percent_industries=as.numeric(as.character(industries$percent_industries))

vals = as.numeric(as.character(industries$num_industries))
names(vals)  =  industries$industry
industry_colors = c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                    "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                    "#D14285", "#6DDE88")
p = waffle::waffle(vals,
                   size = 1, 
                   colors = industry_colors,
                   title = "Global Unicorns by Industry")
p = p + theme(plot.title = element_text(hjust = 0.5,size = 17, face = "bold", colour = "darkred"),
              legend.text = element_text(size = 7))
ggsave(filename = sprintf("./%s_industries.png", data_name) , plot=p, width=9, height=5)

################
## industry bar plots
##############
data_name = "industry_bar"
industries$num_industries = as.numeric(as.character(industries$num_industries))
industries = industries[order(industries$num_industries, decreasing=FALSE),]
industries$industry = factor(industries$industry, levels = industries$industry[order(industries$num_industries, decreasing=FALSE)])
p = ggplot(industries, aes(x=industry, y=num_industries)) + geom_bar(stat="identity") 
p = p + xlab("Industry") + ylab("Number of Unicorns") 
p = p + ggtitle("Number of Unicorn by Industry")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

################################
## ANALYSIS BY COMPANY NAME
################################
usa_companies = unicorns[unicorns$country == "United States",]
usa_companies$letter = factor(substr(usa_companies$company, 1, 1))
data_name = "industry_waffle"
letters = c()
total = nrow(usa_companies)
for (letter in levels(droplevels(usa_companies$letter))) {
  letter_subset = usa_companies[usa_companies$letter==letter,]
  letter_total = nrow(letter_subset)
  stats_row = c(letter,letter_total,total,letter_total/total*100 )
  letters = rbind(letters, stats_row)
}
letters = as.data.table(letters)
letters = as.data.frame(letters)
colnames(letters) = c("letter","num_letters","total","percent_letters")
letters$percent_letters=as.numeric(as.character(letters$percent_letters))

data_name = "letters_bar"
letters$num_letters = as.numeric(as.character(letters$num_letters))
letters = letters[order(letters$num_letters, decreasing=TRUE),]
letters$letter = factor(letters$letter, levels = letters$letter[order(letters$num_letters, decreasing=TRUE)])
p = ggplot(letters, aes(x=letter, y=num_letters)) + geom_bar(stat="identity") 
p = p + xlab("Letter") + ylab("Number of Companies that Start with Letter") 
p = p + ggtitle("Number of Companies that Start with Letter")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + guides(fill=FALSE)
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

GenerateWordCloud <- function(subject_words, data_name) {
  words = Corpus(VectorSource(subject_words))
  words = tm_map(words, content_transformer(tolower))
  #words = tm_map(words, stripWhitespace)
  words = tm_map(words, tolower)
  
  # Generate wordcloud removing all stop words
  complete_stopwords = c(stopwords('english'), "company", "provides", "offers", "helps", "develops", "allows", 
                         "offering", "provider", "designs", "leading", "products", "based", "access")
  png(sprintf("%s_stopwords_wordcloud.png", data_name))
  words = tm_map(words, removeWords, complete_stopwords)
  wordcloud(words, max.words = 50, min.freq=5, random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  dev.off()
  
}
GenerateWordCloud(unicorns$description, "descriptions")

################################
## ANALYSIS BY COMPANY AGE
################################

unicorns$age = rep(2019, nrow(unicorns)) - strtoi(unicorns$year_founded)
unicorns$value_to_age = unicorns$valuation_millions / unicorns$age
unicorns$value_to_funding = unicorns$valuation_millions / unicorns$funding_millions
unicorns$year_added = strtoi(format(as.Date(unicorns$date_joined, format="%m/%d/%Y"),"%y")) + 2000
unicorns$time_to_unicorn = unicorns$year_added - strtoi(unicorns$year_founded)

usa = unicorns[unicorns$country == "United States",]
china = unicorns[unicorns$country == "China",]

data_name = "age"
p = ggplot(data=unicorns, aes(age)) + geom_histogram(alpha = .7, fill="pink", binwidth = 1)
p = p + xlab("Company Age (years)") + ylab("Number of Companies") 
p = p + ggtitle("Age of Unicorn Companies")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "usa_age"
p = ggplot(data=usa, aes(age)) + geom_histogram(alpha = .5, fill="blue", binwidth = 1)
p = p + xlab("Company Age (years)") + ylab("Number of Companies") 
p = p + ggtitle("Age of United States Unicorn Companies")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "china_age"
p = ggplot(data=china, aes(age)) + geom_histogram(alpha = .5, fill="red", binwidth = 1)
p = p + xlab("Company Age (years)") + ylab("Number of Companies") 
p = p + ggtitle("Age of China Unicorn Companies")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "time_to_unicorn"
p = ggplot(data=unicorns, aes(time_to_unicorn)) + geom_histogram(alpha = .7, fill="pink", binwidth = 1)
p = p + xlab("Number of Years to Achieve 1 Billion Dollar Valuation") + ylab("Number of Companies") 
p = p + ggtitle("Time it Took Companies to hit 1B Valuation (from Founding Date)")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "usa_time_to_unicorn"
p = ggplot(data=usa, aes(time_to_unicorn)) + geom_histogram(alpha = .5, fill="blue", binwidth = 1)
p = p + xlab("Number of Years for US Companies to Achieve 1 Billion Dollar Valuation") + ylab("Number of Companies") 
p = p + ggtitle("Time it Took Companies to hit 1B Valuation (from Founding Date)")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "china_time_to_unicorn"
p = ggplot(data=china, aes(time_to_unicorn)) + geom_histogram(alpha = .5, fill="red", binwidth = 1)
p = p + xlab("Number of Years for Chinese Companies to Achieve 1 Billion Dollar Valuation") + ylab("Number of Companies") 
p = p + ggtitle("Time it Took Companies to hit 1B Valuation (from Founding Date)")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)


####
### Value to Age
######


data_name = "value_to_age"
unicorns$value_to_age = as.numeric(as.character(unicorns$value_to_age))
unicorns = unicorns[order(unicorns$value_to_age, decreasing=TRUE),]
unicorns$company = factor(unicorns$company, levels = unicorns$company[order(unicorns$value_to_age, decreasing=FALSE)])
top_valuations = unicorns[1:20,]
p = ggplot(top_valuations, aes(x=company, y=value_to_age, fill=country)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Global Unicorns (Valuation / Age)")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=13,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Valuation (in millions of usd) divided by Age (years)") 
#p = p + guides(fill=FALSE)
p = p + coord_flip() 
#p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)


####
### Funding
######


data_name = "value_to_funding"
p = ggplot(data=unicorns, aes(value_to_funding)) + geom_histogram(alpha = .5, fill="pink", binwidth = 1)
p = p + xlab("Ratio of Valuation to Funding") + ylab("Number of Companies") 
p = p + ggtitle("Ratio of Valuation to Funding for Global Unicorns")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "value_to_age_bar"
unicorns$value_to_funding = as.numeric(as.character(unicorns$value_to_funding))
unicorns = unicorns[order(unicorns$value_to_funding, decreasing=TRUE),]
unicorns$company = factor(unicorns$company, levels = unicorns$company[order(unicorns$value_to_funding, decreasing=FALSE)])
top_valuations = unicorns[1:20,]
p = ggplot(top_valuations, aes(x=company, y=value_to_funding, fill=country)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Global Unicorns (Valuation / Funding)")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=13,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Valuation (in millions of usd) divided by Funding (in millions of usd)") 
#p = p + guides(fill=FALSE)
p = p + coord_flip() 
#p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)


data_name = "funding"
p = ggplot(data=unicorns, aes(funding_millions)) + geom_histogram(alpha = .5, fill="pink", binwidth = 100)
p = p + xlab("Funding in millions of USD") + ylab("Number of Companies") 
p = p + ggtitle("Funding for Global Unicorns")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "funding_bar"
unicorns$funding_millions = as.numeric(as.character(unicorns$funding_millions))
unicorns = unicorns[order(unicorns$funding_millions, decreasing=TRUE),]
unicorns$company = factor(unicorns$company, levels = unicorns$company[order(unicorns$funding_millions, decreasing=FALSE)])
top_valuations = unicorns[1:20,]
p = ggplot(top_valuations, aes(x=company, y=funding_millions, fill=country)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Global Unicorns by Funding")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=13,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Funding (in millions of usd)") 
#p = p + guides(fill=FALSE)
p = p + coord_flip() 
#p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "usa_funding"
p = ggplot(data=usa, aes(funding_millions)) + geom_histogram(alpha = .5, fill="blue", binwidth = 100)
p = p + xlab("Funding in millions of USD") + ylab("Number of Companies") 
p = p + ggtitle("Funding for USA Unicorns")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

data_name = "china_funding"
p = ggplot(data=china, aes(funding_millions)) + geom_histogram(alpha = .5, fill="red", binwidth = 100)
p = p + xlab("Funding in millions of USD") + ylab("Number of Companies") 
p = p + ggtitle("Funding for China Unicorns")
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)

####
### count most common investors
######
# write a python script to separate by investors - use sample from music git 
unicorn_investors = read.csv("unicorn_investors.csv")
csv_field_name = "investor" 
investors =  ddply(unicorn_investors, csv_field_name, summarise, total_valuation = sum(valuation_millions, na.rm=TRUE)  )
investors = investors[order(investors$total_valuation, decreasing=TRUE),]
investors = investors[investors$investor != "",]
investors$investor = factor(investors$investor, levels = investors$investor[order(investors$total_valuation, decreasing=FALSE)])
top_investors= investors[1:20,]
data_name = "investors"
p = ggplot(top_investors, aes(x=investor, y=total_valuation)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Unicorn Investors (by Total Valuation")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=13,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Valuation of Unicorns Funded (in millions of usd)") 
#p = p + guides(fill=FALSE)
p = p + coord_flip() 
#p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./unicorns_%s.png", data_name) , plot=p, width=9, height=5)


