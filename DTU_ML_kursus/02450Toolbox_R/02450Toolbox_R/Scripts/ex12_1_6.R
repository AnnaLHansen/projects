# ex12_1_6
# Load data and functions defined in previous scripts:
source(file.path('Scripts', 'ex12_1_3.R') )
source(file.path('Scripts', 'ex12_1_5.R') )
# Turn into transaction list:
T <- mat2transactions(v$X, v$attributeNames)
# run apriori and print result. See documentation for more options:
rules <- apriori(T, parameter = list(supp = 0.3, conf = 0.6, target = "rules"))
summary(rules)
inspect(rules)
# For only taking a few rules or sorting
#inspect(head(rules, by = "confidence"))