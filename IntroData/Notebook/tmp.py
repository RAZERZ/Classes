import csv

with open("data/final.csv") as final:
    fileReader = csv.reader(final)
    completeFile = [_ for _ in fileReader]

# print(completeFile)
header = "region,municipality,district,party,votes"
print((completeFile[1:]))

#data = cat data/final.csv
