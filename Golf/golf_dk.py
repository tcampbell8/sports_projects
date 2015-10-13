import csv
import unicodedata

f = open("deutche_champ_scramble.csv", 'rt')
writer = csv.writer(open("deutche_scramble_mod.csv", 'w'))
try:
    reader = csv.reader(f)
    for row in reader:
        row[0] = str(row[0]).replace("\xca", " ")
        writer.writerow(row)
finally:
    f.close()









