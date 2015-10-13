import httplib, urllib, base64, json, csv, unicodedata
from BeautifulSoup import BeautifulSoup

def save_data(tags, json):
    try:
        row = [json[t] for t in tags]
        writer.writerow(row)
    except KeyError:
        return



headers = {
    'Ocp-Apim-Subscription-Key': '9326e56846d14efcbb55aa346d98a172',
}

params = urllib.urlencode({
})

tags =[]

season = '2013REG'
week = range(1,17)
for w in week:
    try:
        conn = httplib.HTTPSConnection('api.fantasydata.net')
        conn.request("GET", "/nfl/v2/JSON/GameStatsByWeek/%s/%d" % (season, w), params, headers)
        response = conn.getresponse()
        data = response.read()
        soup = BeautifulSoup(data)
        N = len(data)
        conn.close()
    except Exception as e:
        print("[Errno {0}] {1}".format(e.errno, e.strerror))
    writer = csv.writer(open("NFL_2013/nfl_%s_week%d.csv" % (season, w), "wb"))
    json_data = json.loads(data)
    for tag in json_data[0]:
        tags.append(tag.encode("ASCII"))
    writer.writerow(tags)
    for j in json_data:
        save_data(tags, j)

