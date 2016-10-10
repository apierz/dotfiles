#! /usr/local/bin/python3

import json
import urllib.request
import socket

def check_connectivity(host="8.8.8.8", port=53, timeout=3):
    """
    Host: 8.8.8.8 (google-public-dns-a.google.com)
    OpenPort: 53/tcp
    Service: domain (DNS/TCP)
    """
    try:
      socket.setdefaulttimeout(timeout)
      socket.socket(socket.AF_INET, socket.SOCK_STREAM).connect((host, port))
      return True
    except Exception as ex:
      return False

def main():
  if check_connectivity() == True:
    loc_json = "http://ip-api.com/json"
    loc_result = urllib.request.urlopen(loc_json).read()
    loc_data = json.loads(loc_result.decode())
    latitude = str(loc_data['lat'])
    longitude = str(loc_data['lon'])
    city = loc_data['city']
    region = loc_data['region']
    # print(city + region)

    baseurl = "https://query.yahooapis.com/v1/public/yql?"
    yql_query = 'SELECT item.condition FROM weather.forecast where woeid in (SELECT woeid FROM geo.places(1) WHERE text="'+ city + ', ' + region + '")'
    yql_url = baseurl + urllib.parse.urlencode({'q':yql_query}) + "&format=json"
    result = urllib.request.urlopen(yql_url).read()
    data = json.loads(result.decode())
    print(data['query']['results']['channel']['item']['condition']['temp'] + '@' + data['query']['results']['channel']['item']['condition']['code'])
  else:
    print("--@99")

    
main()

