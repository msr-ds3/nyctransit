#! /usr/bin/env python3
import xml.etree.ElementTree as et
import requests


wiki_root_url = 'https://en.wikipedia.org'
root_url = "https://en.wikipedia.org/wiki/Accessibility_of_the_Metropolitan_Transportation_Authority"

root = et.fromstring(requests.get(root_url).text)

#number of accessible stations per borough (using this for sanity check)
stop_count = (39, 12, 22, 20,5)
tables = root.findall('.//table')[1:6]
all_station_urls = []
for i in range(len(tables)):
	table = tables[i]
	station_urls = [wiki_root_url + url_node.attrib['href'] for url_node in table.findall('.//th[1]/a[@href][@title]')]
	all_station_urls.extend(station_urls)
	assert(len(station_urls) == stop_count[i])

station_codes = ["Complex ID"]
for station_url in all_station_urls:
	station_root = et.fromstring(requests.get(station_url).text)
	code = station_root.find(".//tr[th='Station code']/td").text.strip()
	station_codes.append(code)
with open('accessible_station_ids.csv','w') as file:
	file.write('\n'.join(station_codes))