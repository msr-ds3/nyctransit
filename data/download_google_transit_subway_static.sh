#!/bin/bash

wget http://web.mta.info/developers/data/nyct/subway/google_transit.zip

[ -d google_transit_subway_static ] || mkdir google_transit_subway_static
cd google_transit_subway_static

unzip ../google_transit.zip

rm ../google_transit.zip