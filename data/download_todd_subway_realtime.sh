#!/bin/bash

aws s3 cp --request-payer requester s3://nyc-subway-data/subway_data.csv.gz subway_data.csv.gz

gzip -d subway_data.csv.gz

mv subway_data.csv todd_subway_realtime.csv
