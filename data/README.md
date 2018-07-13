# Data

All relevant data for the project should go in this directory. To avoid committing large files to the repository, we'll store large files elsewhere and keep scripts to download the data and/or pointers to paths where the data are stored. Document each script or pointer below.

## Static subway data from Google Transit

This is the static data about stop, routes, trips, and schedules for the NYC subway system in Google's [standard format](https://developers.google.com/transit/gtfs/). The schedule is roughly what's printed in PDF-ed schedules for trains, refreshed roughly every 4 months. More details [here](http://web.mta.info/developers/developer-data-terms.html#data).

Run `download_google_transit_subway_static.sh` to get the latest version of this data.

## Todd Schneider's clean realtime subway data

This is data from Todd Schneider's post, [Using Countdown Clock Data to Understand the New York City Subway](http://toddwschneider.com/posts/nyc-subway-data-analysis/), which he generously made available on Amazon's S3, as described [here](https://github.com/toddwschneider/nyc-subway-data/tree/master/analysis#partial-data-on-amazon-s3). It contains cleaned realtime information for 5 months of subway data starting in January 2018, with a best guess of when each train left each station, and how long until the next train arrived at that station, collected at the minute level.

Run `download_todd_subway_realtime.sh` to get this data. Note that it requires an [Amazon S3 account](https://docs.aws.amazon.com/AmazonS3/latest/gsg/SigningUpforS3.html) and the [Amazon Command Line Interface](https://docs.aws.amazon.com/cli/latest/userguide/installing.html). The data is posted as a "requester pays" bucket, which means you'll get charged by Amazon every time you run the script. The charge should be small (around a few cents).

Run `load_todd_subway_realtime.R` to get a useful .RData file to keep from loading the .csv everytime.
