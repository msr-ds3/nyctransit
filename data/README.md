# Data

All relevant data for the project should go in this directory. To avoid committing large files to the repository, we'll store large files elsewhere and keep scripts to download the data and/or pointers to paths where the data are stored. Document each script or pointer below.

## Static Subway data from Google Transit

This is the static data about stop, routes, trips, and schedules for the NYC subway system in Google's [standard format](https://developers.google.com/transit/gtfs/). The schedule is roughly what's printed in PDF-ed schedules for trains, refreshed roughly every 4 months. More details [here](http://web.mta.info/developers/developer-data-terms.html#data).

Run `download_google_transit_subway_static.sh` to get the latest version of this data.




