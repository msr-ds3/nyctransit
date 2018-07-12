Add cleaned versions of initial work on the subway data here.

Organize things so that any data files you use are in the `data/` at the top of the repo, and reference those files with relative paths in your code.

For example:

```
stop_times <- read.csv('../../data/google_transit_subway_static/stop_times.txt')

subway_data <- read_csv("../../data/todd_subway_realtime.csv",
                        col_types = cols(route_mta_id = col_factor(levels = NULL)))
```

See the [data/](../data/) directory for more info on how we'll handle large data files.