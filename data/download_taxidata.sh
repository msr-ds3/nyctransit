for color in yellow
do
	for year in 2015
	do
    		for month in 01 02 03 04 05 06 07 08 09 10 11 12
        	do
		    url=https://s3.amazonaws.com/nyc-tlc/trip+data/${color}_tripdata_${year}-${month}.csv
	  	    curl -O $url
		done
	done
done

