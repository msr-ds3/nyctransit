#!/bin/bash

# look at every file in the directory
echo "# Figures"
# f="Commute_time_distribution_by_borough(handicap).png"
for f in *.{png,html}
do
    echo "## [$f]($f)"

    # find all Rmds in the repo
    # grep for the filename
    file=`find .. \( -name '*.R' -o -name '*.Rmd' \) -exec grep -H -n $f {} \;`

    # print based on whether we found the file or not
    if [[ -z $file ]]
    then
	echo "location not found"
    else
    echo 
    echo "| | |"
    echo "-|-"
    echo "**Location** | [`echo $file | cut -d':' -f1`](`echo $file | cut -d':' -f1`)"
    echo "**Line Number** |`echo $file | cut -d':' -f2`"
    line="`echo $file | cut -d':' -f3`"
    echo "**Line** |`echo "$file" | cut -d':' -f3`"

    if [[ "taxi.png" =~ .*\.png ]];
    then
        # echo "**Preview** | ![]($f) "
        echo "<img src=\"$f\" width=\"256\" height=256/>"
    fi
    fi

    echo
done
