#!/bin/bash

# look at every file in the directory
echo "# Figures"
for f in *
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
    echo "| | | |"
    echo "|-|-|-|"
    echo "|Location | [`echo $file | cut -d':' -f1`](`echo $file | cut -d':' -f1`)|"
    echo "|Line Number |`echo $file | cut -d':' -f2`|"
    echo "|Line |`echo $file | cut -d':' -f3`|"

    if [[ "taxi.png" =~ .*\.png ]];
    then
        echo "![]($f)"
    fi
    fi

    echo
done
