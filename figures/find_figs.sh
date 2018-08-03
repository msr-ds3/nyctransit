#!/bin/bash

# look at every file in the directory
for f in *
do
    echo "looking for figure: $f"

    # find all Rmds in the repo
    # grep for the filename
    file=`find .. -name '*.Rmd' -exec grep -H $f {} \;`

    # print based on whether we found the file or not
    if [[ -z $file ]]
    then
	echo "COULDN'T FIND ANYTHING"
    else
	echo $file
    fi

    echo
done

# look at every figure generated from the code
echo
echo "figures saved in the code---make sure these are committed and pushed in"
find .. -name '*.Rmd' -exec grep png {} \;
