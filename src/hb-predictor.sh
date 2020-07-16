#!/bin/bash

# gender=$1
# sample_fraction=$2
# hlen=$3
# extra_id=$4
# hlen_exactly=$5

# Join parameter by a dash and convert = characters by dash as well
function myjoin () {
    # OLDIFS=$IFS
    # IFS='-'
    # echo "$*" | tr '=' '-'
    # IFS=$OLDIFS
    result=""
    for param ; do
	if [[ $param =~ ^input_file= ]] ; then
	    continue;
	fi
	if [[ -z $result ]] ; then
	    result="$param"
	else
	    result=$result-$param
	fi
    done
    echo $result | tr '=' '-'
}

id=$(myjoin "$@")

LC_NUMERIC=C

echo Id is $id

output_dir=../output
#outputbase=$(printf 'jarkko_subset_analyses-2020-07-08-gender-%s-sample-fraction-%.2f-hlen-%s'  $gender $sample_fraction $hlen)
#date=2020-07-15
#outputbase=$(printf 'results-%s-%s' $date $code)
outputbase=$(printf 'results-%s' $id)

# if [[ -n $extra_id ]] ; then
#     outputbase=$outputbase-$extra_id
# fi

echo Output base is $outputbase
cd src

#/usr/bin/time -v --output $output_dir/$outputbase.time ./new_render.R $gender $sample_fraction $hlen $outputbase.md $extra_id ${hlen_exactly} > $output_dir/$outputbase.out 2> $output_dir/$outputbase.err
/usr/bin/time -v --output $output_dir/$outputbase.time ./hb-predictor-helper.R "$@" id=$id output_file=$outputbase.md > $output_dir/$outputbase.out 2> $output_dir/$outputbase.err

cd ..
prefix=$(readlink -f output)   # prefix=/home/toivoja/FRCBS/interval_prediction/output/
# Remove the prefix from image links
sed -i 's!'$prefix'/!!g' output/$outputbase.md
tar -czvf results.tar.gz data/rdata/*$id.RData data/stan_fits/*$id.RData output/results-$id*
echo Computation finished
tail -f /dev/null
