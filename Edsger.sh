#!/bin/bash

name="stdin"
opt="no"
for i in $@; do
    case ${i} in
	-O)
	    opt="y"
	    ;;
	-f)
	    f="y"
	    ;;
	-i)
	    i="y"
	    ;;
	[^.]*\.eds)
	           IFS='.' read -a name <<< ${i}
		   name=${name[0]}
		   ;;
	*)
	    echo "Usage: ./Edsger.sh [options] file..."
	    echo "Options:"
	    echo -e '-O\t optimized'
	    echo -e '-f\t program at stdin and final code at stdout'
	    echo -e '-i\t program at stdin and intermediate code at stdout'
	    exit
    esac
done

./compiler "$@"

if [ ${opt} == "y" ] ; then
    echo "optimized"
    /usr/lib/llvm-3.5/bin/opt -O3 -S  ${name[0]}.ll -o ${name[0]}.ll
fi;
    
if [ ${opt} == "y" ]; then
/usr/lib/llvm-3.5/bin/llc -O3  ${name}.ll
else
/usr/lib/llvm-3.5/bin/llc -O0  ${name}.ll
fi

[[ $f == "y" ]] && cat ${name}.s
[[ $f == "y" ]] && cat ${name}.ll
gcc   ${name}.s -o   ${name} lib.a
