# prepend a libPath specification to each R script in a given path

LIBPATH=".libPaths('/home/galaxy/R/x86_64-pc-linux-gnu-library/3.2/')"

RFILES=`find $1 -name '*.R'`

for file in $RFILES
do
	sed -i "1s;^;$LIBPATH\n;" $file
done
