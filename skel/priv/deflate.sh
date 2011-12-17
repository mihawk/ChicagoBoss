#!/bin/sh 
#17/12/2011 chan sisowath deflate compression script
ERR=./priv/static_deflate.err
LOG=./priv/static_deflate.log

rm -fr ./priv/static_deflate
cp -r ./priv/static ./priv/static_deflate
find ./priv/static_deflate -type f | grep -v ".js$" | grep -v ".css$" | grep -v ".html$" | grep -v ".xml$" | xargs rm -f 
rm -f ${ERR} 
rm -f ${LOG} 
for file in $(find ./priv/static_deflate -type f | awk '{print "\""$0"\""}')
do
  s=$(echo ${file}|wc -l)
  i=$(echo ${file}|sed 's/^"\(.*\)"/\1/')
  if [ -f "$i" ]; then
  	a=$(stat -c %s "${i}") 
        if [ ${a} -eq 0 ]; then
	 	echo "${a} ; ${file} ;Zero file" >> ${ERR}
	else
  		deflate -s -c 9 < "${i}" > "${i}".deflate
  		b=$(stat -c %s "${i}".deflate)
		if [ ${a} -gt ${b} ]; then
  			mv "${i}".deflate "${i}"
			echo -n .
  			echo "${a} ; ${b} ; ${i}" >> ${LOG}
		else
                  	echo -n "bad size compression ${a} -> ${b} :" >> ${ERR}
		  	rm -fv "${i}".deflate >> ${ERR}
		fi
	fi
  else
   echo -n "Error Fichier not found :" >> ${ERR}
   echo ${file} >> ${ERR}
  fi 
done 

cat ${LOG} | awk -F';' '{printf "%10d ; %10d ; %10.2f ;%s\n", $1,$2,($1-$2)*100/$1,$3}' > "${LOG}".tmp
cat "${LOG}".tmp > ${LOG}
echo "------------------ Stat Compression deflate -----------------" >> ${LOG} 
awk -F';' 'BEGIN{min=0;max=0}{if($3<=min){min=$3};if($3>=max){max=$3};sum=sum+$3} END {printf "\t%d Files\n\taverage\t:%3.2f \t\%\n\tmin\t:%3.2f \t\%\n\tmax\t:%3.2f \t\%\n\n",NR,sum/NR,min,max}' "${LOG}".tmp >> ${LOG}
rm "${LOG}".tmp
cat ${LOG}
echo "log file : ${LOG}"
echo 
echo "----------------------- error log ------------------------"
echo 
echo you should check  ${ERR}
echo 

