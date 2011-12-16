#!/bin/sh

rm -fr ./priv/static_gzip
cp -rv ./priv/static ./priv/static_gzip
find static_gzip -type f | grep -v ".js$" | grep -v ".css$" | grep -v ".html$" | grep -v ".xml$" | xargs rm -fv 
for i in $(find ./priv/static_gzip -type f)
do
  echo -n $i ' -> '
  gzip "${i}" 
  echo $i.gz
  echo  $i.gz ' -> ' $i
  mv "${i}".gz "${i}"
done

rm -fr ./priv/static_gzip_msie
cp -rv ./priv/static ./priv/static_gzip_msie
find static_gzip_msie -type f | grep -v ".js$" | grep -v ".css$" | grep -v ".html$" | grep -v ".xml$" | xargs rm -fv 
for i in $(find ./priv/static_gzip_msie -type f)
do
  echo -n $i ' -> MSIE '
  cat ./priv/MSIE > "${i}".gz
  gzip -c "${i}" >> "${i}".gz
  echo ${i}.gz
  echo ${i}.gz ' -> ' $i
  mv "${i}".gz "${i}"
done
