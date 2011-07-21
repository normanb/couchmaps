---------
Overview
---------

A simple wrapper around MapServer (http://mapserver.org/) to enable

   * MapServer map files to be written in JSON and stored in CouchDB
   * MapServer to be managed and run in an Erlang OTP supervision process
   * GDAL CouchDB OGR driver to serve GeoJSON directly from CouchDB as a WFS or rendered through WMS

MapServer is an Open Source platform for publishing spatial data and interactive mapping applications to the web.

------------
Requirements
-------------

1) Couchbase trunk

https://github.com/couchbase/couchdb-manifest (I used the preview branch and set up rake / ruby using RVM)

(currently GeoCouch uses couch_file:flush which isn't in couchdb trunk yet)

GDAL OGR couchdb driver can perform client side filtering, however in ogrcouchdbtablelayer if couchdb isn't present then the error is written to stderr which causes MapServer to raise an error.

2) GDAL trunk

http://svn.osgeo.org/gdal/trunk

3) MapServer trunk with CouchDB wrapper

git@github.com:normanb/mapserver.git

browse here https://github.com/normanb/mapserver/tree/master/mapserver

Configure as follows

./configure --with-wms --with-wfs --with-wcs --with-gdal --with-ogr --with-proj --with-couchdb


4)  Configuration

Edit config/couchmaps.ini  and copy to local.d and add couchmaps ebin files to path

alternatively

mkdir $COUCH_HOME/build/lib/couchdb/plugins/couchmaps/ebin

and copy beam files there.


note: currently map files are writting to tmp, but this can be override with a mapserver section in the ini file 

e.g.

[mapserver]

tmp = /myothertmp


6) Populate database

ogr2ogr -f couchdb -lco "UPDATE_PERMISSIONS=ALL" "couchdb:http://localhost:8000" bcpos_trails.shp 

7) Create a database called mapserver 

Add sample mapserver map

 curl -X PUT --data-binary @sample.map http://localhost:8000/mapserver/_mapserv/sample.map
 
this will convert the sample.map in mapfile format to JSON.

Note you can lock down http client browsing of sample.map with a redirect in couchdb that will not affect server operation.

8) Test WFS GetCapabilities

http://host:8000/mapserver/_mapserv/sample.map?request=getcapabilities&service=wfs&version=1.0.0

10) Test WFS GetFeature

if using geocouch, then test directly first

curl "http://host:8000/bcpos_trails/_design/ogr_spatial/_spatial/spatial?bbox=-180,-90,180,90"

then 

curl "http://host:8000/mapserver/_mapserv/sample.map?SERVICE=WFS&VERSION=1.0.0&REQUEST=getfeature&TYPENAME=BoulderOpenSpace&MAXFEATURES=100"

The geojson data in CouchDB (step 6) is now returned as GML

11) Test WMS GetMap

http://host:8000/mapserver/_mapserv/sample.map?REQUEST=GetMap&SERVICE=WMS&VERSION=1.1.1&LAYERS=BoulderOpenSpace&STYLES=&FORMAT=image/tiff&SRS=EPSG:4326&WIDTH=400&HEIGHT=300&BBOX=-105.54,39.9138,-105.072,40.2608

and open up the returned geotiff to see a rendering of the geojson data in (6).




