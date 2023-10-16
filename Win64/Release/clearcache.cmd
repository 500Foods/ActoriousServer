@ECHO OFF
ECHO Clearing Cache

del /Q cache\days\actorious-births\*
del /Q cache\days\actorious-deaths\*
del /Q cache\days\actorious-releases\*

del /Q cache\days\wikidata-births\*
del /Q cache\days\wikidata-deaths\*
del /Q cache\days\wikidata-releases\*

del /S /Q cache\people\actorious\*
del /S /Q cache\people\tmdb\*
del /Q cache\people\top1000\*
del /Q cache\people\top5000\*