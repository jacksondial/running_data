# the individual activity files are of a different format so lets use python to 
# read them in and use them.
# I am planning to only need activity-level data for activities that are in one
# of my training blocks, which will be defined in a different dataset.

#





file_path = 'data/strava/January_29_2026/activities/18312321983.fit.gz'

from fitparse import FitFile

fitfile = FitFile(file_path)

for record in fitfile.get_messages('record'):
    for data in record:
        print(data.name, data.value)
