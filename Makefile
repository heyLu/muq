ff-to-csv: ff-history.csv ff-visits.csv

ff-history.csv: ff-history.sqlite3
	sqlite3 -csv ff-history.sqlite3 'select url, title, last_visited_date from moz_places'

ff-visits.csv: ff-history.sqlite3
	sqlite3 -csv ff-history.sqlite3 'select * from moz_historyvisits'
