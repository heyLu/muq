ff-to-csv: ff-history.csv ff-visits.csv

ff-history.csv: ff-history.sqlite3
	sqlite3 -csv ff-history.sqlite3 'select id, url, title, last_visit_date from moz_places' > ff-history.csv

ff-visits.csv: ff-history.sqlite3
	sqlite3 -csv ff-history.sqlite3 'select id, from_visit, (from_visit in (select id from moz_historyvisits)), place_id, visit_date, visit_type from moz_historyvisits' > ff-visits.csv
