#/usr/bin/sh

FILENAME="Extracted_"
csv_end=".csv"
path_DB="/home/mmorlot/dev-work/CORTH/Database/DHMD.MDB"

for table in $(mdb-tables -1 $path_DB); do
    echo "Exporting $table..."

    filename_csv="$FILENAME$table$csv_end"
    echo "$filename_csv"
    rm "$filename_csv"
    mdb-export -D '%Y-%m-%d' "$path_DB" "$table" > "$filename_csv"
done
