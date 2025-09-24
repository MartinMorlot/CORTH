#/usr/bin/sh

csv_end=".csv"
path_DB="/home/mmorlot/dev-work/CORTH/Database/DB_files/"

declare -A DB_filename_dict
DB_filename_dict["DHMD.MDB"]="Extracted_Loire_"
DB_filename_dict["BAREMEBASE-HYDRO_TU.MDB"]="Extracted_Moselle_"
DB_filename_dict["SARRE.MDB"]="Extracted_Sarre_"
DB_filename_dict["SEMA67.MDB"]="Extracted_67_"
DB_filename_dict["SEMA68.MDB"]="Extracted_68_"

for db_file in "${!DB_filename_dict[@]}"; do

    FILENAME=${DB_filename_dict[$db_file]}
    path_DB_file="$path_DB$db_file"
    echo "db_file: $db_file, filename: ${DB_filename_dict[$db_file]}"

    

    for table in $(mdb-tables -1 $path_DB_file); do
        echo "Exporting $table..."

        filename_csv="Extracted_files/$FILENAME$table$csv_end"
        echo "$filename_csv"
        # rm "$filename_csv"
        mdb-export -D '%Y-%m-%d' "$path_DB_file" "$table" > "$filename_csv"
    done
done




