#!/bin/bash

# Переменные для MariaDB
MARIADB_USER="root"
MARIADB_PASS="root"
MARIADB_BACKUP_DIR="/home/snake/Documents/sqlbackup"

# Переменные для PostgreSQL
POSTGRES_USER="snake"
POSTGRES_PASS="root"
POSTGRES_BACKUP_DIR="/home/snake/Documents/sqlbackup/postgres"
POSTGRES_DUMP_FILE="$POSTGRES_BACKUP_DIR/dump.sql"

# Функция для импорта баз данных MariaDB
import_mariadb_dbs() {
    for file in "$MARIADB_BACKUP_DIR"/*.sql; do
        if [ -f "$file" ]; then
            db=$(basename "$file" .sql)
            echo "Импортирую базу данных MariaDB: $db"
            mysql -u "$MARIADB_USER" -p"$MARIADB_PASS" -e "DROP DATABASE IF EXISTS $db; CREATE DATABASE $db;"
            mysql -u "$MARIADB_USER" -p"$MARIADB_PASS" "$db" < "$file"
        fi
    done
}

# Функция для импорта баз данных PostgreSQL
import_postgres_dbs() {
    if [ -f "$POSTGRES_DUMP_FILE" ]; then
        echo "Импортирую все базы данных PostgreSQL из файла $POSTGRES_DUMP_FILE"
        psql -U "$POSTGRES_USER" -f "$POSTGRES_DUMP_FILE"
    else
        echo "Файл $POSTGRES_DUMP_FILE не найден."
    fi
}

# Проверка флагов и вызов соответствующих функций
if [ "$1" == "-a" ] || [ "$1" == "--all" ]; then
    import_mariadb_dbs
    import_postgres_dbs
elif [ "$1" == "-m" ] || [ "$1" == "--mariadb" ]; then
    import_mariadb_dbs
elif [ "$1" == "-p" ] || [ "$1" == "--postgres" ]; then
    import_postgres_dbs
else
    echo "Используйте один из следующих флагов:"
    echo "-a или --all для импорта всех баз данных"
    echo "-m или --mariadb для импорта только баз данных MariaDB"
    echo "-p или --postgres для импорта только баз данных PostgreSQL"
fi
