#!/bin/bash

# Переменные для MariaDB
MARIADB_USER="root"
MARIADB_PASS="machaon321"
MARIADB_BACKUP_DIR="/home/snake/sqlbackup"

# Переменные для PostgreSQL
POSTGRES_USER="postgres"
POSTGRES_PASS="root"
POSTGRES_BACKUP_DIR="/home/snake/sqlbackup"

# Создаем директории для бэкапов, если их не существует
mkdir -p "$MARIADB_BACKUP_DIR" "$POSTGRES_BACKUP_DIR"

# Функция для экспорта баз данных MariaDB
export_mariadb_dbs() {
    databases=$(mysql -u "$MARIADB_USER" -p"$MARIADB_PASS" -e 'SHOW DATABASES;' -s --skip-column-names)
    for db in $databases; do
        if [[ "$db" != "information_schema" ]] && [[ "$db" != "performance_schema" ]] && [[ "$db" != "mysql" ]] && [[ "$db" != _* ]]; then
            echo "Экспортирую базу данных MariaDB: $db"
            mysqldump -u "$MARIADB_USER" -p"$MARIADB_PASS" "$db" > "$MARIADB_BACKUP_DIR/$db.sql"
        fi
    done
}

# Функция для экспорта баз данных PostgreSQL
export_postgres_dbs() {
    databases=$(psql -U "$POSTGRES_USER" -c '\list' | awk 'NR!=1 && NR!=2 { print $1 }' | sed '/^$/d')
    for db in $databases; do
        if [[ "$db" != "postgres" ]] && [[ "$db" != "template0" ]] && [[ "$db" != "template1" ]]; then
            echo "Экспортирую базу данных PostgreSQL: $db"
            pg_dump -U "$POSTGRES_USER" "$db" > "$POSTGRES_BACKUP_DIR/$db.sql"
        fi
    done
}

# Вызываем функции для экспорта
export_mariadb_dbs
export_postgres_dbs

echo "Экспорт баз данных завершен."
