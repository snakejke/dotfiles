#!/bin/bash

# Определяем выходной файл
output_file="packages_info.tsv"

# Инициализируем выходной файл с заголовками колонок
echo -e "Package Name\tDescription" > $output_file

# Получаем список установленных пакетов
for pkg in $(xpkg -m); do
    # Получаем описание пакета
    description=$(xbps-query -S $pkg | grep '^short_desc:' | cut -d ' ' -f 2-)
    
    # Если описание не найдено, задаем значение по умолчанию
    if [ -z "$description" ]; then
        description="No description available"
    fi
    
    # Записываем имя пакета и описание в файл
    echo -e "$pkg\t$description" >> $output_file
done

echo "Information about installed packages has been saved to $output_file"
