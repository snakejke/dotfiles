#!/bin/bash

# Проверка наличия входного файла
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 input_file"
  exit 1
fi

input_file="$1"

# Запрашиваем начальное время
read -p "Введите начальное время (например, 00:01:30): " start_time

# Запрашиваем конечное время
read -p "Введите конечное время (например, 00:50:30): " end_time

# Создаем имя выходного файла
filename=$(basename -- "$input_file")
extension="${filename##*.}"
filename="${filename%.*}"
output_file="${filename}_trimmed.${extension}"

# Выполнение обрезки видео
ffmpeg -i "$input_file" -ss "$start_time" -to "$end_time" -c copy "$output_file"

echo "Видео обрезано и сохранено как $output_file"

