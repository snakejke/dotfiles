import re
import sys
import logging

# Set up logging
logging.basicConfig(filename='/tmp/md_to_org.log', level=logging.DEBUG)

def convert_markdown_to_org(input_text):
    logging.debug(f"Received input text: {repr(input_text)}")
    output_lines = []
    inside_code_block = False
    inside_quote_block = False
    inside_table = False
    
    lines = input_text.splitlines()
    i = 0
    
    while i < len(lines):
        line = lines[i]
        stripped_line = line.lstrip()
        logging.debug(f"Processing line: {repr(line)}")
        
        # Блоки кода: начало
        if stripped_line.startswith("```"):
            if inside_code_block:
                output_lines.append("#+END_SRC")
                inside_code_block = False
            else:
                lang = stripped_line[3:].strip() or "text"
                output_lines.append(f"#+BEGIN_SRC {lang}")
                inside_code_block = True
            i += 1
            continue
        
        # Если внутри блока кода, добавляем строку как есть
        if inside_code_block:
            output_lines.append(line)
            i += 1
            continue
        
        # Обработка цитат (многострочные блоки)
        if stripped_line.startswith(">"):
            if not inside_quote_block:
                output_lines.append("#+BEGIN_QUOTE")
                inside_quote_block = True
            # Убираем символ > и пробел после него
            quote_content = re.sub(r"^>\s?", "", line)
            output_lines.append(quote_content)
            i += 1
            continue
        elif inside_quote_block:
            # Закрываем блок цитаты, если строка не начинается с >
            output_lines.append("#+END_QUOTE")
            inside_quote_block = False
            # Продолжаем обработку текущей строки
        
        # Обработка таблиц
        if "|" in line:
            # Проверяем, это разделитель таблицы markdown (строка с дефисами)
            if re.match(r'^\s*\|[\s\-:|]+\|\s*$', line):
                # Это разделитель markdown - преобразуем в org-mode разделитель
                if not inside_table:
                    inside_table = True
                # Подсчитываем количество столбцов
                cols = line.count('|') - 1
                # Создаём разделитель org-mode
                separator_parts = []
                for j in range(cols):
                    separator_parts.append('-' * 10)
                separator = '|' + '+'.join(separator_parts) + '|'
                output_lines.append(separator)
                i += 1
                continue
            elif not re.match(r'^\s*\|-', line.strip()):
                # Это строка данных таблицы
                inside_table = True
                # Убираем markdown форматирование из ячеек
                cleaned_line = re.sub(r'\*\*(.+?)\*\*', r'\1', line)
                # Убираем backticks
                cleaned_line = re.sub(r'`(.+?)`', r'\1', cleaned_line)
                # Разбиваем на ячейки
                cells = cleaned_line.split('|')
                # Удаляем пустые элементы по краям и очищаем
                cells = [cell.strip() for cell in cells if cell.strip()]
                # Собираем обратно
                cleaned_line = '| ' + ' | '.join(cells) + ' |'
                output_lines.append(cleaned_line)
                i += 1
                continue
        else:
            if inside_table:
                inside_table = False
        
        # Обработка горизонтальных линий
        if re.match(r'^\s*-{3,}\s*$', line):
            output_lines.append('-----')
            i += 1
            continue
        
        # Обработка заголовков
        match_heading = re.match(r"^(#+)\s*(\d+\.\s*)?(.*)$", line)
        if match_heading:
            _, number, content = match_heading.groups()
            number = number if number else ""
            # Преобразуем backticks в тильды
            content = re.sub(r"`(.+?)`", r"~\1~", content)
            # Если заголовок содержит жирный шрифт (**текст**), преобразуем в *текст*
            if "**" in content:
                content = re.sub(r"\*\*(.+?)\*\*", r"*\1*", content)
                output_lines.append(f"{number}{content}".strip())
            else:
                # Используем emoji diamond вместо подчеркивания
                output_lines.append(f"🔷 {number}{content}".strip())
            i += 1
            continue
        
        # Инлайн-код: оборачиваем `код` в ~код~
        line = re.sub(r"`(.+?)`", r"~\1~", line)
        
        # Маркированные списки: заменяем - или * на -
        line = re.sub(r"^(\s*)[-*]\s", r"\1- ", line)
        
        # Жирный текст: преобразуем **текст** в *текст*
        line = re.sub(r"\*\*(.+?)\*\*", r"*\1*", line)
        
        # Если ничего не подходит, добавляем строку как есть
        output_lines.append(line)
        i += 1
    
    # Закрываем блок цитаты, если он остался открытым
    if inside_quote_block:
        output_lines.append("#+END_QUOTE")
    
    return "\n".join(output_lines)

if __name__ == "__main__":
    try:
        input_text = sys.stdin.read()
        logging.debug(f"Read from stdin: {repr(input_text)}")
        print(convert_markdown_to_org(input_text))
    except Exception as e:
        logging.error(f"Error occurred: {str(e)}")
        raise










# import re
# import sys
# import logging

# # Set up logging
# logging.basicConfig(filename='/tmp/md_to_org.log', level=logging.DEBUG)

# def convert_markdown_to_org(input_text):
#     logging.debug(f"Received input text: {repr(input_text)}")
#     output_lines = []
#     inside_code_block = False
#     inside_quote_block = False
#     inside_table = False
    
#     lines = input_text.splitlines()
#     i = 0
    
#     while i < len(lines):
#         line = lines[i]
#         stripped_line = line.lstrip()
#         logging.debug(f"Processing line: {repr(line)}")
        
#         # Блоки кода: начало
#         if stripped_line.startswith("```"):
#             if inside_code_block:
#                 output_lines.append("#+END_SRC")
#                 inside_code_block = False
#             else:
#                 lang = stripped_line[3:].strip() or "text"
#                 output_lines.append(f"#+BEGIN_SRC {lang}")
#                 inside_code_block = True
#             i += 1
#             continue
        
#         # Если внутри блока кода, добавляем строку как есть
#         if inside_code_block:
#             output_lines.append(line)
#             i += 1
#             continue
        
#         # Обработка цитат (многострочные блоки)
#         if stripped_line.startswith(">"):
#             if not inside_quote_block:
#                 output_lines.append("#+BEGIN_QUOTE")
#                 inside_quote_block = True
#             # Убираем символ > и пробел после него
#             quote_content = re.sub(r"^>\s?", "", line)
#             output_lines.append(quote_content)
#             i += 1
#             continue
#         elif inside_quote_block:
#             # Закрываем блок цитаты, если строка не начинается с >
#             output_lines.append("#+END_QUOTE")
#             inside_quote_block = False
#             # Продолжаем обработку текущей строки
        
#         # Обработка таблиц
#         if "|" in line:
#             # Проверяем, это разделитель таблицы markdown (строка с дефисами)
#             if re.match(r'^\s*\|[\s\-:|]+\|\s*$', line):
#                 # Это разделитель markdown - преобразуем в org-mode разделитель
#                 if not inside_table:
#                     inside_table = True
#                 # Подсчитываем количество столбцов
#                 cols = line.count('|') - 1
#                 # Создаём разделитель org-mode
#                 separator_parts = []
#                 for j in range(cols):
#                     separator_parts.append('-' * 10)
#                 separator = '|' + '+'.join(separator_parts) + '|'
#                 output_lines.append(separator)
#                 i += 1
#                 continue
#             elif not re.match(r'^\s*\|-', line.strip()):
#                 # Это строка данных таблицы
#                 inside_table = True
#                 # Убираем markdown форматирование из ячеек
#                 cleaned_line = re.sub(r'\*\*(.+?)\*\*', r'\1', line)
#                 # Разбиваем на ячейки
#                 cells = cleaned_line.split('|')
#                 # Удаляем пустые элементы по краям и очищаем
#                 cells = [cell.strip() for cell in cells if cell.strip()]
#                 # Собираем обратно
#                 cleaned_line = '| ' + ' | '.join(cells) + ' |'
#                 output_lines.append(cleaned_line)
#                 i += 1
#                 continue
#         else:
#             if inside_table:
#                 inside_table = False
        
#         # Обработка горизонтальных линий
#         if re.match(r'^\s*-{3,}\s*$', line):
#             output_lines.append('-----')
#             i += 1
#             continue
        
#         # Обработка заголовков
#         match_heading = re.match(r"^(#+)\s*(\d+\.\s*)?(.*)$", line)
#         if match_heading:
#             _, number, content = match_heading.groups()
#             number = number if number else ""
#             # Если заголовок содержит жирный шрифт (**текст**), преобразуем в *текст*
#             if "**" in content:
#                 content = re.sub(r"\*\*(.+?)\*\*", r"*\1*", content)
#                 output_lines.append(f"{number}{content}".strip())
#             else:
#                 # Если жирного шрифта нет, делаем текст подчеркиванием
#                 output_lines.append(f"_{number}{content}._".strip())
#             i += 1
#             continue
        
#         # Инлайн-код: оборачиваем `код` в ~код~
#         line = re.sub(r"`(.+?)`", r"~\1~", line)
        
#         # Маркированные списки: заменяем - или * на -
#         line = re.sub(r"^(\s*)[-*]\s", r"\1- ", line)
        
#         # Жирный текст: преобразуем **текст** в *текст*
#         line = re.sub(r"\*\*(.+?)\*\*", r"*\1*", line)
        
#         # Если ничего не подходит, добавляем строку как есть
#         output_lines.append(line)
#         i += 1
    
#     # Закрываем блок цитаты, если он остался открытым
#     if inside_quote_block:
#         output_lines.append("#+END_QUOTE")
    
#     return "\n".join(output_lines)

# if __name__ == "__main__":
#     try:
#         input_text = sys.stdin.read()
#         logging.debug(f"Read from stdin: {repr(input_text)}")
#         print(convert_markdown_to_org(input_text))
#     except Exception as e:
#         logging.error(f"Error occurred: {str(e)}")
#         raise

