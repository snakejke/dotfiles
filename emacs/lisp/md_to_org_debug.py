import re
import sys
import logging

# Set up logging
logging.basicConfig(filename='/tmp/md_to_org.log', level=logging.DEBUG)

def convert_markdown_to_org(input_text):
    logging.debug(f"Received input text: {repr(input_text)}")
    output_lines = []
    inside_code_block = False
    for line in input_text.splitlines():
        stripped_line = line.lstrip()
        logging.debug(f"Processing line: {repr(line)}")

        # Блоки кода: начало
        if stripped_line.startswith("```"):
            if inside_code_block:
                output_lines.append("#+END_SRC")  # Закрываем блок кода
                inside_code_block = False
            else:
                lang = stripped_line[3:].strip() or "text"  # Получаем язык, если указан
                output_lines.append(f"#+BEGIN_SRC {lang}")
                inside_code_block = True
            continue

        # Если внутри блока кода, добавляем строку как есть
        if inside_code_block:
            output_lines.append(line)
            continue

        # Обработка горизонтальных линий (добавляем до обработки заголовков)
        if re.match(r'^\s*-{3,}\s*$', line):
            output_lines.append('-----')
            continue

        # Обработка заголовков
        match_heading = re.match(r"^(#+)\s*(\d+\.\s*)?(.*)$", line)
        if match_heading:
            _, number, content = match_heading.groups()
            number = number if number else ""  # Если номер отсутствует, оставляем его пустым

            # Если заголовок содержит жирный шрифт (**текст**), преобразуем в *текст*
            if "**" in content:
                content = re.sub(r"\*\*(.+?)\*\*", r"*\1*", content)
                output_lines.append(f"{number}{content}".strip())
            else:
                # Если жирного шрифта нет, делаем текст подчеркиванием
                output_lines.append(f"_{number}{content}._".strip())
            continue

        # Инлайн-код: оборачиваем `код` в ~код~
        line = re.sub(r"`(.+?)`", r"~\1~", line)

        # Маркированные списки: заменяем - или * на -
        line = re.sub(r"^(\s*)[-*]\s", r"\1- ", line)

        # Цитаты: заменяем > на #+
        line = re.sub(r"^>\s", "#+ ", line)

        # Таблицы: добавляем поддержку простых Markdown-таблиц
        if "|" in line and not line.strip().startswith("|-"):
            line = line.replace("|", " | ").strip()
            if not line.startswith("|"):
                line = f"| {line} |"

        # Жирный текст: преобразуем **текст** в *текст*
        line = re.sub(r"\*\*(.+?)\*\*", r"*\1*", line)

        # Если ничего не подходит, добавляем строку как есть
        output_lines.append(line)

    return "\n".join(output_lines)

if __name__ == "__main__":
    try:
        input_text = sys.stdin.read()
        logging.debug(f"Read from stdin: {repr(input_text)}")
        print(convert_markdown_to_org(input_text))
    except Exception as e:
        logging.error(f"Error occurred: {str(e)}")
        raise
